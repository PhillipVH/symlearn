(ns symlearn.coastal
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.walk :as walk]
            [clojure.set :as set]
            [taoensso.carmine :as car]
            [taoensso.tufte :as tufte]
            [symlearn.intervals :as intervals]
            [symlearn.z3 :as z3]
            [cljstache.core :refer [render render-resource]])
  (:import [java.io File FileReader]
           [java.nio.file Paths Path]
           [java.net URI]
           [java.util LinkedList Collection Iterator]
           [automata.sfa SFA SFAInputMove]
           [com.google.common.collect ImmutableList]
           [org.apache.commons.lang3.tuple ImmutablePair]
           [theory.characters CharPred]
           [RegexParser RegexParserProvider]
           [benchmark.regexconverter RegexConverter]))

(set! *warn-on-reflection* true)

(defonce ^Process coastal-instance nil)
(defonce current-parser nil)

(def string-config "Regex.xml")
(def default-parser "")

(defmacro wcar*
  "Wraps Redis commands in a `car/wcar`."
  [& body]
  `(let [redis-conn# {:pool {} :spec {:host (or (System/getenv "REDIS_HOST") "localhost") :port 6379}}]
     (car/wcar redis-conn# ~@body)))

(defn- refine-string
  "Returns [boolean list] of accepted? and path conditions"
  [string]
  ;; enqueue the string for solving
  (let [exploded-string (map int (.toCharArray ^String string))
        strlen (count string)] ;; avoid "first byte is null" encoding issues
    (wcar* (car/del :refine)
           (car/del :refined)
           (apply (partial car/rpush :refine) (if (= 0 strlen) ["epsilon"] exploded-string))))

  ;; wait for a solved response
  (while (not= 1 (wcar* (car/exists :refined))))

  ;; process reponse
  (let [refined-path (tufte/p ::refine-path (wcar* (car/get :refined)))
        [accepted path-condition] (str/split refined-path #"\n")]
    (wcar* (car/del :refined))
    [(read-string accepted) path-condition]))

(defn path->constraints
  "Return a seq of constraints extracted from `path-condition`, each of
  the form [idx op bound], where idx is the index into the array, op is
  the operation of the assertion (EQ, NEQ, GE, etc.), and bound is the
  argument given to the assertion."
  [path-condition]
  (let [constraints (re-seq #"A\$(\d+)(<=|==|>=|!=|>|<)(\d+)" path-condition)]
    (->> constraints
         (map #(vec (drop 1 %))) ;; drop the full match
         (sort-by first)
         (set))))

(defn suffixes*
  "Return the suffixes of the path condition pc"
  [constraint-sets]
  (let [suffixes (map #(drop % constraint-sets)
                      (range 1 (count constraint-sets)))]
    (set suffixes)))

(defn prefixes*
  "Return the suffixes of the path condition pc"
  [constraint-sets]
  (let [prefixes (map #(take % constraint-sets)
                      (range 1 (count constraint-sets)))]
    (set prefixes)))

(assert (= #{[#{:a} #{:b}] [#{:a}]} (prefixes* [#{:a} #{:b} #{:c}])))
(assert (= #{[#{:b} #{:c}] [#{:c}]} (suffixes* [#{:a} #{:b} #{:c}])))

(defprotocol IPathCondition
  (accepted? [this] "Return true if `this` represents a successful parse.")
  (length [this] "Return the number of indices bounded by `this`.")
  (constraints [this] "Returns the bounds on each index bounded by `this`.")
  (witness [this] "Return a string that satisfies the constraints in `this`.")
  (suffixes [this] "Return the path conditions that are suffixes of `this`. Exludes `this` and epsilon.")
  (prefixes [this] "Return the path conditions that are prefixes of `this`. Exludes `this` and epsilon."))

(defn compile-parsers!
  "Compile the parsers installed in the Coastal system."
  []
  (let [coastal-dir (File. "coastal")
        args (into-array ["./gradlew" "compileJava" "--no-daemon"])
        builder (ProcessBuilder. ^"[Ljava.lang.String;" args)]
    (.directory builder coastal-dir)
    (let [compiler (.start builder)]
      (while (.isAlive compiler))
      ::ok)))


(defn compile-equivalence-oracle!
  []
  (sh/with-sh-dir "eqv-coastal"
    ;; compile coastal
    (println (:out (sh/sh "./gradlew" "build" "installDist" "-x" "test" "--no-daemon")))

    ;; install coastal runner
    (println (:out (sh/sh "cp" "-r" "build/install/coastal/" "build/classes/java/examples/")))))

(defn- mk-input
  [n]
  (str/join \, (take n (repeatedly (constantly " '.' ")))))

(defn sfa->java
  "Return the Java source code that represents a parser accepting the language
  described by `sfa`."
  [^SFA sfa fn-name]
  (let [java-src (StringBuilder.)]
    (doto java-src
      (.append (str "\tpublic static boolean " fn-name " (char[] A) {\n"))
      (.append "\t\tint state = ")
      (.append (.getInitialState sfa))
      (.append ";\n")
      (.append "\t\tfor (int idx = 0; idx < A.length; idx++) {\n")
      (.append "\t\t\tchar current = A[idx];\n"))

    (let [states-iter (.iterator (.getStates sfa))]
      (while (.hasNext states-iter)
        (let [state (.next states-iter)]
          (doto java-src
            (.append "\t\t\tif (state == ")
            (.append state)
            (.append ") {\n"))

          (let [transitions-iter ^Iterator (.iterator ^Collection (.getTransitionsFrom ^SFA sfa ^Integer state))]
            (while (.hasNext transitions-iter)
              (let [transition ^SFAInputMove (.next transitions-iter)
                    zi-guard ^CharPred (.guard transition)
                    zi-intervals (.intervals zi-guard)
                    interval-size (.size zi-intervals)
                    ;; interval-size (.. transition ^CharPred guard ^ImmutableList intervals size)
                    ]
                (.append java-src "\t\t\t\tif (")
                (doseq [id (range 0 interval-size)]
                  (let [
                        zi-guard ^CharPred (.guard transition)
                        zi-intervals (.intervals zi-guard)
                        ;; bound (.. transition guard intervals (get id))
                        bound (.get zi-intervals id)
                        left (.getLeft ^ImmutablePair bound)
                        right (.getRight ^ImmutablePair bound)]
                    (when (> id 0)
                      (.append java-src " || "))

                    (if (not (.equals left right))
                      (doto java-src
                        (.append "(current >= ")
                        (.append (if (nil? left)
                                   "Character.MIN_VALUE"
                                   (str "(char)" (int (.charValue ^Character left)))))
                        (.append " && current <= ")
                        (.append (if (nil? right)
                                   "Character.MAX_VALUE"
                                   (str "(char)" (int (.charValue ^Character right)))))
                        (.append ")"))

                      (doto java-src
                        (.append "(current == ")
                        (.append (if (nil? left)
                                   "Character.MIN_VALUE"
                                   (str "(char)" (int (.charValue ^Character left)))))
                        (.append ")")))))
                (doto java-src
                  (.append ") {\n")
                  (.append "\t\t\t\t\tstate = ")
                  (.append (.to transition))

                  (.append ";\n")
                  (.append "\t\t\t\t\tcontinue;\n")
                  (.append "\t\t\t\t}\n"))))))
        (.append java-src "\t\t\t}\n"))
      (doto java-src
        (.append "\t\t}\n")
        (.append "\t\tif ("))

      (let [states-iter (.iterator (.getFinalStates sfa))]
        (while (.hasNext states-iter)
          (let [final-state (.next states-iter)]
            (doto java-src
              (.append "(state == ")
              (.append final-state)
              (.append ") || ")))))
      (doto java-src
        (.append "false) { \n")
        (.append "\t\t\treturn true;\n")
        (.append "\t\t} else {\n")
        (.append "\t\t\treturn false;\n\t\t}\n")
        (.append "\t}")))

    (.toString java-src)))


(defn mk-equivalence-oracle
  [^SFA candidate-sfa target depth]
  (render-resource "templates/Example.java" {:target-fn (sfa->java (intervals/regex->sfa target) "target")
                                             :candidate-fn (sfa->java candidate-sfa "candidate")
                                             :input (mk-input depth)}))

(defn install-equivalence-oracle!
  [^SFA candidate target depth]
  (spit "eqv-coastal/src/examples/java/learning/Example.java"
        (mk-equivalence-oracle candidate target depth))
  (compile-equivalence-oracle!))

(defn check-equivalence!
  [{:keys [depth target ^SFA candidate]}]
  (install-equivalence-oracle! candidate target depth)
  (println "Starting equivalence check: depth " depth)
  (let [coastal-log (:out (sh/sh "./coastal/bin/coastal" "learning/Example.properties" :dir "eqv-coastal/build/classes/java/examples"))
        ce (re-seq #"<<Counter Example: \[(.*)\]>>" coastal-log)]
    (println "Finished equivalence check...")

    (when ce
      (let [counter-example ((comp second first) ce)
            ce-string (apply str (map str/trim (str/split counter-example #",")))]
        (println "Found Counter Example: " ce-string)
        ce-string))))

(defn stop!
  "Stop a Coastal process running in `coastal`."
  []
  (let [coastal-pid (str/trim (:out (sh/sh "pgrep" "-f" "COASTAL")))]
    (sh/sh "kill" "-9" coastal-pid))
  (wcar* (car/flushall))
  (if coastal-instance
    (do
      (.destroyForcibly coastal-instance)
      (while (.isAlive coastal-instance))
      (alter-var-root #'coastal-instance (constantly nil)))
    (println "No coastal to stop"))
  ::ok)



(defn ^Process start!
  "Launch a Coastal process with a config file called `filename` as an argument."
  []
  (if coastal-instance
    (stop!))
  (tufte/p
   ::start-coastal
   (let [path-in-docker (System/getenv "MEMBERSHIP_CONFIG_PATH")
         args-in (or path-in-docker (str (System/getProperty "user.dir") "/resources/Regex.xml"))
         _ (println args-in)
         args (into-array ["./gradlew" "run" (str "--args=" args-in) "--no-daemon"])
         builder (ProcessBuilder. ^"[Ljava.lang.String;" args)
         coastal-dir (File. "coastal")]
     (.directory builder coastal-dir)
     (let [new-coastal-instance (.start builder)]
       (alter-var-root #'coastal-instance (constantly new-coastal-instance))
       ::ok))))

(defn install-parser!
  [regex]
  (if coastal-instance
    (stop!))
  (alter-var-root #'current-parser (constantly regex))
  (let [parser-src (intervals/sfa->java (intervals/regex->sfa regex) "examples.tacas2017" "Regex")]
    (spit "coastal/src/main/java/examples/tacas2017/Regex.java" parser-src)
    (compile-parsers!)
    (start!)
    (wcar* (car/flushall))
    (println (str "Flush for " regex (refine-string ""))) ; flush the result from the first run
    ::ok))

(defn running?
  []
  (and coastal-instance
       (.isAlive coastal-instance)))

(defn active-parser
  []
  current-parser)

(declare query)

(defrecord PathCondition [accepted constraints]
  IPathCondition
  (accepted? [this] (:accepted this))
  (constraints [this] (:constraints this))
  (length [this] (count (:constraints this)))
  (witness [this] (str/join (map char (z3/witness (:constraints this)))))
  (suffixes [this]
    (map query (map #(str/join (map char %)) (map z3/witness (suffixes* (:constraints this))))))
  (prefixes [this]
    (map query (map #(str/join (map char %)) (map z3/witness (prefixes* (:constraints this)))))))

(defn query
  "Return a map with a set of assertions against `string`, and the parser's
  acceptance status."
  [string]
  (if-not coastal-instance
    (println "No coastal instance to query")
    #_(install-parser! default-parser)) ;; default parser accepts nothing
  (let [[accepted path] (refine-string string)
        constraints (->> path
                         path->constraints
                         (map (fn [[idx op guard]]
                                [(Integer/parseInt idx) op (Integer/parseInt guard)]))
                         (sort-by first)
                         (partition-by first)
                         (map (fn [constraints] (vec (map #(vec (drop 1 %)) constraints))))
                         (map set)
                         (vec))]
    (->PathCondition accepted constraints)))

;; create and fill the table

(defn make-table
  "Table"
  []
  (let [epsilon (query "")]
    {:S {epsilon [(accepted? epsilon)]}
     :R {}
     :E [""]}))

(defn fill-row
  [[path row] evidence]
  (let [row-length (count row)
        evidence-count (count evidence)]
    (if (= row-length evidence-count)
      row
      (let [new-row (reduce (fn [row e]
                              (conj row (accepted? (query (str (witness path) e)))))
                            row
                            (drop row-length evidence))]
        new-row))))

(defn fill
  "Table -> Table"
  [table]
  (let [{:keys [S R E]} table]
    (-> table
        (assoc :S (reduce
                   (fn [new-s [path row]] (assoc new-s path (fill-row [path row] E)))
                   {}, S))
        (assoc :R (reduce
                   (fn [new-r [path row]] (assoc new-r path (fill-row [path row] E)))
                   {}, R)))))

;; closing the table

(defn open-entries
  "Return a entries from R in `table` that do not appear in S. Return nil
  if no open entries are present"
  [{:keys [S R]}]
  (let [s-rows (set (vals S))
        r-rows (set (vals R))]
    (let [candidate-rows (set/difference r-rows s-rows)
          entries (filter (fn [[_ row]] (candidate-rows row)) R)]
      (set (keys entries)))))

(defn closed?
  "Table -> Boolean"
  [{:keys [S R]}]
  (let [s-rows (set (vals S))
        r-rows (set (vals R))]
    (set/subset? r-rows s-rows)))

(defn close
  "Table -> Table"
  [{:keys [R] :as table}]
  (if-not (closed? table)
    (let [promotee (first (open-entries table))
          row (R promotee)]
      (-> table
          (update :R #(dissoc % promotee))
          (update :S #(assoc % promotee row))))
    table))

(defn add-path-condition
  "Table -> PathCondition -> Table"
  [table {:keys [accepted] :as path}]
  (let [prefixes (prefixes path)
        table-with-ce (update table :R #(assoc % path [accepted]))
        table-with-prefixes (reduce (fn [table* {:keys [accepted] :as path}]
                                      (update table* :R #(assoc % path [accepted])))
                                    table-with-ce
                                    prefixes)
        filled-table (fill table-with-prefixes)]
    (loop [table filled-table]
      (if (closed? table)
        table
        (recur (close table))))))

(defn add-evidence
  "Table -> Evidence -> Table"
  [table evidence]
  (let [new-table (-> table
                      (update :E #(conj % evidence))
                      fill)]
    (loop [table new-table]
      (if (closed? table)
        table
        (recur (close table))))))

;; adding new information to the table

(defn add-path-condition
  "Table -> PathCondition -> Table"
  [table {:keys [accepted] :as path}]
  (let [prefixes (prefixes path)
        table-with-ce (update table :R #(assoc % path [accepted]))
        table-with-prefixes (reduce (fn [table* {:keys [accepted] :as path}]
                                      (update table* :R #(assoc % path [accepted])))
                                    table-with-ce
                                    prefixes)
        filled-table (fill table-with-prefixes)]
    (loop [table filled-table]
      (if (closed? table)
        table
        (recur (close table))))))

(defn add-evidence
  "Table -> Evidence -> Table"
  [table evidence]
  (let [new-table (-> table
                      (update :E #(conj % evidence))
                      fill)]
    (loop [table new-table]
      (if (closed? table)
        table
        (recur (close table))))))

;; make an sfa from a table

(defn constraint-set->fn
  "[Constraint] -> (Char -> Boolean)"
  [constraint-set]
  (let [assert->fn (fn [[op bound]]
                     (case op
                       ">=" #(>= % bound)
                       ">" #(> % bound)
                       "==" #(== % bound)
                       "!=" #(not= % bound)
                       "<" #(< % bound)
                       "<=" #(<= % bound)))
        assertions (map assert->fn constraint-set)]
    (fn [input]
      (let [results (map #(% input) assertions)]
        (not (some false? results))))))

(defn- root-entries
  [{:keys [S R]}]
  (let [s+r (merge S R)]
    (assert (= (count s+r) (+ (count S) (count R))) "Elements lost during map merge")
    (group-by (comp #(or (first %) #{}) :constraints first) s+r)))

(defn- roots
  [table]
  (let [root-entries (root-entries table)]
    (map (fn [[prefix entries]]
           [prefix (sort-by (comp length first) entries)]) root-entries)))

(defn pop*
  "Safe version of `clojure.core/pop`; returns nil when `coll` is empty"
  [coll]
  (when (seq coll)
    (pop coll)))

(comment "This explodes!" (pop []))
(comment "This returns nil :)" (pop* []))

(defn- compute-prefix-pairs
  [{:keys [S R]}]
  (let [s+r (merge S R)]
    (reduce (fn [prefix-pairs [path row :as entry]]
              (let [prefixes  (filter (fn [[other-path other-row]]
                                        (= (constraints path)
                                           (pop* (constraints other-path))))
                                      s+r)]
                (conj prefix-pairs [entry :is-prefix-of prefixes])))
            []
            s+r)))

(defn compute-transitions
  "Table -> [Transition]"
  [table]
  (let [prefix-pairs (compute-prefix-pairs table)
        transitions
        (reduce (fn [transitions [[prefix-path prefix-row] _ follow]]
                  (if (seq? follow)
                    (conj transitions
                          (map (fn [[path row]]
                                 (let [guard (first (drop (length prefix-path) (constraints path)))
                                       guard-fn (constraint-set->fn (first guard))]
                                   {:from prefix-row
                                    :guard guard
                                    :guard-fn guard-fn
                                    :to row})) follow))
                    transitions))
                []
                prefix-pairs)]
    (flatten transitions)))

(defn initial-state
  "Table -> State"
  [{:keys [S]}]
  (let [[[path row]] (filter (fn [[path row]] (= 0 (length path))) S)]
    row))

(defn table->sfa
  "Table -> (String -> Maybe Boolean)"
  [table]
  (let [transitions (compute-transitions table)]
    (fn [input]
      (let [halted-state
            (reduce
             (fn [state ch]
               (println (str "state " state ", looking at: " ch) )
               (let [transitions-from-state
                     (filter (fn [{:keys [from to guard-fn]}]
                               (and
                                (= state from)
                                (guard-fn (int ch))))
                             transitions)
                     n-transitions (count transitions-from-state)]
                 (cond
                   (= 0 n-transitions) state ;; self loop
                   (= 1 n-transitions) (:to (first transitions-from-state)) ;; jump as per transitions
                   (>= 2 n-transitions) (let [targets (map :to transitions-from-state)
                                              unique-targets (set targets)]
                                          (if (= 1 (count unique-targets)) ;; jump to the one state possible
                                            (first unique-targets)
                                            (reduced [[::non-det (vec transitions-from-state)]])))
                   :default state)))
             (initial-state table)
             input)]
        (first halted-state)))))

(defn states
  [{:keys [S]}]
  (set (vals S)))

(defn- state-map
  "Convert transitions from our internal representation (states are rows, guards are
  constraint sets) to the format required by Symbolic Automata (states are integers,
  guards are CharPreds)."
  [table]
  (let [states (states table)
        state-labels (map-indexed (fn [idx state]
                                    [state idx]) states)]
    (into {} state-labels)))

(defn final-states
  [{:keys [S] :as table}]
  (let [states (states table)
        state-map (state-map table)
        final-states (filter (fn [state] (first state)) states)]
    (map #(get state-map %) final-states)))

(defn- transition->SFAInputMove
  [{:keys [from to guard]}]
  (intervals/make-transition from to guard))

(defn make-sfa
  [table & [{:keys [minimize?]}]]
  (let [state-map (state-map table)
        transitions (compute-transitions table)
        relabeled (map (fn [{:keys [from guard to]}]
                         {:from (get state-map from)
                          :to (get state-map to)
                          :guard (intervals/constraint-set->CharPred guard)})
                       transitions)
        transitions (map transition->SFAInputMove relabeled)
        initial-state (get state-map (initial-state table))
        final-states (final-states table)
        sfa (SFA/MkSFA
             (doto (LinkedList.) (.addAll transitions))
             (int initial-state)
             (doto (LinkedList.) (.addAll (map int final-states)))
             intervals/solver
             false
             false)]
    (if minimize?
      (.minimize sfa intervals/solver)
      sfa)))

(defn show-dot
  [table]
  (let [sfa (make-sfa table {:minimize? true})]
    (println sfa)
    (.createDotFile ^SFA sfa  "aut" "")
    (sh/sh "dot" "-Tps" "aut.dot" "-o" "outfile.ps")
    (sh/sh "xdg-open" "outfile.ps")))

(defn make-sfa*
  [table]
  (let [sfa (make-sfa table)]
    (.minimize ^SFA sfa intervals/solver)))

(defn make-evidence
  "String -> [String]"
  [word]
  (let [ns (range 0 (count word))]
    (map #(apply str (drop % word)) ns)))

(comment (make-evidence "abc"))

(defn process-counter-example
  "Table -> String -> Table"
  [table counter-example]
  (let [unique-evidence (set (:E table))]
    (reduce (fn [table evidence]
              (if-not (contains? unique-evidence evidence)
                (add-evidence table evidence)
                table))
            (add-path-condition table (query counter-example))
            (make-evidence counter-example))))

(defonce target-parser (atom ""))

(defn equivalent?
  [^SFA target ^SFA candidate]
  (.isEquivalentTo target candidate intervals/solver))

(defn learn
  "Learn `target` to `depth`."
  [target depth-limit]

  (tufte/p ::install-parser!
           (reset! target-parser target)
           (install-parser! target))

  (let [target-sfa (intervals/regex->sfa target)
        equivalence-queries (atom 0)
        start (System/currentTimeMillis)]
    (loop [table (make-table)]
     (let [conjecture (tufte/p ::make-sfa (make-sfa* table))
           ;; _ (show-dot table)
           new-table (tufte/p ::check-equivalence!
                              (loop [depth 1]
                                (let [counter-example (check-equivalence! {:depth depth,
                                                                           :target target
                                                                           :candidate conjecture})]
                                  (if counter-example
                                    (do
                                      (swap! equivalence-queries inc)
                                      (process-counter-example table counter-example))
                                    (if (< depth depth-limit) (recur (inc depth)) table)))))]
       (if (= table new-table)
         (do
           (println (str "Bounded equivalence to depth " depth-limit))
           #_(show-dot new-table)
           [new-table @equivalence-queries (- (System/currentTimeMillis) start)])
         (do
           (pprint new-table)
           (recur new-table)))))))

(def stats-accumulator
  (tufte/add-handler! :symlearn "*"
                      (fn [{:keys [pstats]}]
                        (let [now (System/currentTimeMillis)]
                          (println (str "Result saved to result-" now))
                          (spit (str "result-" now)
                                (str "Parser: " @target-parser
                                     "\n" (tufte/format-pstats pstats)))))))



(defn load-benchmark
  [^String filename]
  (for [node (RegexParserProvider/parse (FileReader. filename))]
    (let [solver intervals/solver]
      (try
        (let [sfa (RegexConverter/toSFA node solver)]
          (.minimize sfa solver))
        (catch UnsupportedOperationException e (str (.getMessage e)))))))

(defn evaluate!
  [{:keys [target depth]}]
  (let [[bounded-candidate eqv-queries walltime] (learn target depth)
        golden-target (intervals/regex->sfa target)]
    {:target target
     :candidate bounded-candidate
     :depth depth
     :walltime-s (/ walltime 1000.00)
     :equivalent? (equivalent? (make-sfa* bounded-candidate) golden-target)
     :equivalence-queries eqv-queries}))

(defn evaluate-benchmark!
  [benchmark max-depth]
  (let [regexes (str/split-lines (slurp benchmark))]
    (loop [depth 1]
      (let [results (map #(evaluate! {:target %
                                      :depth depth})
                         regexes)]
        results))))

(comment
  (def results (evaluate-benchmark! "regexlib-clean-10.re" 1))
  (println (first results))
  (pprint (filter (complement :equivalent?) results))

  (stop!)
  )

(defn integration-tests
  []
  (println (install-parser! "abc|g"))
  (println (query "abc"))
  (println (query "ggwp"))

  (println (install-parser! "ggwp?"))
  (println (query "abc"))
  (println (query "ggwp"))

  (println (check-equivalence! {:depth 2
                                :target "gz"
                                :candidate (intervals/regex->sfa "g")}))

  (println (learn "[^\"]+" 2)))

(defn -main
   "This function is a collection of forms that test all integrations."
  [& args]
  (spit "results.edn" (pr-str (evaluate-benchmark! "regexlib-clean-10.re" 1)))
  (stop!)
  (shutdown-agents))

(defn show-sfa
  [^SFA sfa]
  (.createDotFile sfa  "aut" "")
  (sh/sh "dot" "-Tps" "aut.dot" "-o" "outfile.ps")
  (sh/sh "xdg-open" "outfile.ps"))

(defn compare-graphically
  [target candidate]
  (show-sfa (intervals/regex->sfa target))
  (show-sfa (make-sfa* candidate)))
