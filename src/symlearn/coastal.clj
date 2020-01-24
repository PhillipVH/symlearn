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
            [taoensso.timbre :as log]
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
  (log/trace "Requesting refinement:" string)
  ;; enqueue the string for solving
  (let [exploded-string (map int (.toCharArray ^String string))
        strlen (count string)] ;; avoid "first byte is null" encoding issues
    (wcar* (car/del :refined)
           (apply (partial car/rpush :refine) (if (= 0 strlen) ["epsilon"] exploded-string))))

  ;; wait for a solved response
  (while (not= 1 (wcar* (car/exists :refined))))

  ;; process reponse
  (let [refined-path (tufte/p ::refine-path (wcar* (car/get :refined)))
        [accepted path-condition] (str/split refined-path #"\n")]
    (wcar* (car/del :refined))
    (log/trace "Refinement received:" accepted)
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
  (sh/with-sh-dir "eqv-coastal-new"
    ;; compile coastal
    (let [clean-log (:out (sh/sh "./gradlew" "clean"))]
      (log/info clean-log))
    (let [gradle-log (:out (sh/sh "./gradlew" "--build-cache" "compileJava" "installDist" "-x" "test"))]
      (log/info gradle-log))

    ;; install coastal runner
    (log/info (:out (sh/sh "cp" "-r" "build/install/coastal/" "build/classes/java/main/")))))

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
  (spit "eqv-coastal-new/src/main/java/learning/Example.java"
        (mk-equivalence-oracle candidate target depth))
  (log/info "Compiling Equivalence Oracle:" {:target target, :depth depth})
  (compile-equivalence-oracle!))

(defn check-equivalence!
  [{:keys [depth target ^SFA candidate]}]
  (log/info "Starting Equivalence Check:" {:target target, :depth depth})
  (let [coastal-log (:out (sh/sh "./coastal/bin/coastal" "learning/Example.properties" :dir "eqv-coastal-new/build/classes/java/main"))
        ce (re-seq #"<<Counter Example: \[(.*)\]>>" coastal-log)]
    (log/info coastal-log)
    (log/info "Finished Equivalence Check:" {:target target, :depth depth})

    (when ce
      (let [ce (map second ce)
            chars (map #(str/split % #",") ce)]
        (set (map #(apply str %)(map #(map str/trim %) chars)))))))

(defn stop!
  "Stop a Coastal process running in `coastal`."
  []
  (let [coastal-pid (str/trim (:out (sh/sh "pgrep" "-f" "COASTAL.*Regex")))]
    (sh/sh "kill" "-9" coastal-pid))
  (wcar* (car/flushall))
  (if coastal-instance
    (do
      (.destroyForcibly coastal-instance)
      (while (.isAlive coastal-instance))
      (alter-var-root #'coastal-instance (constantly nil)))
    (log/info "No coastal to stop"))
  ::ok)

(defn coastal-pid
  "Argument can be :mem for membership or :eqv for equivalence"
  [mem-or-eqv]
  (let [mem (= :mem mem-or-eqv)
        ident (if mem "Regex" "Example")]
    (str/trim (:out (sh/sh "pgrep" "-f" (str "COASTAL.*" ident))))))

(defn kill-pid!
  [pid]
  (sh/sh "kill" "-9" pid))

(defn check-equivalence-timed!
  [{:keys [depth target ^SFA candidate timeout-ms]}]
  (install-equivalence-oracle! candidate target depth) ;; don't include compilation time
  (let [f (future (check-equivalence! {:depth depth
                                       :target target
                                       :candidate candidate}))
        ce (deref f
                  timeout-ms
                  ::timeout)]
    (if (= ce ::timeout)
      (do
        (log/trace "Equivalence Check for" target "timed out after" timeout-ms "ms")
        (future-cancel f)
        (kill-pid! (coastal-pid :eqv))
        ::timeout)
      (do
        (log/trace "Target" target "learnt successfully")
        ce))))

(defn ^Process start!
  "Launch a Coastal process with a config file called `filename` as an argument."
  []
  (if coastal-instance
    (stop!))
  (tufte/p
   ::start-coastal
   (let [path-in-docker (System/getenv "MEMBERSHIP_CONFIG_PATH")
         args-in (or path-in-docker (str (System/getProperty "user.dir") "/resources/Regex.xml"))
         args (into-array ["./gradlew" "run" (str "--args=" args-in)])
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
    (dotimes [_ 3]
      (refine-string "")) ;; flush the default run from the membership oracle
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
    (log/warn "No membership oracle registered"))
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
               (log/info (str "state " state ", looking at: " ch) )
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
    (log/info sfa)
    (.createDotFile ^SFA sfa  "aut" "")
    (sh/sh "dot" "-Tps" "aut.dot" "-o" "outfile.ps")
    (sh/sh "xdg-open" "outfile.ps")
    (sh/sh "rm" "outfile.ps")))

(defn show-sfa
  [^SFA sfa]
  (.createDotFile sfa  "aut" "")
  (sh/sh "dot" "-Tps" "aut.dot" "-o" "outfile.ps")
  (sh/sh "xdg-open" "outfile.ps"))

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

(defn m->ms
  "Returns `m` minutes in milliseconds."
  [m]
  (* 1000 60 m))

(defn learn
  "Learn `target` to `depth`."
  [target depth-limit timeout-ms]
  (log/info "Learning" {:target target, :depth depth-limit, :timeout-ms timeout-ms})

  ;; install the membership oracle
  (tufte/p ::install-parser!
           (reset! target-parser target)
           (install-parser! target))

  (let [target-sfa (intervals/regex->sfa target)
        equivalence-queries (atom 0)
        start (System/currentTimeMillis)]
    (loop [table (make-table)]
      (let [conjecture (make-sfa* table)
            new-table (loop [depth 1]
                        (let [counter-example (check-equivalence-timed! {:depth depth,
                                                                         :target target
                                                                         :candidate conjecture
                                                                         :timeout-ms timeout-ms})]
                          (cond
                            ;; no counter example, search deeper or yield table
                            (nil? counter-example)
                            (if (< depth depth-limit) (recur (inc depth)) table)

                            ;; equivalence check timed out
                            (= counter-example ::timeout)
                            ::timeout

                            ;; found a counter example, process and return new table
                            :else
                            (do
                              (log/info "Applying counter example(s):" counter-example)
                              (swap! equivalence-queries inc)
                              (reduce (fn [new-table ce]
                                        (log/trace "Applying" ce)
                                        (process-counter-example new-table ce))
                                      table
                                      counter-example)))))]

        (cond
          ;; equivalence check timed out
          (= ::timeout new-table)
          (do
            (log/info "Timeout")
            {:table table
             :queries {:eqv @equivalence-queries
                       :mem -1}
             :time (- (System/currentTimeMillis) start)
             :status :incomplete
             :equivalence :timeout})

          ;; the two SFAs are entirely equivalent
          (equivalent? target-sfa (make-sfa* new-table))
          (do
            (log/info "Total Equivalence")
            {:table new-table
             :queries {:eqv @equivalence-queries
                       :mem -1}
             :time (- (System/currentTimeMillis) start)
             :status :complete
             :equivalence :total})

          ;; the candidate SFA is equivalent to a bound
          (= table new-table)
          (do
            (log/info "Bounded Equivalence:" {:depth depth-limit})
            {:table new-table
             :queries {:eqv @equivalence-queries
                       :mem -1}
             :time (- (System/currentTimeMillis) start)
             :status :complete
             :equivalence :bounded})

          ;; continue learning
          :else
          (do
            (pprint new-table)
            (recur new-table)))))))

(defn load-benchmark
  [^String filename]
  (for [node (RegexParserProvider/parse (FileReader. filename))]
    (let [solver intervals/solver]
      (try
        (let [sfa (RegexConverter/toSFA node solver)]
          (.minimize sfa solver))
        (catch UnsupportedOperationException e (str (.getMessage e)))))))

(defn timeout [timeout-ms callback]
  (let [fut (future (callback))
        ret (deref fut timeout-ms ::timed-out)]
    (when (= ret ::timed-out)
      (future-cancel fut)
      (stop!))
    ret))

(defn regex->sfa*
  "A safe version of `intervals/regex->sfa`, catching unsupported regex exceptions from
  the underlying parser, and timing out on parsers that take too long to construct."
  [target]
  (timeout 5000 #(try (intervals/regex->sfa target) (catch Exception e ::unsupported-regex))))


(defn evaluate!
  "A wrapper around `learn` that insulates us from some of the harsh realities of
  evaluation. If `target` causes the underlying `RegexParserProvider` to crash, or
  the provider takes too long to produce a parser. Calls `learn` as the last step,
  given no error state."
  [{:keys [target depth timeout-ms]}]
  (let [?sfa (regex->sfa* target)]
    (cond
      (= ::timed-out ?sfa)
      {:target target
       :equivalence ::timed-out}

      (= ::unsupported-regex ?sfa)
      {:target target
       :equivalence ::unsupported-regex}

      :else
      (-> (learn target depth timeout-ms)
          (assoc :target target)))))

(defn evaluate-benchmark!
  "Evaluate a given benchmark file, learning to `max-depth`, with a timeout
  on each equivalence check of `timeout-ms`."
  [benchmark max-depth timeout-ms]
  (let [regexes (str/split-lines (slurp benchmark))
        results (reduce (fn [results target]
                          (let [evaluation (evaluate! {:target target
                                                       :depth max-depth
                                                       :timeout-ms timeout-ms})]
                            (Thread/sleep 5000) ;; rest a bit between experiments
                            (conj results evaluation)))
                        []
                        regexes)]
    results))

(comment (pprint (evaluate-benchmark! "regexlib-clean-10.re" 1 (m->ms 2))))

;; integration tests

(defn membership-integration-tests
  "Test the integration between the learner and the membership oracle."
  []
  (log/info "Testing Membership Oracle")
  (install-parser! "abc|g")
  (let [should-accept (query "abc")
        should-reject (query "ggwp")]
    (assert (:accepted should-accept))
    (assert (not (:accepted should-reject))))

  (install-parser! "ggwp?")
  (let [should-reject (query "abc")
        should-accept (query "ggwp")]
    (assert (not (:accepted should-reject)))
    (assert (:accepted should-accept)))

  (let [results (for [i (range 1000)]
                  (query (str i)))]
    (assert (= 1000 (count results))))
  (log/info "All Membership Tests Pass"))

(defn equivalence-integration-tests
  "Test the integration between the learner and the equivalence oracle."
  []
  (log/info "Testing Equivalence Oracle")
  (let [ce (check-equivalence-timed! {:depth 2
                                      :target "gz"
                                      :candidate (intervals/regex->sfa "g")
                                      :timeout-ms (m->ms 4)})]
    (assert (= #{"gz"} ce)))

  (let [ce (check-equivalence-timed! {:depth 2
                                      :target "g(z|a)"
                                      :candidate (intervals/regex->sfa "g")
                                      :timeout-ms (m->ms 4)})]
    (assert (= #{"gz" "ga"} ce)))

  (let [ce (check-equivalence-timed! {:depth 2
                                      :target "g"
                                      :candidate (intervals/regex->sfa "ga")
                                      :timeout-ms (* 30 1000)})]
    (assert (= ::timeout ce)))

  (let [ce (check-equivalence-timed! {:depth 2
                                      :target "g(z|a)"
                                      :candidate (intervals/regex->sfa "g")
                                      :timeout-ms (* 1000 60 3)})]
    (assert (= #{"gz" "ga"} ce)))
  (log/info "All Equivalence Tests Pass"))

(defn learner-integration-tests
  []
  (log/info "Testing Learner")
  (let [{:keys [table queries time status equivalence]} (learn "b|aa" 1 (m->ms 30))
        conjecture (make-sfa* table)
        bounded-target (intervals/regex->sfa "b")
        total-target (intervals/regex->sfa "b|aa")]
    (assert (equivalent? bounded-target conjecture))
    (assert (not (equivalent? total-target conjecture)))
    (assert (= :complete status))
    (assert (= :bounded equivalence)))

  (let [{:keys [table queries time status equivalence]} (learn "[^\"]+" 2 (m->ms 30))
        target (intervals/regex->sfa "[^\"]+")
        conjecture (make-sfa* table)]
    (assert (equivalent? target conjecture))
    (assert (= :complete status))
    (assert (= :total equivalence)))

  (let [{:keys [table queries time status equivalence]} (learn "[^\"]+" 2 100)
        target (intervals/regex->sfa "[^\"]+")
        conjecture (make-sfa* table)]
    (assert (not (equivalent? target conjecture)))
    (assert (= :incomplete status))
    (assert (= :timeout equivalence)))
  (log/info "All Learner Tests Pass"))

(defn regex->sfa*-tests
  []
  (assert (= ::timed-out (regex->sfa* "\\w{5,255}")))
  (assert (= ::unsupported-regex (regex->sfa* "\\p{N}]")))
  (assert (equivalent? (intervals/regex->sfa "b")
                       (regex->sfa* "b"))))

(defn pathological-regex
  []
  (learn "^\\w+.*$" 2))

(defn integration-tests
  "Checks integration between the learner and the equivalence + membership oracles."
  []
  (log/info "Running Integration Tests")
  (membership-integration-tests)
  (equivalence-integration-tests)
  (learner-integration-tests)
  (regex->sfa*-tests)
  (log/info "All Integration Tests Pass"))

(defn evaluate-regexlib
  [{:keys [depth eqv-timeout]}]
  (log/info "Starting regexlib Evaluation")
  (let [results (evaluate-benchmark! "regexlib-clean-100.re" depth eqv-timeout)]
    (spit "results/results.edn" (pr-str results))
    (log/info "Finished regexlib Evaluation")))

(defn -main
  [& args]
  (evaluate-regexlib {:depth 3, :eqv-timeout (m->ms 10)})
  (stop!)
  (shutdown-agents))
