(ns symlearn.coastal
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
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

;; time utilities

(defn m->ms
  "Returns `m` minutes in milliseconds."
  [m]
  (* 1000 60 m))

(defn ms->m
  [ms]
  (/ ms 60000))

(defn ms-to-timeout
  "Returns the number of milliseconds left"
  [start now]
  (let [diff (- now start)
        minutes (float (/ diff 60000))]
    (if (>= minutes 10)
      0
      (m->ms (- 10 minutes)))))

;; coastal integration

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

;; code generation and compilation

(defn compile-parsers!
  "Compile the parsers installed in the Coastal system."
  []
  (sh/with-sh-dir "coastal"
    (sh/sh "./gradlew" "clean"))
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
                    interval-size (.size zi-intervals)]
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

;; process management

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

(defn coastal-emergency-stop
  []
  (map kill-pid! (map coastal-pid #{:mem :eqv})))

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
  (log/info "Generating Parser:" {:target regex})
  (let [parser-src (intervals/sfa->java (intervals/regex->sfa regex) "examples.tacas2017" "Regex")]
    (spit "coastal/src/main/java/examples/tacas2017/Regex.java" parser-src)
    (log/info "Compiling Parser:" {:target regex})
    (compile-parsers!)
    (log/info "Starting Parser:" {:target regex})
    (start!)
    (log/info "Flushing Parser:" {:target regex})
    (dotimes [_ 3]
      (refine-string "")) ;; flush the default run from the membership oracle
    (log/info "Parser Ready:" {:target regex})
    ::ok))

;; table manipulation

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

(def mem-queries (atom 0))
(def newest-table (atom nil))

(defn query
  "Return a map with a set of assertions against `string`, and the parser's
  acceptance status."
  ([string count?]
   (when count
     (swap! mem-queries inc))
   (query string))
  ([string]
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
     (->PathCondition accepted constraints))))

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
                              (conj row (accepted? (query (str (witness path) e) :count))))
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

(defn close-totally
  "Keep closing a table until it is closed."
  [table]
  (loop [table table]
    (if (closed? table)
      table
      (recur (close table)))))

(defn add-path-condition
  "Table -> PathCondition -> Table"
  [table {:keys [accepted] :as path}]
  (if (get table path)
    (log/error "Duplicate entry to table:" path)
    (let [prefixes (prefixes path)
          table-with-ce (update table :R #(assoc % path [accepted]))
          table-with-prefixes (reduce (fn [table* {:keys [accepted] :as path}]
                                        (update table* :R #(assoc % path [accepted])))
                                      table-with-ce
                                      prefixes)
          filled-table (fill table-with-prefixes)]
      (close-totally filled-table))))

(defn make-evidence
  "String -> [String]"
  [word]
  (let [ns (range 0 (count word))]
    (map #(apply str (drop % word)) ns)))

(defn add-evidence
  "Table -> Evidence -> Table"
  [table evidence]
  (let [new-table (-> table
                      (update :E #(conj % evidence))
                      fill)]
    (close-totally new-table)))

(defn add-evidence*
  "Like `add-evidence`, but adds all the suffixes to E" ;; TODO Better suffix selection
  [table counter-example]
  (reduce (fn [table evidence]
            (add-evidence table evidence))
          table
          (make-evidence counter-example)))

;; sfa creation

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
        transitions (set (compute-transitions table))
        relabeled (set (map (fn [{:keys [from guard to]}]
                          {:from (get state-map from)
                           :to (get state-map to)
                           :guard (intervals/constraint-set->CharPred guard)})
                        transitions))
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
    (.minimize ^SFA sfa intervals/solver))) ;; TODO This can throw org.sat4j.specs.TimeoutException

(defn ce->pred
  [ce]
  (let [path-condition (constraints (query ce))]
    (map intervals/constraint-set->CharPred path-condition)))

(defn- predicate-transitions
  [table]
  (let [state-map (state-map table)
        transitions (set (compute-transitions table))
        relabeled (set (map (fn [{:keys [from guard to]}]
                              {:from (get state-map from)
                               :to (get state-map to)
                               :guard (intervals/constraint-set->CharPred guard)})
                           transitions))]
    relabeled))

(defn- intersection-of-transitions
  [transitions]
  (let [guards (map :guard transitions)
        empty-intersection (CharPred/of (ImmutableList/of))
        intersections (for [guard guards] ;; for every transition out of q_i
                        (flatten (map (fn [other-guard]
                                        (if (identical? guard other-guard)
                                          empty-intersection
                                          (intervals/intersection guard other-guard)))
                                      guards)))
        all (set (flatten intersections))]
    (reduce (fn [pred intersection]
              (intervals/union pred intersection))
            empty-intersection
            all)))

(deftest intersections
  (let [transitions [{:guard (intervals/make-interval \a \b)}
                     {:guard (intervals/make-interval \b \c)}]

        empty-intersection (CharPred/of (ImmutableList/of))
        intersection (intersection-of-transitions transitions)]
    (is (= (intervals/make-interval \b \b)
           intersection))
    (is (= empty-intersection
           (intersection-of-transitions [{:guard (intervals/make-interval \a \b)}])))
    (is (= empty-intersection
           (intersection-of-transitions [])))))

(defn deterministic?
  "Check for any outgoing transitions that have non-empty intersection."
  [table]
  (let [grouped-transitions (group-by :from (predicate-transitions table))
        empty-intersection (CharPred/of (ImmutableList/of))
        intersection-per-state (map (fn [[_ transitions]]
                                      (intersection-of-transitions transitions))
                                    grouped-transitions)
        non-empty (filter #(not= % empty-intersection) intersection-per-state)]
    (= 0 (count non-empty))))

(defn process-counter-example
  "Table -> String -> Table"
  [table counter-example]
  (let [unique-evidence (set (:E table))
        paths (map (comp constraints query) (:E table))
        unique-paths (set (map #(map intervals/constraint-set->CharPred %) paths))]
    (let [new-table (add-path-condition table (query counter-example :count))]
      (if (= 1 (count unique-evidence))
        (add-evidence* new-table counter-example)
        (if-not (deterministic? new-table)
          (do
            (log/info "Table has non-deterministic transitions, applying evidence.")
            (add-evidence* new-table counter-example))
          (do
            (log/info "Table is deterministic, not applying evidence.")
            new-table))))))

;; evaluation

(defn timeout
  [timeout-ms callback]
  (let [fut (future (callback))
        ret (deref fut timeout-ms ::timed-out)]
    (when (= ret ::timed-out)
      (future-cancel fut)
      (stop!))
    ret))

(defn check-equivalence!
  [{:keys [depth target ^SFA candidate]}]
  (log/info "Starting Equivalence Check:" {:target target, :depth depth})
  (let [coastal-log (:out (sh/sh "./coastal/bin/coastal"
                                 "learning/Example.properties"
                                 :dir "eqv-coastal-new/build/classes/java/main"))
        ce (re-seq #"<<Counter Example: (.*)>>" coastal-log)]
    (log/info coastal-log)
    (log/info "Finished Equivalence Check:" {:target target, :depth depth})
    (when ce
      (let [ce (map second ce)]
        (set ce)))))

(defn check-equivalence-timed!
  [{:keys [depth target ^SFA candidate timeout-ms]}]
  (install-equivalence-oracle! candidate target depth) ;; don't include compilation time
  (log/info "Minutes for this check:" (ms->m timeout-ms))
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

(defn check-equivalence-perfect
  "Use symbolicautomata to check our candidate against a perfect target."
  [^SFA target ^SFA candidate]
  (let [check (SFA/areEquivalentPlusWitness target candidate intervals/solver (m->ms 10))
        equivalent? (.getFirst check)]
    (when-not equivalent?
      #{(str/join "" (.getSecond check))})))

(defn equivalent?
  [^SFA target ^SFA candidate]
  (.isEquivalentTo target candidate intervals/solver))



(defn learn
  "Learn `target` to `depth`."
  [{:keys [target depth-limit timeout-ms oracle] :or {oracle :coastal}}]
  (log/info "Learning" {:target target, :depth depth-limit, :timeout-ms timeout-ms})

  ;; install the membership oracle
  (reset! mem-queries 0)

  (let [installed? (timeout (* 1000 30) #(install-parser! target))]
    (if (= ::timed-out installed?)
      ;; parser not installed, return a timeout
      (do
        (log/error "Parser Installation Timed Out")
        (coastal-emergency-stop)
        (Thread/sleep 5000)
        {:target target
         :status :incomplete
         :equivalence :parser-timeout})

      ;; parser installed, begin learning
      (let [target-sfa (.minimize (intervals/regex->sfa target) intervals/solver)
            equivalence-queries (atom 0)
            start (System/currentTimeMillis)
            max-depth (atom 1)
            cached-ces (atom #{})]
        (loop [table (make-table)]
          (let [conjecture (make-sfa* table)
                new-table (loop [depth 1]
                            ;; update the max depth
                            (swap! max-depth (fn [max]
                                               (if (> depth max)
                                                 depth
                                                 max)))
                            (let [counter-example
                                  (if (= :perfect oracle)
                                    (check-equivalence-perfect target-sfa conjecture)
                                    (check-equivalence-timed!
                                     {:depth depth,
                                      :target target
                                      :candidate conjecture
                                      :timeout-ms (ms-to-timeout start
                                                                 (System/currentTimeMillis))}))]
                              (cond
                                ;; no counter example, search deeper or yield table
                                (nil? counter-example)
                                (if (< depth depth-limit)
                                  (recur (inc depth))
                                  table)

                                ;; equivalence check timed out
                                (= counter-example ::timeout)
                                ::timeout

                                ;; found a counter example, process and return new table
                                :else
                                (do
                                  (log/info "Got counter example(s):" counter-example)
                                  (swap! equivalence-queries inc)
                                  (reduce (fn [new-table ce]
                                            (if (contains? @cached-ces (ce->pred ce))
                                              (log/error "Duplicate CE:" ce)
                                              (swap! cached-ces conj (ce->pred ce)))
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
                           :mem @mem-queries
                           :max-eqv-depth @max-depth}
                 :time (- (System/currentTimeMillis) start)
                 :status :incomplete
                 :equivalence :timeout})

              ;; the two SFAs are entirely equivalent
              (equivalent? target-sfa (make-sfa* new-table))
              (do
                (log/info "Total Equivalence")
                {:table new-table
                 :queries {:eqv @equivalence-queries
                           :mem @mem-queries
                           :max-eqv-depth @max-depth}
                 :time (- (System/currentTimeMillis) start)
                 :status :complete
                 :equivalence :total})

              ;; the candidate SFA is equivalent to a bounded
              (= table new-table)
              (do
                (log/info "Bounded Equivalence:" {:depth depth-limit})
                {:table new-table
                 :queries {:eqv @equivalence-queries
                           :mem @mem-queries
                           :max-eqv-depth @max-depth}
                 :time (- (System/currentTimeMillis) start)
                 :status :complete
                 :equivalence :bounded})

              ;; continue learning
              :else
              (let [now (System/currentTimeMillis)
                    diff (- now start)
                    minutes (/ diff 60000)]
                (if (> minutes 10)
                  (do
                    (log/info "Timeout (Global)")
                    {:table table
                     :queries {:eqv @equivalence-queries
                               :mem @mem-queries
                               :max-eqv-depth @max-depth}
                     :time (- (System/currentTimeMillis) start)
                     :status :incomplete
                     :equivalence :timeout})
                  (do
                    (log/info "Beginning next learning cycle (time left:" (float (- 10 minutes))"minutes)")
                    (recur new-table)))))))))))



(defn regex->sfa*
  "A safe version of `intervals/regex->sfa`, catching unsupported regex exceptions from
  the underlying parser, and timing out on parsers that take too long to construct."
  [target]
  (timeout 5000 #(try (intervals/regex->sfa target) (catch Exception e ::unsupported-regex))))

;; evaluation

(defn evaluate!
  "A wrapper around `learn` that insulates us from some of the harsh realities of
  evaluation. If `target` causes the underlying `RegexParserProvider` to crash, or
  the provider takes too long to produce a parser. Calls `learn` as the last step,
  given no error state."
  [{:keys [target depth timeout-ms oracle] :or {oracle :coastal}}]
  (let [?sfa (regex->sfa* target)]
    (cond
      (= ::timed-out ?sfa)
      {:target target
       :equivalence ::timed-out}

      (= ::unsupported-regex ?sfa)
      {:target target
       :equivalence ::unsupported-regex}

      :else
      (-> (learn {:target target,
                  :depth-limit depth
                  :timeout-ms timeout-ms
                  :oracle oracle})
          (assoc :target target)))))

(defn evaluate-benchmark!
  "Evaluate a given benchmark file, learning to `max-depth`, with a timeout
  on each equivalence check of `timeout-ms`."
  [benchmark max-depth timeout-ms oracle]
  (let [regexes (str/split-lines (slurp benchmark))
        results (reduce (fn [results target]
                          (let [evaluation (evaluate! {:target target
                                                       :depth max-depth
                                                       :timeout-ms timeout-ms
                                                       :oracle oracle})
                                new-results (conj results evaluation)]
                            (log/info "Evaluation of" target "complete")
                            (log/info evaluation)
                            (Thread/sleep 5000) ;; rest a bit between experiments
                            (spit "results/results.edn" (pr-str new-results))
                            new-results))
                        []
                        regexes)]
    results))

(defn evaluate-regexlib
  ([]
   (do
     (log/info "Starting regexlib Evaluation")
     (let [[depth timeout-ms] (str/split-lines (slurp "results/benchmark.spec"))
           results (evaluate-benchmark! "results/benchmark.re"
                                        (read-string depth)
                                        (read-string timeout-ms)
                                        :coastal)]
       (sh/sh "mkdir" "-p" "results")
       (spit "results/results.edn" (pr-str results))
       (log/info "Finished regexlib Evaluation"))))
  ([file depth timeout-ms oracle]
   (do
     (log/info "Starting regexlib Evaluation")
     (let [results (evaluate-benchmark! file
                                        depth
                                        timeout-ms
                                        oracle)]
       (sh/sh "mkdir" "-p" "results")
       (spit "results/results.edn" (pr-str results))
       (log/info "Finished regexlib Evaluation")))))

;; reporting

(defn escape-string
  [string]
  (let [escapes (map char-escape-string string)
        chars (str/split string #"")]
    (reduce (fn [string idx]
              (if (nth escapes idx)
                (str string (nth escapes idx))
                (str string (nth chars idx))))
            ""
            (range (count string)))))

(defn load-benchmark
  [filename]
  (-> filename
      slurp
      str/split-lines))

(defn benchmarks->csv
  [benchmarks]
  (let [indexed (map-indexed (fn [idx benchmark]
                               (assoc benchmark :target-id idx))
                             benchmarks)
        header (str/join \, ["TargetID"
                              "Model.StateCount"
                              "Model.TransitionCount"
                              "Memb.Queries"
                              "Equiv.CE"
                              "Learner.Type"
                              "TargetRegex"
                              "\n"])]
    (reduce (fn [csv benchmark]
              (if (or (= ::timed-out (:equivalence benchmark))
                      (= "Regex is not supported" (:equivalent? benchmark))
                      (nil? (:table benchmark)))
                (str csv (:target-id benchmark) ",Timeout\n")
                (let [candidate ^SFA (make-sfa (:table benchmark))
                      state-count (.stateCount candidate)
                      target (:target benchmark)
                      transition-count (.getTransitionCount candidate)
                      target-id (:target-id benchmark)
                      {:keys [mem eqv]} (:queries benchmark)
                      line (str/join \, [target-id
                                          state-count
                                          transition-count
                                          mem
                                          eqv
                                          (:equivalence benchmark)
                                          (escape-string target)])]
                  (str csv line "\n"))))
            header
            indexed)))

(defn -main
  [& args]
  (evaluate-regexlib)
  (stop!)
  (shutdown-agents))

(comment

  (def bench (load-benchmark "regexlib-stratified.re"))


  (coastal-emergency-stop)

  (def evaluation (future
                    (evaluate! {:target (nth bench 2)
                                :depth 30
                                :timeout-ms (m->ms 10)
                                :oracle :coastal})))

  (future-done? evaluation)
  (future-cancel evaluation)~

  (pprint evaluation)

  (show-sfa (regex->sfa* (nth bench 9)))
  (show-sfa (make-sfa* (:table @evaluation)))

  (coastal-emergency-stop)

  (def eval-fullset (future (evaluate-regexlib "regexlib-stratified.re" 30 (m->ms 10) :perfect)))

  (def eval-partial-last (future (evaluate-regexlib "rest.re" 30 (m->ms 10) :perfect)))

  (java.util.Collections/binarySearch [0 1 2 8] 8 compare))
