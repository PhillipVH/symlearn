(ns symlearn.coastal
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [taoensso.carmine :as car]
            [taoensso.tufte :as tufte]
            [clojure.walk :as walk]
            [symlearn.paths :as paths]
            [symlearn.intervals :as intervals]
            [symlearn.ranges :as ranges]
            [symlearn.table :as table]
            [symlearn.z3 :as z3]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.set :as set])
  (:import [java.io File]
           [java.util LinkedList]
           [automata.sfa SFA]))

(set! *warn-on-reflection* true)

(defonce ^Process *coastal-instance* nil)
(defonce *current-parser* nil)

(def string-config "Regex.xml")
(def default-parser "")

(defmacro wcar*
  "Wraps Redis commands in a `car/wcar`."
  [& body]
  `(let [redis-conn# {:pool {} :spec {:host "127.0.0.1" :port 6379}}]
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
        args (into-array ["./gradlew" "compileJava"])
        builder (ProcessBuilder. ^"[Ljava.lang.String;" args)]
    (.directory builder coastal-dir)
    (let [compiler (.start builder)]
      (while (.isAlive compiler))
      ::ok)))

(defn stop!
  "Stop a Coastal process running in `coastal`."
  []
  (when *coastal-instance*
    (.destroyForcibly *coastal-instance*)
    (while (.isAlive *coastal-instance*))
    (alter-var-root #'*coastal-instance* (constantly nil)))
  ::ok)

(defn ^Process start!
  "Launch a Coastal process with a config file called `filename` as an argument."
  []
  (if *coastal-instance*
    (stop!))
  (tufte/p
   ::start-coastal
   (let [config (io/resource string-config)
         args (into-array ["./gradlew" "run" (str "--args=" (.getPath config))])
         builder (ProcessBuilder. ^"[Ljava.lang.String;" args)
         coastal-dir (File. "coastal")]
     (.directory builder coastal-dir)
     (let [coastal-instance (.start builder)]
       (alter-var-root #'*coastal-instance* (constantly coastal-instance))
       ::ok))))

(defn install-parser!
  [regex]
  (if *coastal-instance*
    (stop!))
  (alter-var-root #'*current-parser* (constantly regex))
  (let [parser-src (intervals/sfa->java (intervals/regex->sfa regex) "examples.tacas2017" "Regex")]
    (spit "coastal/src/main/java/examples/tacas2017/Regex.java" parser-src)
    (compile-parsers!)
    (start!)
    ;; flush the result from the default run
    (refine-string "")
    ::ok))

(defn running?
  []
  (and *coastal-instance*
       (.isAlive *coastal-instance*)))

(defn active-parser
  []
  *current-parser*)

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
  (if-not *coastal-instance*
    (install-parser! default-parser)) ;; default parser accepts nothing
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

;; closing a table

(defn closed?
  "Table -> Boolean"
  [{:keys [S R]}]
  (let [s-rows (set (vals S))
        r-rows (set (vals R))]
    (set/subset? r-rows s-rows)))

(defn open-entries
  "Return a entries from R in `table` that do not appear in S. Return nil
  if no open entries are present"
  [{:keys [S R]}]
  (let [s-rows (set (vals S))
        r-rows (set (vals R))]
    (let [candidate-rows (set/difference r-rows s-rows)
          entries (filter (fn [[_ row]] (candidate-rows row)) R)]
      (set (keys entries)))))

(defn close
  "Table -> Table"
  [{:keys [R] :as table}]
  (if-not (closed? table)
    (let [promotee (first (open-entries table))
          row (R promotee)]
      (-> table
          (update-in [:R] #(dissoc % promotee))
          (update-in [:S] #(assoc % promotee row))))
    table))

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

(defn accepts?
  "Table -> String -> Maybe Boolean"
  [table input]
  ((table->sfa table) input))

#_(defn generate-langauge
  "Table -> [String]"
  [table])

#_(defn equivalent?
  [table depth]
  (let [accepts? (table->sfa)]))

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
  [table]
  (let [state-map (state-map table)
        transitions (compute-transitions table)
        relabeled (map (fn [{:keys [from guard to]}]
                         {:from (get state-map from)
                          :to (get state-map to)
                          :guard (intervals/constraint-set->CharPred guard)})
                       transitions)
        transitions (map transition->SFAInputMove relabeled)
        initial-state (get state-map (initial-state table))
        final-states (final-states table)]
    (SFA/MkSFA
     (doto (LinkedList.) (.addAll transitions))
     (int initial-state)
     (doto (LinkedList.) (.addAll (map int final-states)))
     intervals/solver
     false
     false)))


(defn show-dot
  [table & [{:keys [minimize?]}]]
  (let [sfa (make-sfa table)
        min-sfa (if minimize? (.minimize sfa intervals/solver) sfa)]
    (println sfa)
    (.createDotFile min-sfa  "aut" "")
    (sh/sh "dot" "-Tps" "aut.dot" "-o" "outfile.ps")
    (sh/sh "xdg-open" "outfile.ps")))


(-> (make-table)
    (add-path-condition (query "a"))
    (add-evidence "a")

    (add-path-condition (query "abc"))
    (add-evidence "abc")
    (add-evidence "bc")
    (add-evidence "c")

    (add-path-condition (query "aa"))
    (add-evidence "aa")

    (add-path-condition (query "aaa"))
    (add-evidence "aaa")

    (show-dot {:minimize? false}))

(defn generate-language
  [^SFA sfa])

#_(defn add-ce
    [table ce]
    (-> table
        (add-path-condition (query ce))
        (add-evidence ce)))
