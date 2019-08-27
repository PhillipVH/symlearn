(ns constraint-processing.learner
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [taoensso.carmine :as car :refer [wcar]]
            [taoensso.tufte :as tufte]
            [constraint-processing.core :as paths]
            [constraint-processing.sfa :as sfa]
            [constraint-processing.ranges :as ranges])
  (:import Parser
           TacasParser
           SingleParser
           LearnLarge))

(def redis-conn
  "The default connection details for Redis running on localhost."
  {:pool {} :spec {:host "127.0.0.1" :port 6379}})

(defmacro wcar*
  "Wraps Redis commands in a `car/wcar`."
  [& body] `(car/wcar redis-conn ~@body))

(def ^:dynamic *parse-fn*
  "A function wrapper for the underlying parser."
  #(TacasParser/parse %))

(defn make-concrete
  "Return a seq of the first element of every constraint in `path`."
  [path]
  (map first path))

(defn mixed->concrete
  "Take a (potentially mixed) seq of constraints,
  and return a concrete input that the underlying domain
  parser understands. Result is an integer array."
  [path]
  (let [concrete (reduce (fn [concrete constraint]
                           (cond
                             (empty? constraint)
                             concrete

                             (= :c (first constraint))
                             (concat concrete (rest constraint))

                             (= 2 (count constraint))
                             (conj concrete (first constraint))))
                         []
                         path)]
    (into [] concrete)))

(defn member?
  "Return the result of calling `*parse-fun*` with `input`, true if the parser accepts,
  false if it rejects."
  [input]
  (*parse-fn* (into-array Integer/TYPE input)))

(defn check-membership
  "Takes a path condition and a seq of evidence. Returns
  an ordered seq of membership query results."
  [path evidence]
  (map #(member? (mixed->concrete (conj (vec path) %))) evidence))

(defn fill-entry
  "Returns an entry where a membership query has been issued for each piece of evidence."
  [entry evidence]
  (let [{:keys [row path]} entry
        row-length (count row)
        evidence-length (count evidence)]
    (if-not (= row-length evidence-length)
      (tufte/p ::fill-entry (assoc entry :row (vec (check-membership path evidence))))
      (tufte/p ::no-fill-entry entry))))

(defn fill-entries
  [entries evidence]
  (reduce (fn [filled entry]
            (conj filled (fill-entry entry evidence)))
          #{}
          entries))

(defn fill
  "Return the table filled by doing membership queries on columns where
  the concatenation of the entry's path and the evidence have not yet
  been evaluated."
  [table]
  (-> table
      (assoc :S (fill-entries (:S table) (:E table)))
      (assoc :R (fill-entries (:R table) (:E table)))))

(defn make-table
  "Create an empty observation table."
  []
  {:S #{{:path [] :row []}}
   :R #{}
   :E [[]]})

(defn add-r
  "Add a single row into the R section of `table`. Calls `fill` after adding."
  [table r]
  (let [prefixes (paths/prefixes r)
        s-paths (into #{} (map :path (:S table)))
        new-table (reduce (fn [new-table prefix]
                            (if (contains? s-paths prefix)
                              new-table
                              (update new-table :R #(conj % {:path prefix :row []}))))
                          table
                          prefixes)]
    (fill new-table)))

(defn add-rs
  "Returns `table` after every path in `rs` has been added to R."
  [table rs]
  (reduce (fn [new-table {:keys [path]}]
            (add-r new-table path))
          table
          rs))

(defn init-table
  "Add an initial counter example from the database into the
  observation table."
  [table db]
  (let [follow-paths (paths/follow-paths [] db)
        new-table (reduce (fn [new-table r]
                            (add-r new-table (:path r)))
                          table
                          follow-paths)]
    (fill new-table)))

(defn process-ce
  "Return `table` after adding `path` to R. Calls `fill` after promoting."
  [table {:keys [path]}]
  (-> table
      (add-r path)
      (fill)))

(defn promote
  "Return `table` after moving `path` from R to S. Calls `fill` after promoting."
  [table path]
  (-> table
      (update :R (fn [entries] (vec (filter #(not= (:path %) path) entries))))
      (update :S (fn [entries] (conj entries {:path path :row []})))
      (fill)))

(defn closed?
  "Returns true if all rows patterns in R appear in S."
  [table]
  (let [r-rows (set (map first (group-by :row (:R table))))
        s-rows (set (map first (group-by :row (:S table))))]
    (set/subset? r-rows s-rows)))

(defn get-close-candidates
  "Return a rows from R in `table` that do not appear in S."
  [table]
  (let [r-rows (into #{} (map first (group-by :row (:R table))))
        s-rows (into #{} (map first (group-by :row (:S table))))]
    (if-not (set/subset? r-rows s-rows)
      (let [candidate-rows (set/difference r-rows s-rows)
            candidate-row (first candidate-rows)]
        (set (filter #(= candidate-row (:row %)) (:R table)))))))

(defn close
  "Return `table` after attempting a close operation. If successful, an entry
  from R will be moved to S."
  [table db]
  (let [close-candidate (-> table get-close-candidates first :path)
        follow-candidates (paths/follow-paths close-candidate db)]
    (if (nil? close-candidate)
      table
      (if (empty? follow-candidates)
        (promote table close-candidate)
        (-> table
            (promote close-candidate)
            (add-rs follow-candidates))))))

(defn add-evidence
  "Return `table` with `evidence` added to E. Calls `fill` after adding."
  [table evidence]
  (-> table
      (update :E #(conj % evidence))
      (fill)))

(defn row->entry
  "Given a observation table and a row, return the matching
  entry in S."
  [table row]
  (->> table
       :S
       (filter #(= (:row %) row))
       first))

(defn get-prefix-pairs
  "Given an observation table, produce the proper prefix pairs, such that
  (len u1) == (dec (len u2)). These pairs are used to synthesize transitions."
  [table]
  (let [states (:S table)
        entries (set/union (:R table) (:S table))
        prefixes (reduce (fn [transitions entry]
                           (->> entries
                                (map (juxt identity
                                           (constantly entry)
                                           #(paths/prefix? (:path %) (:path entry))))
                                (filter last)
                                (into [])
                                (conj transitions)))
                         []
                         entries)]
    (->> prefixes
         (mapcat identity)
         (into []))))

(defn pair->transition
  "Return an SFA transition constructed from `prefix-pair`."
  [table prefix-pair]
  (let [[from to] prefix-pair
        from-row (:row from)
        to-row (:row to)]
    {:from (:path (row->entry table from-row))
     :input (last (:path to))
     :to (:path (row->entry table to-row))}))

(defn pairs->transitions
  "Transform pairs of rows from `prefix-pairs` into SFA transitions."
  [table prefix-pairs]
  (->> (for [[from to] prefix-pairs]
         (if (= from to)
           []
           (pair->transition table [from to])))
       (filter #(not= nil (:input %)))
       vec))

(defn get-transitions
  "Return the transitions encoded in `table`."
  [table]
  (let [states (:S table)
        prefix-pairs (get-prefix-pairs table)
        transitions (pairs->transitions table prefix-pairs)]
    transitions))

(defn get-state-map
  "Returns a mapping from paths in S to monotonic natural numbers."
  [table]
  (as-> table $
    (:S $)
    (map first $)
    (map second $)
    (map vector $ (iterate inc 0))
    (into {} $)))

(defn get-final-states
  "Returns a vector of final states, labeled as per `get-state-map`."
  [table state-map]
  (->> (:S table)
       (filter #(first (:row %)))
       (map :path)
       (map (partial get state-map))
       vec))

(defn build-sfa
  "Returns an SFA constructed from `table`. If information in `table` is not
  sufficient to construct a complete SFA, the incomplete SFA will be artificially
  completed."
  [table]
  (let [transitions (get-transitions table)
        state-map (get-state-map table)
        final-states (into #{} (get-final-states table state-map))
        simple-transitions (reduce (fn [steps transition]
                                     (conj steps {:from (get state-map (:from transition))
                                                  :input (:input transition)
                                                  :to (get state-map (:to transition))}))
                                   #{}
                                   transitions)
        sfa {:transitions (group-by :from simple-transitions)
             :initial-state (get state-map [])
             :final-states final-states
             :states (set (vals state-map))}]
    (if-not (sfa/complete? sfa)
      (sfa/complete sfa)
      sfa)))

(defn execute-sfa
  "Takes an SFA and a vector of input, and returns the acceptance status of the
  SFA after the given run of input."
  [sfa input]
  (loop [state (:initial-state sfa)
         input input]
    (if (empty? input)
      (contains? (:final-states sfa) state)
      (let [trans (first (filter #(ranges/intersects? (:input %) [(first input) (first input)])
                                 (get (:transitions sfa) state)))]
        (recur (:to trans) (rest input))))))

(defn row-equivalent?
  "Return true if `evidence` does not expose a new row in `table`, false otherwise."
  [table evidence]
  (let [table-with-evidence (add-evidence table evidence)
        old-s-rows (set (map :row (:S table)))
        old-r-rows (set (map :row (:R table)))
        old-rows (set/union old-s-rows old-r-rows)
        new-s-rows (set (map :row (:S table-with-evidence)))
        new-r-rows (set (map :row (:R table-with-evidence)))
        new-rows (set/union new-s-rows new-r-rows)]
    (= (count new-rows) (count old-rows))))

(defn run-all-from-db
  "Run every input from `db` through `sfa` and compare to the expected result.
  Returns true if all results match expected."
  [sfa db]
  (let [paths (paths/sorted-paths db)]
    (loop [paths paths]
      (if (empty? paths)
        true
        (let [path (first paths)
              should-accept (:accepted path)
              input (make-concrete (:path path))
              accepted (execute-sfa sfa input)]
          (if (= accepted should-accept)
            (recur (rest paths))
            path))))))


(defn make-evidences
  "Convert `path` and its suffixes into a concrete inputs."
  [path]
  (vec (map #(vec (concat [:c] %)) (paths/suffixes (map first path)))))

(defn apply-evidences
  "Apply each piece of evidence in `evidences` to `table`. If the evidence does
  results in a row equivalent table, discard the evidence."
  [table evidences]
  (reduce
   (fn [new-table evidence]
     (if-not (row-equivalent? new-table evidence)
       (add-evidence new-table evidence)
       new-table))
   table
   evidences))

(defn longest-matching-prefix
  "Given a CE and a database, get the shortest prefix of the CE in the database"
  [db ce-path]
  (last
   (sort (fn [p1 p2] (< (count (:path p1)) (count (:path p2))))
         (filter (fn [{:keys [path]}]
                   (let [n (count path)
                         shorter (< n (count ce-path))]
                     (if shorter
                       (let [prefix (take n ce-path)
                             is-prefix-of (= prefix path)]
                         is-prefix-of))))
                 db))))

(defn suffix-difference
  "Given a CE and a path, get the suffix that differentiates them."
  [ce-path path]
  (let [n (count path)]
    (->> ce-path
         reverse
         (take n)
         reverse
         vec)))

(defn- expand-node
  "Takes a node and returns all the children of that node.
  Each child has the transition that generated them as a field."
  [trans-table node]
  (reduce (fn [children trans]
            (let [child-parent (conj (:parent node) (:input trans))
                  child-label (:to trans)
                  ?artificial (:artificial trans)
                  child {:parent child-parent
                         :label child-label}]
              (if ?artificial
                (conj children (assoc child :artificial true))
                (conj children child))))
          []
          (get trans-table (:label node))))

(defn- depth-limited-search
  "Takes an SFA and generates all the words of length N "
  [node n tt]
  (if-not (>= 0 n)
    (let [children (expand-node tt node)]
      (for [child children]
        (depth-limited-search child (dec n) tt)))
    node))

(defn- induce-words
  "Driver function for `depth-limited-search.`"
  [sfa n]
  (let [root {:parent []
              :label (:initial-state sfa)}
        tt (:transitions sfa)]
    (flatten (depth-limited-search root n tt))))

(defn make-queries
  "Return a database of inputs and expected outputs of `sfa`, length limited by `depth`.
  If the database contains inputs generated with artificial paths, return only those
  inputs, otherwise return the full set of inputs."
  [sfa depth]
  (loop [depth depth
         queries []]
    (if (>= depth 0)
      (let [words (induce-words sfa depth)
            new-queries (reduce (fn [queries word]
                                  (let [accept (contains? (:final-states sfa) (:label word))
                                        path (:parent word)
                                        ?artificial (:artificial word)]
                                    (if ?artificial
                                      (conj queries {:accepted accept, :path path, :artificial true})
                                      (conj queries {:accepted accept, :path path}))))
                                []
                                words)]
        (recur (dec depth) (conj queries new-queries)))
      (let [artificial-queries (->> queries
                                    flatten
                                    (filter :artificial))] ;; we only care about paths that have been generated using artificial input
        (if (= (count artificial-queries) 0) ;; if no artifical queries, run the full base set
          (flatten queries)
          artificial-queries)))))

(defn refine-path
  "Return the exact constraints along `path` by invoking a Coastal diver."
  [path]
  (if (= path [])
    []
    (let [input (str/join " " (map first path))]

      (wcar* (car/del "refined")
             (car/set "refine" input))

      (while (not= 1 (wcar* (car/exists "refined"))))

      (let [refined-path (tufte/p ::refine-path (wcar* (car/get "refined")))]
        (wcar* (car/del "refined"))
        (read-string refined-path)))))

(defn check-sfa-paths
  [sfa paths]
  (reduce (fn [critical-paths {:keys [path]}]
            (if (or
                 (contains? (set (map first path)) 2147483648) ;; FIXME nasty hack to deal with cases where the SFA is incomplete
                 (contains? (set (map second path)) -1)
                 (contains? (set (map first path)) -1))
              critical-paths
              (let [refined (refine-path path)
                    accepted (member? (make-concrete refined))]
                (if (not= path refined)
                  (conj critical-paths {:path path, :refined refined, :accepted accepted})
                  critical-paths))))
          []
          paths))

(defn apply-ces-from-sfa
  [table db ces]
  (reduce (fn [[db table] ce]
            (let [{:keys [refined accepted]} ce
                  new-entry {:accepted accepted, :path refined}
                  evidence (make-evidences refined)
                  table-with-ce (process-ce table new-entry)
                  table-with-evidence (tufte/p ::apply-evidences (apply-evidences table-with-ce evidence))]
              [(conj db new-entry) (close table-with-evidence (conj db new-entry))]))
          [db table]
          ces))

(defn learn-with-coastal-dynamic
  "Return an table learnt with seed database `db` and a reverse equivalence
  check up to `depth`. The learnt table has metadata attached describing why
  the learning halted."
  [db depth & opts]
  (pprint opts)
  (let [counter (atom 0)
        prev-table (atom nil)]
    (loop [db db
           table (init-table (make-table) db)]
      (swap! counter inc)
      (if-not (closed? table)
        (recur db (close table db))

        (let [sfa (build-sfa table)
              ce-from-db (run-all-from-db sfa db)
              ces-from-sfa (tufte/p ::check-sfa-paths (check-sfa-paths sfa (tufte/p ::make-queries (make-queries sfa depth))))]
          (cond
            ;; Forward equivalence check
            (map? ce-from-db)
            (let [counter-example (:path ce-from-db)
                  table-with-ce (process-ce table {:path counter-example})
                  evidences (make-evidences #_counter-example (suffix-difference counter-example (longest-matching-prefix db counter-example)))
                  table-with-evidence (apply-evidences table-with-ce evidences)]

              (if (= @prev-table table-with-evidence)
                (with-meta table {:reason :fixed-point, :stage :forward-equivalence, :input ce-from-db})
                (do
                  (reset! prev-table table)
                  (recur db table-with-evidence))))

            ;; Reverse equivalence check
            (not (empty? ces-from-sfa))
            (let [[db' table'] (tufte/p ::apply-ces-from-sfa (apply-ces-from-sfa table db ces-from-sfa))]
              (if (= @prev-table table')
                (with-meta table' {:reason :fixed-point, :stage :backward-equivalence, :inputs ces-from-sfa})
                (do
                  (reset! prev-table table')
                  (recur db' table'))))

            ;; Return the learnt table
            :default
            (with-meta table {:reason :complete})))))))
