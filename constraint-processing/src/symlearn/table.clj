(ns symlearn.table
  (:require [taoensso.tufte :as tufte]
            [symlearn.paths :as paths]
            [clojure.set :as set]
            [symlearn.sfa :as sfa]))

(def ^:dynamic *parse-fn*
  "A function wrapper for the underlying parser. Rebound to the correct parser
  before the learning procedure is called."
  #(TacasParser/parse %))

(defn member?
  "Return the result of calling `*parse-fun*` with `input`, true if the parser accepts,
  false if it rejects."
  [input]
  (*parse-fn* (into-array Integer/TYPE input)))

(defn check-membership
  "Takes a path condition and a seq of evidence. Returns
  an ordered seq of membership query results."
  [path evidence]
  (map #(member? (paths/mixed->concrete (conj (vec path) %))) evidence))


(defn- fill-entry
  "Returns `entry` where a membership query has been issued for each piece of `evidence.`"
  [entry evidence]
  (let [{:keys [row path]} entry
        row-length (count row)
        evidence-length (count evidence)]
    (if-not (= row-length evidence-length)
      (tufte/p ::fill-entry (assoc entry :row (vec (check-membership path evidence))))
      (tufte/p ::no-fill-entry entry))))

(defn- fill-entries
  "Returns `entries` where each entry has been filled with `fill-entry`."
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
        s-paths (set (map :path (:S table)))
        new-table (reduce (fn [new-table prefix]
                            (if (contains? s-paths prefix)
                              new-table
                              (update new-table :R #(conj % {:path prefix :row []}))))
                          table
                          prefixes)]
    (fill new-table)))

(defn add-rs
  "Returns `table` after every path in `rs` has been added to R. Indirectly calls `fill`."
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
        new-table (add-rs table follow-paths)]
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
  (let [r-rows (set (map first (group-by :row (:R table))))
        s-rows (set (map first (group-by :row (:S table))))]
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table to SFA conversion functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
         vec)))

(defn find-entry-with-row
  "Return the entry in S of `table` that has a membership row equal to `row`."
  [table row]
  (->> table
       :S
       (filter #(= (:row %) row))
       first))

(defn pair->transition
  "Return an SFA transition constructed from `prefix-pair`."
  [table prefix-pair]
  (let [[from to] prefix-pair
        from-row (:row from)
        to-row (:row to)]
    {:from (:path (find-entry-with-row table from-row))
     :input (last (:path to))
     :to (:path (find-entry-with-row table to-row))}))

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

(defn table->sfa
  "Returns an SFA constructed from `table`. If information in `table` is not
  sufficient to construct a complete SFA, the incomplete SFA will be artificially
  completed."
  [table]
  (let [transitions (get-transitions table)
        state-map (get-state-map table)
        final-states (set (get-final-states table state-map))
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
