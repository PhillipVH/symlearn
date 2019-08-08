(ns constraint-processing.learner
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [taoensso.carmine :as car :refer [wcar]]
            [constraint-processing.core :as paths]
            [constraint-processing.sfa :as sfa]
            [constraint-processing.ranges :as ranges])
  (:import Parser
           TacasParser
           SingleParser
           LearnLarge))

(def redis-conn {:pool {} :spec {:host "127.0.0.1" :port 6379}}) ; See `wcar` docstring for opts
(defmacro wcar* [& body] `(car/wcar redis-conn ~@body))

(def inf Integer/MAX_VALUE)

(defn make-concrete
  [path]
  (for [constraint path]
    (first constraint)))

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

(defn int-arr
  [list]
  (into-array Integer/TYPE list))

(defn member?
  [w]
  (TacasParser/parse (int-arr w))
  #_(LearnLarge/parse (int-arr w)))

(defn check-membership
  "Takes a path condition and a seq of evidence. Returns
  an ordered seq of membership query results."
  [path evidence]
  (map #(member? (mixed->concrete (conj (vec path) %))) evidence))

(defn fill-entry
  [entry evidence]
  (assoc entry :row (into [] (check-membership (:path entry) evidence))))

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
  "Add a single row into the R section of the observation table."
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
  [table rs]
  (reduce (fn [new-table r]
            (add-r new-table (:path r)))
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
  [table ce]
  (-> table
      (add-r (:path ce))
      (fill)))

(defn promote
  [table path]
  (-> table
      (update :R (fn [entries] (vec (filter #(not= (:path %) path) entries))))
      (update :S (fn [entries] (conj entries {:path path :row []})))
      (fill)))

(defn closed?
  [table]
  (let [r-rows (into #{} (map first (group-by :row (:R table))))
        s-rows (into #{} (map first (group-by :row (:S table))))]
    (if (set/subset? r-rows s-rows)
      true
      false)))

(defn get-close-candidates
  [table]
  (let [r-rows (into #{} (map first (group-by :row (:R table))))
        s-rows (into #{} (map first (group-by :row (:S table))))]
    (if-not (set/subset? r-rows s-rows)
      (let [candidate-rows (set/difference r-rows s-rows)
            candidate-row (first candidate-rows)]
        (into #{} (filter #(= candidate-row (:row %)) (:R table)))))))

(defn close
  [table db]
  (let [close-candidate (-> table get-close-candidates first :path)
        follow-candidates (paths/follow-paths close-candidate db)]
    (if (empty? follow-candidates)
      (promote table close-candidate)
      (-> table
          (promote close-candidate)
          (add-rs follow-candidates)))))

(defn add-evidence
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
  "Given an observation table, produce the proper prefix pairs,
  such that (len u1) == (- 1 (len u2))."
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
  [table prefix-pair]
  (let [from (first prefix-pair)
        to (second prefix-pair)
        from-row (:row from)
        to-row (:row to)]
    {:from (:path (row->entry table from-row))
     :input (last (:path to))
     :to (:path (row->entry table to-row))}))

(defn pairs->transitions
  [table prefix-pairs]
  (->> (for [pair prefix-pairs]
         (if (= (first pair) (second pair))
           []
           (pair->transition table pair)))
       (filter #(not= nil (:input %)))
       (into [])))

(defn get-transitions
  [table]
  (let [states (:S table)
        prefix-pairs (get-prefix-pairs table)
        transitions (pairs->transitions table prefix-pairs)]
    transitions))

#_(defn get-state-map
  [transitions]
  (as-> transitions $
    (group-by :from $)
    (map first $)
    (map vector $ (iterate inc 0))
    (into {} $)))

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

(defn get-intersecting-pairs
  [from-to-pairs]
  (for [pair from-to-pairs]
    (for [transition (first (drop 1 pair))]
      (reduce (fn [intersections other-transition]
                (cond
                  (= transition other-transition)
                  intersections

                  (ranges/intersects? (:input transition) (:input other-transition))
                  (conj intersections #{transition other-transition})

                  :else
                  intersections))
              []
              (first (drop 1 pair))))))

(defn get-inconsistent-transitions
  [table]
  (let [transitions (-> (build-sfa table) :transitions vals)
        f-transitions (mapcat identity transitions)
        from-to-pairs (->> f-transitions (group-by (juxt :from)))]
    (->> (get-intersecting-pairs from-to-pairs)
         (flatten)
         (apply set/union))))

(defn row-equivalent?
  "Return `true` if adding the new evidence doesn't
  introduce a new unique row, `false` otherwise."
  [table evidence]
  (let [new-table (-> table
                      (add-evidence evidence))
        old-s-rows (map :row (:S table))
        old-r-rows (map :row (:R table))
        s-rows (map :row (:S new-table))
        r-rows (map :row (:R new-table))
        rows (set/union s-rows r-rows)
        old-rows (set/union old-s-rows old-r-rows)]
    (=
     (count (set old-rows))
     (count (set rows)))))

(defn consistent?
  [table]
  (empty? (get-inconsistent-transitions table)))

(defn run-all-from-db
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
  [path]
  (map #(concat [:c] %) (paths/suffixes (map first path))))

(make-evidences [[0 20] [30 40]])

(defn apply-evidences
  [table evidences]
  (reduce
   (fn [new-table evidence]
     (if-not (row-equivalent? new-table evidence)
       (add-evidence new-table evidence)
       new-table))
   table
   evidences))

(defn learn-without-coastal
  [db]
  (let [counter (atom 0)]
    (loop [table (init-table (make-table) db)]
      (swap! counter inc)
      (cond
        ;; First handle the case in which the table is not closed
        (not (closed? table))
        (do
          (println "----" @counter "----")
          (println "Closed Table")
          (pprint (close table db))
          (recur (close table db)))

        ;; Do an equivalence query and handle the evidence prefix addition
        (map? (run-all-from-db (build-sfa table) db))
        (let [counter-example (:path (run-all-from-db (build-sfa table) db))
              path (vec (concat [:c] counter-example))
              table-with-ce (process-ce table {:path counter-example})
              evidences (make-evidences (drop 1 path))
              table-with-evidence (apply-evidences table-with-ce evidences)]
          (do
            (println "----" @counter "----")
            (println "Added Counter Example & Evidence")
            (println counter-example)
            #_(safe-dot (build-sfa table) "tacas" @counter)
            (pprint table-with-evidence)
            (recur table-with-evidence)))

        ;; Uh, are we done?
        :default
        (let [n-rows (count (set/union (:R table) (:S table)))
              n-cols (count (:E table))]
          (println "----" @counter "----")
          (println "Done!")
          (pprint table)
          (println "Number of rows in R: " (count (:R table)))
          (println "Number of rows in S: " (count (:S table)))
          (println "Number of columns in E: " (count (:E table)))
          #_(safe-dot (build-sfa table) "tacas" @counter)
          #_(pprint (build-sfa table))
          table)))))


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
         (into []))))

(defn expand-node
  "Takes a node and returns all the children of that node.
  Each child has the transition that generated them as a field."
  [trans-table node]
  (reduce (fn [children trans]
            (let [child-parent (conj (:parent node) (:input trans))
                  child-label (:to trans)
                  child {:parent child-parent
                         :label child-label}]
              (conj children child)))
          []
          (get trans-table (:label node))))

(defn depth-limited-search
  "Takes an SFA and generates all the words of length N "
  [node n tt]
  (if-not (>= 0 n)
    (let [children (expand-node tt node)]
      (for [child children]
        (depth-limited-search child (dec n) tt)))
    node))

(def dls depth-limited-search #_(memoize depth-limited-search))

(defn induce-words
  [sfa n]
  (let [root {:parent []
              :label (:initial-state sfa)}
        tt (:transitions sfa)]
    (flatten (dls root n tt))))

(defn make-queries
  [sfa depth]
  (loop [depth depth
         queries []]
    (if (>= depth 0)
      (let [words (induce-words sfa depth)
            new-queries (reduce (fn [queries word]
                                  (let [accept (contains? (:final-states sfa) (:label word))
                                        path (:parent word)]
                                    (conj queries {:accepted accept :path path})))
                                []
                                words)]
        (recur (dec depth) (conj queries new-queries)))
      (flatten queries))))

(defn run-all-from-sfa
  [sfa db]
  (let [paths (paths/sorted-paths db)]
    (loop [paths paths]
      (if (empty? paths)
        true
        (let [path (first paths)
              should-accept (:accepted path)
              input (make-concrete (:path path))
              accepted (member? input)]
          (if (= accepted should-accept)
            (recur (rest paths))
            path))))))

(defn refine-path
  [input]
  (wcar* (car/del "refined")
         (car/set "refine" input))

  (while (not= 1 (wcar* (car/exists "refined"))))

  (let [refined-path (wcar* (car/get "refined"))]
    (wcar* (car/del "refined"))
    (read-string refined-path)))

(defn learn-with-coastal
  [db]
  (let [counter (atom 0)
        rev-depth (atom 20)
        prev-table (atom nil)
        table->img #(sfa/sfa->img (build-sfa %))]
    (loop [db db
           table (init-table (make-table) db)]
      (swap! counter inc)
      (cond
        ;; First handle the case in which the table is not closed
        (not (closed? table))
        (let [closed-table (close table db)]
          (recur db (close table db)))

        ;; Do a forward equivalence query and handle the evidence prefix addition
        (map? (run-all-from-db (build-sfa table) db))
        (let [counter-example (:path (run-all-from-db (build-sfa table) db))
              table-with-ce (process-ce table {:path counter-example})
              evidences (make-evidences (suffix-difference counter-example (longest-matching-prefix db counter-example)))
              table-with-evidence (apply-evidences table-with-ce evidences)]
          (recur db table-with-evidence))

        ;; Do a backward equivalence check
        (map? (run-all-from-sfa (build-sfa table) (make-queries (build-sfa table) @rev-depth)))
        (let [counter-example (:path (run-all-from-sfa (build-sfa table) (make-queries (build-sfa table) @rev-depth)))
              should-accept (not (:accepted (run-all-from-sfa (build-sfa table) (make-queries (build-sfa table) @rev-depth))))
              refined (refine-path (str/join " " (map first counter-example)))
              lmp (:path (longest-matching-prefix db refined))
              feasible-refined (vec (take (inc (count lmp)) refined))
              feasible-evidence (make-evidences feasible-refined)
              table-with-ce (process-ce table {:path feasible-refined})
              table-with-refined-evidence (apply-evidences table-with-ce feasible-evidence #_refined-evidences)]
          (println (str "----- Reverse Eqv Query (counter: " @counter ")---------"))
          (pprint counter-example)
          (pprint refined)
          (pprint table-with-refined-evidence)
          (if (= table-with-refined-evidence @prev-table)
            (do
              (println "Fixed point table, terminating")
              table-with-refined-evidence)
            (do
              (reset! prev-table table-with-refined-evidence)
              (recur (paths/sorted-paths (conj db {:accepted should-accept, :path refined})) table-with-refined-evidence))))

        ;; The learnt table
        :default
        (do
          (pprint db)
          (pprint table)
          table)))))
