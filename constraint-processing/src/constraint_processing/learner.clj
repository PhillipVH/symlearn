(ns constraint-processing.learner
  (:require [constraint-processing.core :as paths]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.shell :refer [sh]]
            [clojure.walk :as walk]
            [taoensso.carmine :as car :refer [wcar]])
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
  (TacasParser/parse (int-arr w)))

(defn check-membership
  "Takes a path condition and a seq of evidence. Returns
  an ordered seq of membership query results."
  [path evidence]
  (map #(member? (mixed->concrete (conj (into [] path) %))) evidence))

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
      (update :R (fn [entries] (into [] (filter #(not= (:path %) path) entries))))
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

(defn add-rs
  [table rs]
  (reduce (fn [new-table r]
            (add-r new-table (:path r)))
          table
          rs))

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
                                   transitions)]
    {:transitions (group-by :from simple-transitions)
     :initial-state (get state-map [])
     :final-states final-states}))

(defn intersects?
  "Given two constraint pairs, determine if the first intersects the second."
  [[x1 x2] [y1 y2]]
  (and
   (<= x1 y2)
   (<= y1 x2)))

(defn intersection
  "Given two constraint pairs, determine the intersection."
  [[[x1 x2] [y1 y2]]]
  [(max x1 y1) (max x2 y2)])

(defn execute-sfa
  "Takes an SFA and a vector of input, and returns the acceptance status of the
  SFA after the given run of input."
  [sfa input]
  (loop [state (:initial-state sfa)
         input input]
    (if (empty? input)
      (contains? (:final-states sfa) state)
      (let [trans (first (filter #(intersects? (:input %) [(first input) (first input)])
                                 (get (:transitions sfa) state)))]
        (recur (:to trans) (rest input))))))

#_(defn get-from-to-pairs
  [table]
  (let [transitions (-> (build-sfa table) :transitions vals)
        f-transitions (mapcat identity transitions)
        from-to-pairs (->> f-transitions (group-by (juxt :from :to)))]
    from-to-pairs))

(defn get-intersecting-pairs
  [from-to-pairs]
  (for [pair from-to-pairs]
    (for [transition (first (drop 1 pair))]
      (reduce (fn [intersections other-transition]
                (cond
                  (= transition other-transition)
                  intersections

                  (intersects? (:input transition) (:input other-transition))
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

(defn sfa->dot
  "Given some SFA, generate the dot code that will draw
  the automaton."
  [sfa]
  (let [header "digraph finite_state_machine { \nrankdir=LR; size=\"8.5\"\n "
        final-state-style (str "node [ shape = doublecircle]; " (str/join "; " (:final-states sfa)) ";\n")
        initial-state-style (str "node [ shape = point ]; qi;\n")
        normal-state-style (str "node [ shape = circle ];\n")
        first-transition (str "qi -> " (:initial-state sfa) ";\n")
        transitions (reduce
                     (fn [trans-string {:keys [from to input]}]
                       (let [input (if (= (second input) Integer/MAX_VALUE) [(first input) "∞"] input)]
                         (str trans-string "\n" from " -> " to " [ label = \"[" (first input) " " (second input) "]\"];")))
                     ""
                     (apply concat (map second (filter map-entry? (:transitions sfa)))))
        footer "\n}"]
    (-> header
        (str final-state-style)
        (str initial-state-style)
        (str normal-state-style)
        (str first-transition)
        (str transitions)
        (str footer))))

(defn sfa->img
  "Take an SFA and generate an image from it, using `sfa->dot`."
  [sfa]
  (spit "tmp.dot" (sfa->dot sfa))
  (sh/sh "dot"
         (str "-Gdpi=" 300)
         (str "-T" "png")
         "tmp.dot"
         "-o"
         "tmp.png")
  (sh/sh "xdg-open" "tmp.png")
  (sh/sh "rm" "tmp.png")
  (sh/sh "rm" "tmp.dot"))

(defn make-evidences
  [path]
  (map #(concat [:c] %) (paths/suffixes (map first path))))

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
              path (into [] (concat [:c] counter-example))
              table-with-ce (process-ce table {:path counter-example})
              evidences (make-evidences (drop 1 path))
              table-with-evidence (apply-evidences table-with-ce evidences)]
          (do
            (println "----" @counter "----")
            (println "Added Counter Example & Evidence")
            (println counter-example)
            (safe-dot (build-sfa table) "tacas" @counter)
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
          (safe-dot (build-sfa table) "tacas" @counter)
          (pprint (build-sfa table)))))))


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
  (let [counter (atom 0)]
    (loop [table (init-table (make-table) db)]
      (swap! counter inc)
      (cond
        ;; First handle the case in which the table is not closed
        (not (closed? table))
        (do
          (println "----" @counter "----")
          (println "Closed Table")
          (pprint table)
          (recur (close table db)))

        ;; Do an equivalence query and handle the evidence prefix addition
        (map? (run-all-from-db (build-sfa table) db))
        (let [counter-example (:path (run-all-from-db (build-sfa table) db))
              path (into [] (concat [:c] counter-example))
              table-with-ce (process-ce table {:path counter-example})
              ;; evidences (make-evidences (drop 1 path))
              evidences (make-evidences (suffix-difference (drop 1 path) (longest-matching-prefix db (drop 1 path))))
              table-with-evidence (apply-evidences table-with-ce evidences)]
          (do
            (println "----" @counter "----")
            (println "Added Counter Example & Evidence (Forward)")
            (pprint counter-example)
            (safe-dot (build-sfa table-with-evidence) "tacas" @counter)
            ;; (pprint table-with-evidence)
            (recur table-with-evidence)))

        ;; Do a reverse equivalence check
        (map? (run-all-from-sfa (build-sfa table) (make-queries (build-sfa table) 7)))
        (let [counter-example (:path (run-all-from-sfa (build-sfa table) (make-queries (build-sfa table) 7)))]
          (do
            (println "----" @counter "----")
            (println "Added Counter Example & Evidence (Backward)")
            (pprint counter-example)
            (let [refined-counter (refine-path (str/join " " (map first counter-example)))
                  ;; refined-counter (read-string (wcar* (car/get "refined")))
                  refined-evidences (make-evidences (suffix-difference refined-counter (longest-matching-prefix db refined-counter)))
                  table-with-ce (process-ce table {:path refined-counter})
                  table-with-refined-ev (apply-evidences table-with-ce refined-evidences)]
              ;; (wcar* (car/del "refined"))
              (pprint refined-counter)

              (safe-dot (build-sfa table-with-refined-ev) "tacas" @counter)
              (pprint table-with-refined-ev)
              (recur table-with-refined-ev))))

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
          (safe-dot (build-sfa table) "tacas" @counter)
          #_(build-sfa table)
          table)))))
