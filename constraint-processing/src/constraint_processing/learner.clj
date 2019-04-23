(ns constraint-processing.learner
  (:require [constraint-processing.core :as paths]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set])
  (:import Parser
           TacasParser))

(-> #{}
    (conj [:path [2 3]])
    (conj [:path [2 3]]))

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
  [table]
  (-> table
      (assoc :S (fill-entries (:S table) (:E table)))
      (assoc :R (fill-entries (:R table) (:E table)))))

(defn make-table
  "Create an empty observation table."
  []
  {:S #{ {:path [] :row []} }
   :R #{}
   :E [[]]})

(defn init-table
  "Add an initial counter example from the database into the
  observation table."
  [table db]
  (let [initial-path (nth db 3)
        accepted (:accepted initial-path)]
    (-> table
        (update :R #(conj % {:path (:path initial-path) :row []}))
        (fill))))

(defn process-ce
  [table ce]
  (-> table
      (add-r (:path ce))
      ;; (update :R #(conj % {:path (:path ce) :row []}))
      (fill)))

(defn promote
  [table path]
  (-> table
      (update :R (fn [entries] (into [] (filter #(not= (:path %) path) entries))))
      (update :S (fn [entries] (conj entries {:path path :row []})))
      (fill)))

(defn add-r
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
    ;; (-> table
    ;;    (update :R #(conj % {:path (:path r) :row []}))
    ;;    (fill))))


(defn closed?
  [table]
  (let [r-rows (into #{} (map first (group-by :row (:R table))))
        s-rows (into #{}(map first (group-by :row (:S table))))]
    (if (set/subset? r-rows s-rows)
      (println "Closed")
      (println "Not closed" (set/difference r-rows s-rows)))
    table))

(defn add-evidence
  [table evidence]
  (-> table
      (update :E #(conj % evidence))
      (fill)))

(defn close
  [table path db]
  (let [follow-candidates (paths/follow-paths path db)]
    (if (empty? follow-candidates)
      table
      (-> table
          (promote path)
          (add-r (:path (rand-nth follow-candidates)))))))

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

(defn conjecture
  "Given a closed and consistent observation table, will produce
  an EDN data structure that represents the SFA."
  [table]
  (let [states (:S table)
        prefix-pairs (get-prefix-pairs table)
        transitions (pairs->transitions table prefix-pairs)]
    transitions))

;; Usage
(def tacas-files ["constraints-depth-1"
                  "constraints-depth-2"
                  "constraints-depth-3"
                  "constraints-depth-4"
                  "constraints-depth-5"
                  "constraints-depth-6"
                  "constraints-depth-7"])

(def short-files ["alt-con-1"
                  "alt-con-2"])

(def db (-> tacas-files
            paths/create-database
            paths/sorted-paths))

(defn sprint
  [arg message]
  (do
    (println "----------------")
    (println message)
    (println "----------------")
    (pprint arg)
    arg))


(-> (make-table)
    (init-table db)
    (sprint "1. Table initialized")

    (process-ce (first (paths/follow-paths [] db)))
    (sprint "2. Processed CE")

    (closed?)
    (close [[0 20]] db)

    (sprint "Before accept")

    ;; (process-ce (paths/query [[0 20] [25 30] [0 10]] :exact db))

    (sprint "Added accept")


    (conjecture)

    (pprint))


[:todo-list
 [:h "Automatic detection of non-determinism in the table."]
 [:h "Implementation of `fill` using `promote` and `add-r`." :done]
 [:h "Automatic closing of table, including the addition of a new entry in R."
  :desc "For this we need to figure out an ergonomic way of getting new information from the database component, including filtering for 'smarter', shorter paths."]
 [:h "`add-r` needs to add prefixes as well." :done]]
