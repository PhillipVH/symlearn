(ns constraint-processing.learner
  (:require [constraint-processing.core :as paths]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set])
  (:import Parser))

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
  (Parser/parse (int-arr w)))

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
          []
          entries))

(defn fill
  [table]
  (-> table
      (assoc :S (fill-entries (:S table) (:E table)))
      (assoc :R (fill-entries (:R table) (:E table)))))

(defn make-table
  "Create an empty observation table."
  []
  {:S #{ {:path [[]] :row []} }
   :R #{ {:path [] :row []} }
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
      (update :R #(conj % {:path (:path ce) :row []}))
      (fill)))

(defn promote
  [table path]
  (-> table
      (update :R (fn [entries] (into [] (filter #(not= (:path %) path) entries))))
      (update :S (fn [entries] (conj entries {:path path :row []})))
      (fill)))

(defn add-r
  [table r]
  (-> table
      (update :R #(conj % {:path (:path r) :row []}))
      (fill)))

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

(def db (-> short-files
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

    (process-ce (nth db 2))
    (sprint (str "2. Processed CE " (nth db 2)))

    (promote [[1 1]])
    (add-r (-> (paths/query [[1 1]] :starts-with db) second))
    (sprint "3. Close path [[1 1]]")


    (add-r (-> (paths/query [[2 Integer/MAX_VALUE]] :starts-with db) second))

    (sprint "4. Added [[2 Inf] x]")

    (add-evidence [:c 1])

    (sprint "4. Added evidence [0 1 2]")

    (closed?)

    (promote [[2 Integer/MAX_VALUE]])

    (sprint "5. Promoted")

    (closed?)

    (conjecture))


(defn conjecture
  "Given a closed and consistent observation table, will produce
  an EDN data structure that represents the SFA."
  [table]
  (let [states (:S table)]
    (reduce (fn [transitions entry]
              (conj transitions (map (partial paths/prefix? (:path entry)) (:path states))))
            []
            (:R table))))

