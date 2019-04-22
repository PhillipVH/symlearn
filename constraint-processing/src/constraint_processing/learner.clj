(ns constraint-processing.learner
  (:require [constraint-processing.core :as paths]
            [clojure.pprint :refer [pprint]])
  (:import Parser))


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

;; (member? (mixed->concrete [[0 0] [32 40] [:c 1 2 3]]))

;; (identity db)

(defn int-arr
  [list]
  (into-array Integer/TYPE list))


(defn member?
  [w]
  (Parser/parse (int-arr w)))


(defn make-table
  "Create an empty observation table."
  []
  {:S [{:path [[]] :row [nil]}]
   :R [{:path [] :row []}]
   :E [[]]})


(defn init-table
  "Add an initial counter example from the database into the
  observation table."
  [table db]
  (let [initial-path (nth db 3)
        accepted (:accepted initial-path)]
    (-> table
        (update :R #(conj % {:path (:path initial-path) :row []})))))


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


(defn process-ce
  [table ce]
  (-> table
      (update :R #(conj % {:path (:path ce) :row []}))
      (fill)))


(defn close
  [table path]
  (-> table
      (update :R (fn [entries] (into [] (filter #(= (:path %) path) entries))))
      (update :S (fn [entries] (conj entries {:path path :row []})))
      (fill)))


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

(def short-db (-> short-files
                  paths/create-database
                  paths/sorted-paths))

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
    (sprint "Intital table")

    (init-table db)
    (sprint "Table initialized")

    (process-ce (nth db 2))
    (sprint (str "Processed CE " (nth db 2)))

    (close [[1 1]])
    (sprint "Close path [[1 1]]")

    (add-evidence [:c 0 1 2])

    (sprint "Added evidence [0 1 2]"))

