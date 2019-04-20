(ns constraint-processing.learner
  (:require [constraint-processing.core :as paths]
            [clojure.pprint :refer [pprint]])
  (:import Parser))


(defn int-arr
  [list]
  (into-array Integer/TYPE list))


(defn member?
  [w]
  (Parser/parse (int-arr w)))


(defn make-table
  []
  {:S [[[] nil]]
   :R []
   :E [[]]})


(defn init-table
  [table db]
  (let [initial-path (second db)
        accepted (= :accept (first initial-path))]
    (-> table
        (update :R #(conj % [(second initial-path) accepted])))))


(defn row
  [table path]
  (->> (:R table)
       (filter #(= path (first %)))
       (map second)))


(defn make-concrete
  [path]
  (for [constraint path]
    (first constraint)))


(defn fill-cell
  [cell evidence]
  (let [filled (not= nil (second cell))
        path (first cell)]
    (if-not filled
      (let [query (concat (make-concrete path) evidence)]
        (assoc cell 1 (member? query)))
      cell)))


(defn fill-section
  [section evidence]
  (reduce (fn [result e] (conj result (into [] (mapcat #(fill-cell % e) section))))
          []
          evidence))


(defn fill
  [table]
  (-> table
      (update :S #(fill-section % (:E table)))
      (update :R #(fill-section % (:E table)))))

;; Usage
(def db (-> ["alt-con-1" "alt-con-2"]
            paths/create-database
            paths/sorted-paths))

(def table (-> (make-table)
               (init-table db)))


(-> (make-table)
    (init-table db)
    (fill))

