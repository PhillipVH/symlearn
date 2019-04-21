(ns constraint-processing.learner
  (:require [constraint-processing.core :as paths]
            [clojure.pprint :refer [pprint]])
  (:import Parser))


(defn make-concrete
  [path]
  (for [constraint path]
    (first constraint)))


(defn int-arr
  [list]
  (into-array Integer/TYPE list))


(defn member?
  [w]
  (Parser/parse (int-arr w)))


(defn make-table
  []
  {:S [{:path [[]] :row [nil]}]
   :R [{:path [] :row []}]
   :E [[]]})


(defn init-table
  [table db]
  (let [initial-path (nth db 3)
        accepted (= :accept (first initial-path))]
    (-> table
        (update :R #(conj % {:path (second initial-path) :row []})))))


{:S [{:path [[]], :row [nil]}], :R [{:path [], :row []}], :E [[]]}

(defn fill-row
  [path evidence]
  (map #(member? (apply concat (make-concrete path) %)) evidence))


(defn fill-section
  [section evidence]
  (map #(fill-row (:path %) evidence) section))

(defn fill
  [table]
  (-> table
      (update :S #(fill-section % (:E table)))
      (update :R #(fill-section % (:E table)))))

(defn process-ce
  [table ce]
  (-> table
      (update :R #(conj % [(second ce) (= :accept (first ce))]))))

(defn close
  [table path]
  (let [r (keep #{path} (:R table))]
    (println r)
    (-> table
        (update :R #(remove #{path} %))
        (update :S #(conj % r)))))

(defn add-evidence
  [table evidence]
  (-> table
      (update :E #(conj % evidence))))

;; Usage
(def db (-> ["alt-con-1" "alt-con-2"]
            paths/create-database
            paths/sorted-paths))

(identity db)

(def table (-> (make-table)
               (init-table db)))



(-> (make-table)
    (init-table db))

