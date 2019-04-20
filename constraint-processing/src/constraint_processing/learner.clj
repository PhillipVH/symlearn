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

(defn pred
  [lower upper]
  [lower upper])

(defn make-table
  []
  {:S []
   :R []
   :E []
   :f {:R []
       :S []}})

(defn init-table
  [table db]
  (let [initial-path (second db)
        accepted (= :accept (first initial-path))]
    (-> table
        (update :E #(conj % []))
        (update :S #(conj % []))
        (update-in [:f :S] #(assoc % 0 [0]))
        (update :R #(conj % (second initial-path)))
        (update-in [:f :R] #(assoc % 0 [(if accepted 1 -1)])))))

(defn make-concrete
  [path]
  (for [constraint path]
    (first constraint)))


(defn fill-section
  [table section]
  (loop [table table
         i 0]
    (if (= i (count (section table)))
      table
      (let* [s (nth (section table) i)
             s-concrete (make-concrete s)
             e (nth (:E table) i)
             query (concat s-concrete e)
             is-member (member? (int-arr query))]
        (if is-member
          (recur (update-in table [:f section] #(assoc % i [1])) (inc i))
          (recur (update-in table [:f section] #(assoc % i [-1])) (inc i)))))))

(defn fill
  [table]
  (-> table
      (fill-section :R)
      (fill-section :S)))


;; Usage
(def db (-> ["alt-con-1" "alt-con-2"]
            paths/create-database
            paths/sorted-paths))

(def table (-> (make-table)
               (init-table db)))

(pprint table)

(pprint (fill table))

(pprint (second db))

