(ns constraint-processing.learner
  (:require [constraint-processing.core :as paths])
  (:import Parser))

(defn member?
  [w]
  (Parser/parse (int-arr w)))

(defn make-obs-table
  []
  {:S [[]]
   :R [[1]]
   :E [[]]
   :f {:R [[0]]
       :S [[0]]}})

(defn fill
  [table section]
  (loop [table table
         i 0]
    (if (= i (count (section table)))
      table
      (let* [s (nth (section table) i)
             e (nth (:E table) i)
             query (concat s e)
             is-member (member? (int-arr query))]
        (if is-member
          (recur (update-in table [:f section] #(assoc % i [1])) (inc i))
          (recur (update-in table [:f section] #(assoc % i [-1])) (inc i)))))))



(defn int-arr
  [list]
  (into-array Integer/TYPE list))


;; Usage
(def db (paths/create-database ["alt-con-1" "alt-con-2"]))

(def table (make-obs-table))

(fill table :R)

(-> table
    (fill :R)
    (fill :S))


