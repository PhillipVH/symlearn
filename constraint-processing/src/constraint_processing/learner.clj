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


(def table {:S [{:path [[]], :row [nil]}],
            :R [{:path [[0 0]], :row []} {:path [[1 1]], :row []}],
            :E [[] [:c 4] [:c 4 2]]})


(defn fill
  [table]
  (-> table
      (assoc :S (fill-entries (:S table) (:E table)))
      (assoc :R (fill-entries (:R table) (:E table)))))


(defn process-ce
  [table ce]
  (-> table
      (update :R #(conj % {:path (second ce) :row [nil]} ))))

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

(nth (identity db) 2)


(-> (make-table)
    (init-table db)
    (fill)
    (process-ce (nth db 1))
    (fill)
    (add-evidence [:c 0 1])
    (fill))

