(ns symlearn.paths
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn make-concrete
  "Return a seq of the first element of every constraint in `path`."
  [path]
  (map first path))

(defn mixed->concrete
  "Take a (potentially mixed) seq of constraints,
  and return a concrete input that the underlying domain
  parser understands."
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
    (vec concrete)))

(defn sorted-paths
  "Sort inputs in `db` in order of ascending length."
  [db]
  (sort #(compare (count (:path %1))
                  (count (:path %2)))
        db))

(defn prefix?
  "Returns true if `s1` is a prefix of `s2.`"
  [s1 s2]
  (=
   s1
   (->> s2
        (reverse)
        (drop 1)
        (reverse))))

(defn query
  "Searches `db` for a set of inputs that match the given `constraints`
  under a certain `mode`.

  :exact => return inputs that are exactly equal to `path`.
  :starts-with => return inputs that start with `path`.
  :prefixes => return all inputs that are prefixes of `path`."
  [constraints mode db]
  (cond
    (= mode :exact)
    (first (filter #(= (:path %) constraints) db))

    (= mode :starts-with)
    (filter #(and
              (>= (count (:path %)) (count constraints))
              (= (take (count constraints) (:path %)) constraints)) db)

    (= mode :prefixes)
    (mapcat identity
            (for [n (range (count constraints))]
              (filter #(= (:path %) (->> constraints reverse (drop n) reverse)) db)))))

(defn prefixes
  "Return every prefix of `path`, including `path`."
  [path]
  (for [n (range (count path))]
    (->> path reverse (drop n) reverse vec)))

(defn suffixes
  "Return every suffix of `path`, including `path`."
  [path]
  (for [n (range (count path))]
    (->> path (drop n) vec)))

(defn make-evidences
  "Convert `path` and its suffixes into a concrete inputs."
  [path]
  (vec (map #(vec (concat [:c] %)) (suffixes (make-concrete path)))))

(defn follow-paths
  "Return inputs from `db` that have `path` as a suffix and are exactly one
  constraint longer."
  [path db]
  (->> db
       (query path :starts-with)
       (filter #(= (count (:path %)) (inc (count path))))
       vec))
