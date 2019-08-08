(ns constraint-processing.ranges
  "Utilities to facilitate reasoning over ranges (of non-negative integers).")

(defn starting-with
  "Returns the ranges in the coll that start with the given number."
  [ranges start]
  (first (filter #(= (first %) start) ranges)))

(defn union
  "Returns the union of two adjacent, disjointed ranges."
  [[x1 y1] [x2 y2]]
  [x1 y2])

(defn intersection
  "Given two constraint pairs, determine the intersection."
  [[[x1 x2] [y1 y2]]]
  [(max x1 y1) (max x2 y2)])

(defn intersects?
  "Given two constraint pairs, determine if the first intersects the second."
  [[x1 x2] [y1 y2]]
  (and
   (<= x1 y2)
   (<= y1 x2)))

(defn get-largest-range
  "Returns the largest contiguous range constructed from a set of disjoint
  ranges, false if the range does not start with zero."
  [ranges]
  (let [initial (starting-with ranges 0)]
    (if-not (empty? initial)
      (loop [longest initial]
        (let [next (starting-with ranges (inc (second longest)))]
          (cond
            ;; no range that follows
            (empty? next)
            longest

            ;; longest ranges
            (= (second longest) Integer/MAX_VALUE)
            longest

            ;; union two ranges
            (= (inc (second longest)) (first next))
            (recur (union longest next)))))
      nil)))

