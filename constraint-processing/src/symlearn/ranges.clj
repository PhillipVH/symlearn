(ns symlearn.ranges
  "Utilities to facilitate reasoning over ranges (of non-negative integers).")

(defn adjacent
  "Returns true if two ranges are adjacent."
  [r1 r2]
  (= (inc (second r1)) (first r2)))

(defn starting-with
  "Returns the ranges in the coll that start with the given number."
  [ranges start]
  (first (filter #(= (first %) start) ranges)))

(defn grow
  "Returns one range resulting from merging two adjacent, disjointed ranges."
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

(defn range-between
  "Returns the range between r1 and r2 (excluding the last element of r1 and
  the first element of r2), and nil if the ranges are adjacent.

  TODO r1 and r2 intersect, the SFA has non-determinism"
  [r1 r2]
  (if-not (adjacent r1 r2)
    (do
      [(inc (second r1)) (dec (first r2))])))

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
            (recur (grow longest next)))))
      nil)))

(defn get-completing-predicates
  "Return a set of ranges that will complement `ranges` and cover the
  entire domain."
  [ranges]
  (let [preds (sort-by first ranges)
        completing-preds
        (reduce (fn [[prev-range completing-preds] pred]
                  (if-let [new-pred (range-between prev-range pred)]
                    [pred (conj completing-preds new-pred)]
                    [pred completing-preds]))
                [[-1 -1] #{}]
                preds)
        [_ r] (first completing-preds)
        completing-preds (second completing-preds)]
    (if-not (= Integer/MAX_VALUE r)
      (conj completing-preds [(inc r) Integer/MAX_VALUE])
      completing-preds)))
