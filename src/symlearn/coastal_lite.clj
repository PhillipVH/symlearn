(ns symlearn.coastal-lite
  (:require [clojure.string :as str]
            [loom.graph :refer [digraph edges dest src successors]]
            [loom.io :refer [view]]
            [loom.label :refer [add-labeled-edges label]]
            [symlearn.coastal :as coastal]
            [symlearn.intervals :as i :refer [negate union printable]]))

;; coastal-lite

(defn init-coastal-lite [oracle]
  (coastal/install-parser! oracle))

(defn guards [word]
  (let [{:keys [constraints]} (coastal/query word)
        guards (map i/constraint-set->CharPred constraints)]
    guards))

(defn witness [guard]
  (if (seq? guard)
    (str/join (map i/left guard))
    (str (i/left guard))))

(defn mk-guard
  ([bound] (mk-guard bound bound))
  ([lower upper] (i/make-interval lower upper)))

(defn unicode []
  (mk-guard \u0000 \uffff))

(defn outgoing [prefix]
    (loop [explored (negate (unicode))
           outgoing-guards #{}]
      (if (= (unicode) explored)
        outgoing-guards
        (let [next-guard (->> explored
                              negate
                              witness
                              (str prefix)
                              guards
                              last)]
          (recur (union explored next-guard)
                 (conj outgoing-guards next-guard))))))

(defn expand-graph [graph prefix]
  (let [guards (outgoing prefix)]
    (reduce (fn [graph guard]
              (add-labeled-edges graph [prefix (str prefix (witness (printable guard)))] guard))
            graph
            guards)))

(defn initial-graph []
  (expand-graph (digraph) ""))

(defn frontier [graph & [{:keys [prune-fn] :or {prune-fn identity}}]]
  (let [edges (edges graph)
        edges+length (map (juxt (comp count dest) identity) edges)
        grouped-by (group-by first edges+length)
        frontier-index (apply max (keys grouped-by))
        frontier (get grouped-by frontier-index)
        leaf-edges (map (fn [[_ edge]] edge) frontier)]
    (filter #(prune-fn graph %) leaf-edges)))

(defn unroll [steps & [opts]]
  (loop [graph (initial-graph)
         steps steps]
    (if (= 0 steps)
      graph
      (let [frontier-prefixes (map dest (frontier graph opts))
            graph' (reduce (fn [graph' prefix]
                             (expand-graph graph' prefix))
                           graph
                           frontier-prefixes)]
        (recur graph' (dec steps))))))

(defn find-edge* [graph [src-node dest-node]]
  (first
   (filter (fn [edge]
             (and (= src-node (src edge))
                  (= dest-node (dest edge))))
           (edges graph))))

(def find-edge (memoize find-edge*))

(defn prefix [word]
  (let [n (count word)]
    (if (= 0 n)
      word
      (subs word 0 (dec n)))))

(defn fischer-prune [graph edge]
  (let [guard (label graph edge)
        src (src edge)
        parent (find-edge graph [(prefix src) src])]
    (not (and parent
              (= (unicode) guard (label graph parent))))))

(defn leaf-nodes [graph]
  (reduce (fn [leaf-nodes edge]
            (if (nil? (successors graph (dest edge)))
              (conj leaf-nodes (dest edge))
              leaf-nodes))
          []
          (edges graph)))

;; TODO instant automaton, just add leaf nodes!
(comment
  (init-coastal-lite "abc|g+|////")
  (view (unroll 20 {:prune-fn fischer-prune}))
  (leaf-nodes (unroll 20 {:prune-fn fischer-prune}))
  )
