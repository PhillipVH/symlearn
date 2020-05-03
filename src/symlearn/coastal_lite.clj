(ns symlearn.coastal-lite
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [loom.graph :refer [digraph edges nodes dest src successors]]
            [loom.io :refer [view]]
            [loom.label :refer [add-labeled-edges label]]
            [taoensso.tufte :as tufte]
            [taoensso.timbre :as log]
            [symlearn.coastal :as coastal]
            [symlearn.table :as table]
            [symlearn.sfa :as sfa]
            [symlearn.evaluation :as evaluation]
            [symlearn.intervals :as i :refer [negate union printable]]))

;; coastal-lite

(defn init-coastal-lite [oracle-str]
  (coastal/install-parser! oracle-str)
  (reset! table/target-sfa (sfa/regex->sfa* oracle-str)))

(defn guards [word]
  (let [{:keys [constraints]} (coastal/query word)
        guards (map i/constraint-set->CharPred constraints)]
    guards))

(defn guards+acceptance [word]
  (let [{:keys [constraints accepted]} (coastal/query word)
        guards (map i/constraint-set->CharPred constraints)]
    {:guards guards
     :accepted accepted}))

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
                              guards+acceptance)
              {:keys [guards] :as out} next-guard
              next-guard (last guards)]
          (recur (union explored next-guard)
                 (conj outgoing-guards out #_next-guard))))))

(defn expand-graph [graph [prefix _ :as origin]]
  (tufte/p
   ::expand-prefix
   (let [guards (outgoing prefix)]
     (reduce (fn [graph {:keys [guards accepted]}]
               (let [dest-prefix (str prefix (witness (last guards)))
                     dest-node [dest-prefix accepted]]
                 (add-labeled-edges
                  graph
                  [origin dest-node] guards)))
             graph
             guards))))

(defn epsilon []
  ["" (:accepted (coastal/query ""))])

(defn initial-graph []
  (expand-graph (digraph) (epsilon)))

(defn frontier [graph & [{:keys [prune-fn] :or {prune-fn (constantly true)}}]]
  (tufte/p
   ::select-frontier
   (let [edges (edges graph)
         edges+length (map (juxt (comp count first dest) identity) edges)
         grouped-by (group-by first edges+length)
         frontier-index (apply max (keys grouped-by))
         frontier (get grouped-by frontier-index)
         leaf-edges (map (fn [[_ edge]] edge) frontier)]
     (filter #(prune-fn graph %) leaf-edges))))

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
  (tufte/p
   ::find-edge
   (first
    (filter (fn [edge]
              (let [[src-prefix _] (src edge)
                    [dest-prefix _] (dest edge)]
                (and (= src-node src-prefix)
                     (= dest-node dest-prefix))))
            (edges graph)))))

(def find-edge (memoize find-edge*))

(defn prefix [word]
  (let [n (count word)]
    (if (= 0 n)
      word
      (subs word 0 (dec n)))))

(defn fischer-prune [graph edge]
  (let [guard (label graph edge)
        [src-prefix _] (src edge)
        parent (find-edge* graph [(prefix src-prefix) src-prefix])]
    (not (and parent ;; parent exists and
              (= (unicode) guard (label graph parent)))))) ;; guard is unicode

(defn leaf-nodes [graph]
  (reduce (fn [leaf-nodes edge]
            (if (nil? (successors graph (dest edge)))
                (conj leaf-nodes (dest edge))
              leaf-nodes))
          []
          (edges graph)))

(defn accepted-nodes [nodes]
  (tufte/p
   ::filter-accepted-nodes
   (filter (fn [[_ accepted]] accepted) nodes)))

(defn fixpoint-unroll [bound]
  (loop [depth 0
         table (table/make-table)]
    (log/info "Unrolling to depth " depth)
    (let [table' (reduce (fn [table [word _]]
                           (table/process-counter-example table word))
                         table
                         (accepted-nodes (nodes (unroll depth {:prune-fn fischer-prune}))))
          equivalent (sfa/equivalent? (sfa/make-sfa table')
                                      @table/target-sfa)]
      (if (or equivalent (= bound depth))
        {:depth depth
         :table table
         :equivalent equivalent}
        (recur (inc depth) (table/make-table) #_table')))))

(defn profile-unroll []
  (tufte/add-basic-println-handler! {})
  (let [bench (evaluation/load-benchmark "regexlib-stratified.re")
        specimen (nth bench 30)]
    (init-coastal-lite specimen))
  (Thread/sleep 1000) ;; we start querying the oracle too quickly
  (doseq [depth (range 5)]
    (println "Unrolling to depth" depth)
    (tufte/profile
     {}
     (let [graph (unroll depth {:prune-fn fischer-prune})
           nodes (nodes graph)]
       (println (count nodes))
       (println (count (accepted-nodes nodes)))))))

(defn profile-fixpoint-unroll []
  (tufte/add-basic-println-handler! {})
  (let [bench (evaluation/load-benchmark "regexlib-stratified.re")]
    (doseq [specimen bench]
      (init-coastal-lite specimen)
      (Thread/sleep 1000) ;; we start querying the oracle too quickly
      (println "Learning " specimen)
      (tufte/profile
       {}
       (let [{:keys [depth equivalent]} (fixpoint-unroll 7)]
         (println "Depth" depth)
         (println "Equivalent " equivalent))))))

(defn -main
  []
  (profile-fixpoint-unroll))

(comment

  (tufte/add-basic-println-handler! {})

  (def bench (evaluation/load-benchmark "regexlib-stratified.re"))

  (nth bench 30)

  (init-coastal-lite (nth bench 30))

  (def x (tufte/profile {} (unroll 5 {:prune-fn fischer-prune})))

  ;; (filter accepted (leaf-nodes x))

  (doseq [target bench]
    (init-coastal-lite target)
    (Thread/sleep 2000)
    (let [{:keys [depth equivalent]} (fixpoint-unroll 10)]
      (println (format "Depth: %d, Equivalent: %s"  depth equivalent))))
)

;; fixpoint unrolling fn
;; - unroll until sfa/equivalent?
;; - obtain conjecture -> need to validate (eqv oracle => coastal | gtestr | perfect)
;; - regexlib perfect

;; run negative examples after learnt from positive examples
