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
            [symlearn.time :as time]
            [symlearn.sfa :as sfa]
            [symlearn.evaluation :as evaluation]
            [symlearn.intervals :as i :refer [negate union printable]]))

(set! *warn-on-reflection* true)

;; coastal-lite

(defn init-coastal-lite [oracle-str]
  (log/info "Initializing Coastal Lite")
  (coastal/install-parser! oracle-str)
  (reset! table/target-sfa (sfa/regex->sfa* oracle-str))
  (log/info "Initialized Coastal Lite"))

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

(def unicode (mk-guard \u0000 \uffff))

(defn outgoing [prefix]
  (tufte/p
   ::discover-outgoing-guards
   (loop [explored (negate unicode)
          outgoing-guards #{}]
     (if (= unicode explored)
       outgoing-guards
       (let [next-guard (->> explored
                             negate
                             witness
                             (str prefix)
                             guards+acceptance)
             {:keys [guards] :as out} next-guard
             next-guard (last guards)]
         (when (nil? next-guard)
           (pprint out))
         (recur (union explored next-guard)
                (conj outgoing-guards out #_next-guard)))))))

(defn expand-graph [graph [prefix _ :as origin]]
  (let [guards (outgoing prefix)]
    (reduce (fn [graph {:keys [guards accepted]}]
              (let [discovered-guard (last guards)
                    dest-prefix (str prefix (witness discovered-guard))
                    dest-node [dest-prefix accepted]]
                (add-labeled-edges
                 graph
                 [origin dest-node]
                 discovered-guard)))
            graph
            guards)))

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
              (= unicode guard (label graph parent)))))) ;; guard is unicode

(defn leaf-nodes [graph]
  (reduce (fn [leaf-nodes edge]
            (if (nil? (successors graph (dest edge)))
                (conj leaf-nodes (dest edge))
              leaf-nodes))
          []
          (edges graph)))

(defn accepted [nodes]
  (tufte/p
   ::filter-accepted-nodes
   (filter (fn [[_ accepted]] accepted) nodes)))

(defn graph->table
  "Returns a new table, with accepting nodes of `g` added."
  [g]
  (tufte/p
   ::graph->table
   (reduce (fn [table [word _]]
             (table/process-counter-example table word))
           (table/make-table)
           (accepted (nodes g)))))

(defn fixpoint-unroll [bound]
  (loop [depth 0
         graph (initial-graph)
         table nil #_(table/make-table)]
    (log/info "Unrolling to depth " depth)
    (let [graph' (coastal/timeout (time/m->ms 5) #(unroll depth {:prune-fn fischer-prune}))]
      (if (keyword? graph')
        (do
          (log/info "Unroll operation timed out after 5 minutes")
          {:depth depth
           :graph graph
           :table table
           :equivalent false
           :timeout :unroll})
        (let [nodes (nodes graph')
              edges (edges graph')
              _ (log/info "Node count:" (count nodes)", Accepted Nodes:" (count (accepted nodes))", Edge Count: " (count edges))
              table' (coastal/timeout (time/m->ms 5) #(graph->table graph'))]
          (if (keyword? table')
            (do
              (log/info "Graph to table conversion timed out after 5 minutes")
              {:depth depth
               :table table
               :equivalent false
               :graph graph'
               :timeout :graph})
            (let [hypothesis (sfa/make-sfa table')
                  equivalent (sfa/equivalent? hypothesis
                                              @table/target-sfa)
                  ce (coastal/check-equivalence-perfect {:oracle @table/target-sfa
                                                         :hypothesis hypothesis})]
              (if (or equivalent
                   (= bound depth))
                {:depth depth
                 :table table'
                 :equivalent equivalent
                 :ce (if ce (count (first ce)) -1)
                 :graph graph'}
                (recur (inc depth) graph' table')))))))))

;; profiling

(defn profile-fixpoint-unroll []
  (tufte/add-basic-println-handler! {})
  (let [bench (evaluation/load-benchmark "regexlib-80%.re")
        results-file "results.edn"]
    (spit results-file "[\n")
    (doseq [[idx specimen] (map-indexed #(vec [%1 %2]) bench)]
      (try
        (log/info "("idx"): Learning" specimen)
        (Thread/sleep 1000)
        (init-coastal-lite specimen)
        (Thread/sleep 1000) ;; we start querying the oracle too quickly
        (let [[report pstats] (tufte/profiled {} (fixpoint-unroll 30))
              nodes (nodes (:graph report))
              edges (edges (:graph report))
              report+pstats (-> report
                                (dissoc :graph)
                                #_(assoc :skeleton-graph {:nodes nodes
                                                        :edges edges})
                                (assoc :pstats @pstats)
                                (assoc :target specimen))]
          (if (:timeout report)
            (do
              (coastal/stop!)
              (log/info "Timeout"))
            (let [{:keys [depth equivalent]} report]
              (log/info "Depth" depth)
              (log/info "Equivalent " equivalent)))

          (spit results-file (pr-str report+pstats) :append true)
          (spit results-file "\n" :append true))
        (catch Exception e (log/error e))))
    (spit results-file "]" :append true)))

(defn load-profile-reports []
  (read-string (slurp "results.edn")))

(defn -main
  []
  (profile-fixpoint-unroll)
  (coastal/stop!)
  )

;; fixpoint unrolling fn
;; - unroll until sfa/equivalent?
;; - obtain conjecture -> need to validate (eqv oracle => coastal | gtestr | perfect)
;; - regexlib perfect

;; run negative examples after learnt from positive examples
