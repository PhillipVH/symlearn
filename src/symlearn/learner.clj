(ns symlearn.learner
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [taoensso.carmine :as car :refer [wcar]]
            [taoensso.tufte :as tufte]
            [symlearn.paths :as paths]
            [symlearn.sfa :as sfa]
            [symlearn.coastal :as coastal]
            [symlearn.ranges :as ranges]
            [symlearn.table :as table])
  (:import Parser
           TacasParser
           SingleParser
           LearnLarge))

(set! *warn-on-reflection* true)

(defn- expand-node
  "Takes a node and returns all the children of that node.
  Each child has the transition that generated them as a field."
  [trans-table node]
  (reduce (fn [children trans]
            (let [child-parent (conj (:parent node) (:input trans))
                  child-label (:to trans)
                  ?artificial (:artificial trans)
                  child {:parent child-parent
                         :label child-label}]
              (if ?artificial
                (conj children (assoc child :artificial true))
                (conj children child))))
          []
          (get trans-table (:label node))))

(defn- depth-limited-search
  "Takes an SFA and generates all the words of length N "
  [node n tt]
  (if-not (>= 0 n)
    (let [children (expand-node tt node)]
      (for [child children]
        (depth-limited-search child (dec n) tt)))
    node))

(defn- induce-words
  "Driver function for `depth-limited-search.`"
  [sfa n]
  (let [root {:parent []
              :label (:initial-state sfa)}
        tt (:transitions sfa)]
    (flatten (depth-limited-search root n tt))))

(defn- make-queries
  "Return a database of inputs and expected outputs of `sfa`, length limited by `depth`.
  If the database contains inputs generated with artificial paths, return only those
  inputs, otherwise return the full set of inputs."
  [sfa depth]
  (tufte/p
   ::make-queries
   (loop [depth depth
          queries []]
     (if (>= depth 0)
       (let [words (induce-words sfa depth)
             new-queries (reduce (fn [queries word]
                                   (let [accept (contains? (:final-states sfa) (:label word))
                                         path (:parent word)
                                         ?artificial (:artificial word)]
                                     (if ?artificial
                                       (conj queries {:accepted accept, :path path, :artificial true})
                                       (conj queries {:accepted accept, :path path}))))
                                 []
                                 words)]
         (recur (dec depth) (conj queries new-queries)))
       (let [artificial-queries (->> queries
                                     flatten
                                     (filter :artificial))] ;; we only care about paths that have been generated using artificial input
         (if (= (count artificial-queries) 0) ;; if no artifical queries, run the full base set
           (flatten queries)
           artificial-queries))))))


(defn- check-sfa-paths
  [sfa paths]
  (reduce (fn [critical-paths {:keys [path]}]
            (if (or
                 (contains? (set (map first path)) 2147483648) ;; FIXME nasty hack to deal with cases where the SFA is inconsistent
                 (contains? (set (map second path)) -1)
                 (contains? (set (map first path)) -1))
              critical-paths
              (let [refined (coastal/refine-path path)
                    accepted (table/member? (paths/make-concrete refined))]
                (if (not= path refined)
                  (conj critical-paths {:path path, :refined refined, :accepted accepted})
                  critical-paths))))
          []
          paths))

(defn- apply-ces-from-sfa
  "FIXME Why does the algorithm explode if we don't `symlearn.table/close` here?"
  [table db ces]
  (reduce (fn [[db table] ce]
            (let [{:keys [refined accepted]} ce
                  new-entry {:accepted accepted, :path refined}
                  evidence (paths/make-evidences refined)
                  new-db (conj db new-entry)
                  table-with-ce (table/process-ce table new-entry)
                  table-with-evidence (table/apply-evidences table-with-ce evidence)]
              [new-db (table/close table-with-evidence new-db)]))
          [db table]
          ces))

(defn check-equivalence
  "Checks the conjectured SFA for equivalence, checking equivalence for inputs
  up to a length of `depth`. Returns any counter examples discovered if the
  equivalence check fails."
  [sfa depth]
  (tufte/p
   ::check-equivalence
   (let [queries (make-queries sfa depth)
         counter-examples (check-sfa-paths sfa queries)]
     counter-examples
     #_(when (seq counter-examples)
       counter-examples))))

(defn learn
  "Return an table learnt with seed database `db` and a reverse equivalence
  check up to `depth`. The learnt table has metadata attached describing why
  the learning halted."
  [{:keys [depth parse-fn table db]}]
  (binding [table/*parse-fn* parse-fn]
    (let [counter (atom 0)
          prev-table (atom nil)]
      (loop [db (or db (coastal/get-seed-inputs))
             table (or table (table/init-table (table/make-table) db))]
        (swap! counter inc)
        (if-not (table/closed? table)
          (recur db (table/close table db))

          (let [sfa (table/table->sfa table)
                ce-from-db (sfa/run-all-from-db sfa db)
                ces-from-sfa (check-equivalence sfa depth)]
            (cond
              ;; Forward equivalence check
              (map? ce-from-db)
              (let [counter-example (:path ce-from-db)
                    table-with-ce (table/process-ce table ce-from-db)
                    evidences (paths/make-evidences counter-example)
                    table-with-evidence (table/apply-evidences table-with-ce evidences)]

                (if (= @prev-table table-with-evidence)
                  {:table table
                   :db db
                   :depth depth
                   :reason {:stage :forward-eqv, :input ce-from-db}}
                  (do
                    (reset! prev-table table)
                    (recur db table-with-evidence))))

              ;; Reverse equivalence check
              (not (empty? ces-from-sfa))
              (let [[db' table'] (tufte/p ::apply-ces-from-sfa (apply-ces-from-sfa table db ces-from-sfa))]
                (if (= @prev-table table')
                  {:table table
                   :db db'
                   :depth depth
                   :reason {:stage :backward-eqv, :inputs ces-from-sfa}}
                  (do
                    (reset! prev-table table')
                    (recur db' table'))))

              ;; Return the learnt table
              :default
              {:table table
               :db db
               :depth depth
               :reason :equivalent})))))))