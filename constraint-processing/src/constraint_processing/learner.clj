(ns constraint-processing.learner
  (:require [constraint-processing.core :as paths]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:import Parser
           TacasParser))

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

(defn int-arr
  [list]
  (into-array Integer/TYPE list))

(defn member?
  [w]
  (TacasParser/parse (int-arr w)))

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
          #{}
          entries))

(defn fill
  [table]
  (-> table
      (assoc :S (fill-entries (:S table) (:E table)))
      (assoc :R (fill-entries (:R table) (:E table)))))

(defn make-table
  "Create an empty observation table."
  []
  {:S #{ {:path [] :row []} }
   :R #{}
   :E [[]]})

(defn init-table
  "Add an initial counter example from the database into the
  observation table."
  [table db]
  (let [initial-path (nth db 3)
        accepted (:accepted initial-path)]
    (-> table
        (add-r (:path initial-path))
        (fill))))

(defn add-r
  [table r]
  (let [prefixes (paths/prefixes r)
        s-paths (into #{} (map :path (:S table)))
        new-table (reduce (fn [new-table prefix]
                            (if (contains? s-paths prefix)
                              new-table
                              (update new-table :R #(conj % {:path prefix :row []}))))
                          table
                          prefixes)]
    (fill new-table)))

(defn process-ce
  [table ce]
  (-> table
      (add-r (:path ce))
      (fill)))

(defn promote
  [table path]
  (-> table
      (update :R (fn [entries] (into [] (filter #(not= (:path %) path) entries))))
      (update :S (fn [entries] (conj entries {:path path :row []})))
      (fill)))

(defn closed?
  [table]
  (let [r-rows (into #{} (map first (group-by :row (:R table))))
        s-rows (into #{}(map first (group-by :row (:S table))))]
    (if (set/subset? r-rows s-rows)
      true
      false)))

(defn get-close-candidates
  [table]
  (let [r-rows (into #{} (map first (group-by :row (:R table))))
        s-rows (into #{}(map first (group-by :row (:S table))))]
    (if-not (set/subset? r-rows s-rows)
      (let [candidate-rows (set/difference r-rows s-rows)
            candidate-row (first candidate-rows)]
        (into #{} (filter #(= candidate-row (:row %)) (:R table)))))))

(defn close-auto
  [table]
  (promote table (:path (first (get-close-candidates table)))))

(defn add-evidence
  [table evidence]
  (-> table
      (update :E #(conj % evidence))
      (fill)))

(defn close
  [table path db]
  (let [follow-candidates (paths/follow-paths path db)]
    (if (empty? follow-candidates)
      (promote table path)
      (-> table
          (promote path)
          (add-r (:path (rand-nth follow-candidates)))))))

(defn row->entry
  "Given a observation table and a row, return the matching
  entry in S."
  [table row]
  (->> table
       :S
       (filter #(= (:row %) row))
       first))

(defn get-prefix-pairs
  "Given an observation table, produce the proper prefix pairs,
  such that (len u1) == (- 1 (len u2))."
  [table]
  (let [states (:S table)
        entries (set/union (:R table) (:S table))
        prefixes (reduce (fn [transitions entry]
                           (->> entries
                                (map (juxt identity
                                           (constantly entry)
                                           #(paths/prefix? (:path %) (:path entry))))
                                (filter last)
                                (into [])
                                (conj transitions)))
                         []
                         entries)]
    (->> prefixes
         (mapcat identity)
         (into []))))

(defn pair->transition
  [table prefix-pair]
  (let [from (first prefix-pair)
        to (second prefix-pair)
        from-row (:row from)
        to-row (:row to)]
    {:from (:path (row->entry table from-row))
     :input (last (:path to))
     :to (:path (row->entry table to-row))}))

(defn pairs->transitions
  [table prefix-pairs]
  (->> (for [pair prefix-pairs]
         (if (= (first pair) (second pair))
           []
           (pair->transition table pair)))
       (filter #(not= nil (:input %)))
       (into [])))

(defn get-transitions
  [table]
  (let [states (:S table)
        prefix-pairs (get-prefix-pairs table)
        transitions (pairs->transitions table prefix-pairs)]
    transitions))

(defn get-states
  [transitions]
  (let [states (group-by :from transitions)]
    states))

;; Usage
(def tacas-files ["constraints-depth-1"
                  "constraints-depth-2"
                  "constraints-depth-3"
                  "constraints-depth-4"
                  "constraints-depth-5"
                  "constraints-depth-6"
                  "constraints-depth-7"])

(def short-files ["alt-con-1"
                  "alt-con-2"])

(def db (-> tacas-files
            paths/create-database
            paths/sorted-paths))

(defn sprint
  [arg message]
  (do
    (println "----------------")
    (println message)
    (println "----------------")
    (pprint arg)
    arg))

(let [table (-> (make-table) (init-table db))]
  (loop [read true
         count 0
         table table]
    (println (str "----------" count "----------"))
    (pprint table)
    (println ">")
   (if read
     (let [input (read-line)]
       (cond
         (= input "quit")
         (println "Bye!")

         (= input "closed?")
         (do
           (println (closed? table))
           (recur true (inc count) table))

         (= input "close")
         (recur true (inc count) (close-auto table))

         (= input "conj")
         (do
           (pprint (build-sfa table))
           (recur true (inc count) table))

         (= input "ce")
         (do
           (println "> Enter counter example as vector: ")
           (let [ce (edn/read-string (read-line))]
             (recur true (inc count) (process-ce table {:path ce}))))

         (= input "ev")
         (do
           (println "> Enter evidence as vector: ")
           (let [evidence (edn/read-string (read-line))]
             (println (str "Adding evidence " evidence))
             (recur true (inc count) (add-evidence table evidence)))))))))

(defn get-state-map
  [transitions]
  (as-> transitions $
    (group-by :from $)
    (map first $)
    (map vector $ (iterate inc 0))
    (into {} $)))

(defn get-final-states
  [table state-map]
  (->> (:S table)
       (filter #(first (:row %)))
       (map :path)
       (map (partial get state-map))
       (into [])))

(defn build-sfa
  [table]
  (let [transitions (get-transitions table)
        state-map (get-state-map transitions)
        final-states (get-final-states table state-map)
        simple-transitions (reduce (fn [steps transition]
                                     (conj steps {:from (get state-map (:from transition))
                                                  :input (:input transition)
                                                  :to (get state-map (:to transition))}))
                                   []
                                   transitions)]
    {:transitions simple-transitions
     :initial-state (get state-map [])
     :final-states final-states}))

[:todo-list
 [:h "Automatic detection of non-determinism in the table."]
 [:h "Implementation of `fill` using `promote` and `add-r`." :done]
 [:h "Automatic closing of table, including the addition of a new entry in R."
  :desc "For this we need to figure out an ergonomic way of getting new information from the database component, including filtering for 'smarter', shorter paths."]
 [:h "`add-r` needs to add prefixes as well." :done]]
