(ns constraint-processing.learner
  (:require [constraint-processing.core :as paths]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:import Parser
           TacasParser
           SingleParser))

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
  (SingleParser/parse (int-arr w)))

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

(defn init-table
  "Add an initial counter example from the database into the
  observation table."
  [table db]
  (let [follow-paths (paths/follow-paths [] db)
        new-table (reduce (fn [new-table r]
                            (add-r new-table (:path r)))
                            table
                            follow-paths)]
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

(defn add-rs
  [table rs]
  (reduce (fn [new-table r]
            (add-r new-table (:path r)))
          table
          rs))

(defn close
  [table db]
  (let [close-candidate (-> table get-close-candidates first :path)
        follow-candidates (paths/follow-paths close-candidate db)]
    (if (empty? follow-candidates)
      (promote table close-candidate)
      (-> table
          (promote close-candidate)
          (add-rs follow-candidates)
          ;; (add-r (:path (rand-nth follow-candidates)))
          ))))

(defn add-evidence
  [table evidence]
  (-> table
      (update :E #(conj % evidence))
      (fill)))

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
    {:transitions (group-by :from simple-transitions)
     :initial-state (get state-map [])
     :final-states final-states}))

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

(def single-value-files (map #(str "single-value-" %) (range 1 7)))

(def db (-> single-value-files
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

;; Single Values Example
(def target (edn/read-string "
{:transitions
 {0
  [{:from 0, :input [0 0], :to 3}
   {:from 0, :input [3 3], :to 1}
   {:from 0, :input [1 1], :to 2}
   {:from 0, :input [4 2147483647], :to 3}
   {:from 0, :input [2 2], :to 0}],
  1
  [{:from 1, :input [0 0], :to 3}
   {:from 1, :input [3 2147483647], :to 3}
   {:from 1, :input [2 2], :to 1}
   {:from 1, :input [1 1], :to 2}],
  2
  [{:from 2, :input [2 2], :to 2}
   {:from 2, :input [1 1], :to 0}
   {:from 2, :input [0 0], :to 3}
   {:from 2, :input [3 2147483647], :to 3}],
  3
  [{:from 3, :input [0 2147483647], :to 3}
   {:from 3, :input [0 2147483647], :to 3}]},
 :initial-state 1,
 :final-states [1]}"))

(-> (make-table)
    (init-table db)
    (sprint "Initial table")
    ;; (closed?) ;; false
    (close db)
    (sprint "Closed table")
    ;; (build-sfa) -- ce
    (process-ce {:path [[1 1] [1 1] [3 3]]})
    (sprint "Added ce 1 . 1 . 3 ")
    ;; (closed?) -- true
    ;; (build-sfa) -- non-det
    (add-evidence [:c 3])
    (sprint "Added evidence 3")
    ;; (closed?) -- false
    (close db)
    ;; (closed?)
    ;; (build-sfa) -- ce
    (process-ce {:path [[1 1] [0 0] [0 Integer/MAX_VALUE]]})
    (sprint "Added ce 1 . 0 . [0 inf]")
    ;; (closed?) ; -- true
    (is-consistent)
    ;; (build-sfa) ;-- non-det
    ;; (add-evidence [:c 1 3])
    ;; (closed?) -- false
    ;; (close db)
    ;; (closed?) -- true
    ;; (build-sfa)
    ;; (= target)
    (pprint)
    )

(defn intersects?
  "Given two constraint pairs, determine if the first intersects the second."
  [[x1 x2] [y1 y2]]
  (and
   (<= x1 y2)
   (<= y1 x2)))

(defn intersection
  "Given two constraint pairs, determine the intersection."
  [[[x1 x2] [y1 y2]]]
  [(max x1 y1) (max x2 y2)])

(defn is-consistent
  [table]
  (let [transitions (-> (build-sfa table) :transitions vals)
        f-transitions (mapcat identity transitions)
        from-to-pairs (->> f-transitions (group-by (juxt :from :to)))]
    ))


;; (let [table (-> (make-table) (init-table db))]
;;   (loop [read true
;;          count 0
;;          table table]
;;     (println (str "----------" count "----------"))
;;     (pprint table)
;;     (println ">")
;;    (if read
;;      (let [input (read-line)]
;;        (cond
;;          (= input "quit")
;;          (println "Bye!")

;;          (= input "closed?")
;;          (do
;;            (println (closed? table))
;;            (recur true (inc count) table))

;;          (= input "close")
;;          (recur true (inc count) (close table db))

;;          (= input "conj")
;;          (do
;;            (pprint (build-sfa table))
;;            (recur true (inc count) table))

;;          (= input "ce")
;;          (do
;;            (println "> Enter counter example as vector: ")
;;            (let [ce (edn/read-string (read-line))]
;;              (recur true (inc count) (process-ce table {:path ce}))))

;;          (= input "ev")
;;          (do
;;            (println "> Enter evidence as vector: ")
;;            (let [evidence (edn/read-string (read-line))]
;;              (println (str "Adding evidence " evidence))
;;              (recur true (inc count) (add-evidence table evidence)))))))))


[:todo-list
 [:h "Automatic detection of non-determinism in the table."]
 [:h "Implementation of `fill` using `promote` and `add-r`." :done]
 [:h "Automatic closing of table, including the addition of a new entry in R."
  :desc "For this we need to figure out an ergonomic way of getting new information from the database component, including filtering for 'smarter', shorter paths." :done]
 [:h "`add-r` needs to add prefixes as well." :done]]
