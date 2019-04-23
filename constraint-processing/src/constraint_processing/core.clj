(ns constraint-processing.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn get-records
  [data]
  (let [entries (-> data
                    (string/split #"\n\n"))
        stripped (map #(string/split % #"\n") entries)]
    stripped))


(defn record->run
  [record]
  (let [accepted (if (= "Accepted" (first record)) true false)
        constraints (drop 2 record)
        processed (reduce
                   (fn [pc pair]
                     (let [matches (drop 1 (re-find #"A\[(\d)\]\[(\d+|null),(\d+)\]" pair))
                           index (Integer/parseInt (nth matches 0))
                           lower (if (= (nth matches 1) "null") -1 (Integer/parseInt (nth matches 1)))
                           upper (Integer/parseInt (nth matches 2))]
                       (if-not (= -1 lower)
                         (conj pc [lower upper])
                         pc)))
                   []
                   constraints)]
    {:accepted accepted
     :path processed}))


(defn process-records
  [records]
  (->> records
       (map record->run)
       (into #{})))

(defn build-db
  [filename]
  (->> (io/resource filename)
       slurp
       get-records
       process-records))


(defn merge-dbs
  [dbs]
  (apply set/union dbs))


(def dbs (map build-db ["constraints-depth-1"
                        "constraints-depth-2"
                        "constraints-depth-3"
                        "constraints-depth-4"
                        "constraints-depth-5"
                        "constraints-depth-6"
                        "constraints-depth-7"]))

(defn create-database
  [files]
  (->> files
       (map build-db)
       merge-dbs))


(def db (merge-dbs dbs))

(defn prefix?
  "Returns true if s1 is a prefix of s2."
  [s1 s2]
  (=
   s1
   (->> s2
        (reverse)
        (drop 1)
        (reverse))))

(defn query
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
  [path]
  (for [n (range (count path))]
    (->> path reverse (drop n) reverse (into []))))

(defn sorted-paths
  [db]
  (sort #(compare (count (:path %1))
                  (count (:path %2)))
        db))


(defn feasible?
  [path db]
  (not (empty? (query path :exact db))))

(defn follow-paths
  "Given a path, find paths in the database that grow the path by one."
  [path db]
  (->> db
       (query path :starts-with)
       (filter #(= (count (:path %)) (inc (count path))))
       (into [])))

;;;; Usage examples

;; Match only exactly the given path condition
;; (query [[0 20] [25 30]] :exact db)

;; Match a path condition, and also match all the prefixes of that path condition
;; (pprint (query [[0 20] [31 98] [25 30] [0 10] [0 20]] :prefixes db))

;;;; TACASFUBAR

;; (filter #(= (count (second %)) 1) db)

;; (def c0 [0 20])
;; (def c1 [0 24])
;; (def c2 [21 (Integer/MAX_VALUE)])
;; (def c3 [25 30])
;; (def c4 [0 10])
;; (def c5 [99 99])
;; (def c6 [0 (Integer/MAX_VALUE)])
;; (def c7 [11 (Integer/MAX_VALUE)])

;; (count (query [c0 c3] :starts-with db))
