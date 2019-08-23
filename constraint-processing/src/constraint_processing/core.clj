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

(defn sorted-paths
  [db]
  (sort #(compare (count (:path %1))
                  (count (:path %2)))
        db))

(defn build-db
  [filename]
  (->> (io/resource filename)
       slurp
       get-records
       process-records
       sorted-paths))


(defn merge-dbs
  [dbs]
  (apply set/union dbs))

(defn create-database
  [files]
  (->> files
       (map build-db)
       (apply set/union)))


;; (def db (merge-dbs dbs))

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


(defn suffixes
  [path]
  (for [n (range (count path))]
    (->> path (drop n) (into []))))


(defn follow-paths
  ""
  [path db]
  (->> db
       (query path :starts-with)
       (filter #(= (count (:path %)) (inc (count path))))
       vec))

(defn load-db-from-prefix
  "Load n files with the given prefix into a database, and
  return that database."
  [prefix n]
  (-> (map (partial str prefix) (range 1 (inc n)))
      create-database
      sorted-paths))

(defn load-files
  "Take a list of files and load them into a database."
  [files]
  (-> files
      create-database
      sorted-paths))
