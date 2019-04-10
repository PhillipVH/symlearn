(ns constraint-processing.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def data-1 (slurp (get-constraint-file 1)))
(def data-2 (slurp (get-constraint-file 2)))
(def data-3 (slurp (get-constraint-file 3)))
(def data-4 (slurp (get-constraint-file 4)))

(defn get-constraint-file
  [depth]
  (io/resource (str "constraints-depth-" depth)))

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
    [(if accepted :accept :reject) processed]))


(defn process-records
  [records]
  (->> records
       ;; get-records
       (map record->run)
       (into #{})))


(def db-4 (->> data-4
               get-records
               process-records))

(def db-3 (->> data-3
               get-records
               process-records))

(def db-2 (->> data-2
               get-records
               process-records))

(def db-1 (->> data-1
               get-records
               process-records))

(def db (apply set/union [db-1 db-2 db-3 db-4]))

(defn query
  [constraints db]
  (filter #(= (second %) constraints) db))

(query [[21 (Integer/MAX_VALUE)] [21 (Integer/MAX_VALUE)]] db)
