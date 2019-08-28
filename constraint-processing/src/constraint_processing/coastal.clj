(ns constraint-processing.coastal
  (:require [clojure.string :as str]
            [taoensso.carmine :as car]
            [taoensso.tufte :as tufte]
            [constraint-processing.paths :as paths]
            [constraint-processing.ranges :as ranges]
            [constraint-processing.table :as table]))

(defmacro wcar*
  "Wraps Redis commands in a `car/wcar`."
  [& body]
  `(let [redis-conn# {:pool {} :spec {:host "127.0.0.1" :port 6379}}]
     (car/wcar redis-conn# ~@body)))

(defn refine-path
  "Return the exact constraints along `path` by invoking a Coastal diver."
  [path]
  (if (= path [])
    []
    (let [input (str/join " " (paths/make-concrete path))]

      (wcar* (car/del :refined)
             (car/set :refine input))

      (while (not= 1 (wcar* (car/exists :refined))))

      (let [refined-path (tufte/p ::refine-path (wcar* (car/get :refined)))]
        (wcar* (car/del :refined))
        (read-string refined-path)))))

(defn- get-seed-constraints
  "Return all constraints of unit length for the parser currently running
  in the Coastal system."
  []
  (let [[seed] (refine-path [[0 0]])
        complement (first (ranges/get-completing-predicates #{seed}))]
    (loop [known #{seed}
           unknown #{complement}]
      (if (empty? unknown)
        known
        (let [guess (first unknown)
              [constraint] (refine-path [guess])
              known' (conj known constraint)
              unknown' (ranges/get-completing-predicates known')]
          (recur (conj known constraint) unknown'))))))

(defn get-seed-inputs
  "Return a database of constraints and acceptance information for input of unit length."
  []
  (map (fn [[min max]]
         {:path [[min max]] :accepted (table/member? [min])})
       (get-seed-constraints)))
