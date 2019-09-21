(ns symlearn.coastal
  (:require [clojure.string :as str]
            [taoensso.carmine :as car]
            [taoensso.tufte :as tufte]
            [symlearn.paths :as paths]
            [symlearn.ranges :as ranges]
            [symlearn.table :as table]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

(defmacro wcar*
  "Wraps Redis commands in a `car/wcar`."
  [& body]
  `(let [redis-conn# {:pool {} :spec {:host "redis" :port 6379}}]
     (car/wcar redis-conn# ~@body)))

(defn refine-path
  "Return the exact constraints along `path` by invoking a Coastal diver."
  [path]
  (tufte/p
   ::refine-path
   (if (= path []) ;; the empty path will always be the empty path
     []
     (let [input (str/join " " (paths/make-concrete path))]

       (wcar* (car/del :refined)
              (car/set :refine input))

       (while (not= 1 (wcar* (car/exists :refined))))

       (let [refined-path (tufte/p ::refine-path (wcar* (car/get :refined)))]
         (wcar* (car/del :refined))
         (read-string refined-path))))))

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

(defn compile-parsers!
  "Compile the parsers installed in the Coastal system."
  []
  (let [coastal-dir (File. "coastal")
        args (into-array ["./gradlew" "compileJava"])
        builder (ProcessBuilder. ^"[Ljava.lang.String;" args)]
    (.directory builder coastal-dir)
    (.start builder)))

(defn ^Process start-coastal!
  "Launch a Coastal process with a config file called `filename` as an argument."
  [filename]
  (tufte/p
   ::start-coastal
   (let [config (io/resource filename)
         args (into-array ["./gradlew" "run" (str "--args=" (.getPath config))])
         builder (ProcessBuilder. ^"[Ljava.lang.String;" args)
         coastal-dir (File. "coastal")]
     (.directory builder coastal-dir)
     (.start builder))))

(defn stop-coastal!
  "Stop a Coastal process running in `coastal`."
  [^Process coastal]
  (.destroyForcibly coastal))
