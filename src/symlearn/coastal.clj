(ns symlearn.coastal
  (:require [clojure.string :as str]
            [taoensso.carmine :as car]
            [taoensso.tufte :as tufte]
            [clojure.walk :as walk]
            [symlearn.paths :as paths]
            [symlearn.intervals :as intervals]
            [symlearn.ranges :as ranges]
            [symlearn.table :as table]
            [symlearn.z3 :as z3]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

(def ^Process *coastal-instance* nil)

(def string-config "Regex.xml")

(defmacro wcar*
  "Wraps Redis commands in a `car/wcar`."
  [& body]
  `(let [redis-conn# {:pool {} :spec {:host "127.0.0.1" :port 6379}}]
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

(defn refine-string
  "Returns [boolean list] of accepted? and path conditions"
  [string]
  (wcar* (car/del :refined)
         (car/set :refine string))

  (while (not= 1 (wcar* (car/exists :refined))))
  (let [refined-path (tufte/p ::refine-path (wcar* (car/get :refined)))
        [accepted path-condition] (str/split refined-path #"\n")]
    (wcar* (car/del :refined))
    [(read-string accepted) path-condition]))

(defn path->constraints
  "Return a seq of constraints extracted from `path-condition`, each of
  the form [idx op bound], where idx is the index into the array, op is
  the operation of the assertion (EQ, NEQ, GE, etc.), and bound is the
  argument given to the assertion."
  [path-condition]
  (let [constraints (re-seq #"A\$(\d+)(<=|==|>=|!=|>|<)(\d+)" path-condition)]
    (->> constraints
         (map #(drop 1 %)) ;; drop the full match
         (sort-by first)
         (set))))

(defprotocol IPathCondition
  (accepted? [this] "Return true if `this` represents a successful parse.")
  (length [this] "Return the number of indices bounded by `this`.")
  (constraints [this] "Returns the bounds on each index bounded by `this`.")
  (witness [this] "Return a string that satisfies the constraints in `this`."))

(defrecord PathCondition [accepted constraints]
  IPathCondition
  (accepted? [_] accepted)
  (constraints [_] constraints)
  (length [_] (->> constraints
                   keys
                   (reduce max)
                   inc))
  (witness [this] (z3/solve this)))

(defn query
  "Return a map with a set of assertions against `string`, and the parser's
  acceptance status."
  [string]
  (let [[accepted path] (refine-string string)
        constraints (->> path
                         path->constraints
                         (map (fn [[idx op guard]]
                                [(Integer/parseInt idx) op (Integer/parseInt guard)]))
                         (group-by #(first %))
                         (into (sorted-map)))]
    (->PathCondition accepted constraints)))

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
    (let [compiler (.start builder)]
      (while (.isAlive compiler)))))

(defn stop!
  "Stop a Coastal process running in `coastal`."
  []
  (.destroyForcibly *coastal-instance*)
  (while (.isAlive *coastal-instance*))
  (alter-var-root #'*coastal-instance* (constantly nil))
  ::ok)

(defn ^Process start!
  "Launch a Coastal process with a config file called `filename` as an argument."
  []
  (if *coastal-instance*
    (stop!))
  (tufte/p
   ::start-coastal
   (let [config (io/resource string-config)
         args (into-array ["./gradlew" "run" (str "--args=" (.getPath config))])
         builder (ProcessBuilder. ^"[Ljava.lang.String;" args)
         coastal-dir (File. "coastal")]
     (.directory builder coastal-dir)
     (let [coastal-instance (.start builder)]
       (alter-var-root #'*coastal-instance* (constantly coastal-instance))
       ::ok))))

(defn install-parser!
  [regex]
  (if *coastal-instance*
    (stop!))
  (let [parser-src (intervals/sfa->java (intervals/regex->sfa regex) "examples.tacas2017" "Regex")]
    (spit "coastal/src/main/java/examples/tacas2017/Regex.java" parser-src)
    (compile-parsers!)
    (start!)))

(defn running?
  []
  (if *coastal-instance*
    (.isAlive *coastal-instance*)
    false))

(comment

  ;; Start an instance of Coastal
  (start!)

  ;; Confirm that it is running
  (running?)

  ;; Install an arbitrary regex
  (install-parser! "5?l|w")
  (install-parser! "(a|b)?")
  (install-parser! "0-0(-0)?\\+?")

  ;; Query Coastal for path information of some input given to the parser
  (refine-string "") ;; TODO Turn path conditions into fns we can call to check membership (fn [str] (>= 2 str[0]) (<= 1 str[2])) etc

  ;; Stop Coastal
  (stop!)

  )
