(ns symlearn.coastal
  (:require [clojure.string :as str]
            [taoensso.carmine :as car]
            [taoensso.tufte :as tufte]
            [clojure.walk :as walk]
            [symlearn.paths :as paths]
            [symlearn.ranges :as ranges]
            [symlearn.table :as table]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [instaparse.core :as insta]
            [symlearn.intervals :as intervals]
            [clojure.java.shell :as sh])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

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
  [string]
  (wcar* (car/del :refined)
         (car/set :refine string))

  (while (not= 1 (wcar* (car/exists :refined))))
  (let [refined-path (tufte/p ::refine-path (wcar* (car/get :refined)))
        [accepted path-condition] (str/split refined-path #"\n")]
    (wcar* (car/del :refined))
    [(read-string accepted) path-condition]))

(path->constraints (second (refine-string "c")))

(defn prepare-z3-cmd
  [string]
  (let [const-decl "(declare-const a Int)\n"
        [_ path] (refine-string string)
        assertions (vec (for [[_ op bound] (path->constraints path)]
                          (format "(assert (%s a %s))\n" (if (= "==" op) "=" op) bound)))
        sat-call "(check-sat)\n"
        model-call "(get-model)\n"]
    (str const-decl (str/join "\n" assertions) sat-call model-call)))

(defn z3-model
  "Return a model that "
  [string])

(prepare-z3-cmd "b")
(sh/sh "z3" "-in" :in (prepare-z3-cmd "b"))

(defn path->constraints
  "Return a seq of constraints extracted from `path-condition`, each of
  the form [idx op bound], where idx is the index into the array, op is
  the operation of the assertion (EQ, NEQ, GE, etc.), and bound is the
  argument given to the assertion."
  [path-condition]
  (let [constraints (re-seq #"A\$(\d+)(<=|==|>=|!=|>|<)(\d+)" path-condition)]
    (->> constraints
         (map #(drop 1 %)) ;; drop the full match
         (sort-by first))))

(defn constraint-node?
  "Return true if `node` is a constraint node over an array index."
  [node]
  (when (and (vector? node) (= 4 (count node)))
    (let [[type lhs guard rhs] node
          [_ [_ _ idx]] lhs
          [_ op] guard
          [_ bound] rhs]
      (and (= type :expr) (not (nil? idx))))))

(def path-condition-parser
  "A parser that eats Coastal path conditions"
  (insta/parser
   "S = expr?
    expr = lhs comp rhs | <'('> expr <')'> | expr ('&&' expr)
    lhs = #'\\d+' | idx
    idx = 'A$' (#'\\d')
    rhs = #'\\d+'
    comp = '==' | '!=' | '>' | '>=' | '<' | '<='"))

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
