(ns symlearn.coastal
  (:gen-class)
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [symlearn.codegen :as codegen]
            [symlearn.intervals :as intervals]
            [symlearn.time :as time]
            [symlearn.z3 :as z3]
            [taoensso.carmine :as car]
            [taoensso.timbre :as log]
            [taoensso.tufte :as tufte])
  (:import automata.sfa.SFA
           java.io.File))

(set! *warn-on-reflection* true)

(defonce ^Process coastal-instance nil)

(def redis-conn {:pool {}
                 :spec {:host (or (System/getenv "REDIS_HOST") "localhost")
                        :port 6379}})

(defmacro wcar*
  "Wraps Redis commands in a `car/wcar`."
  [& body]
  `(car/wcar redis-conn ~@body))

(defn refine-string
  "Returns [boolean list] of accepted? and path conditions"
  [string]
  (log/trace "Requesting refinement:" string)
  ;; enqueue the string for solving
  (let [exploded-string (map int (.toCharArray ^String string))
        strlen (count string)] ;; avoid "first byte is null" encoding issues
    (wcar* (car/del :refined)
           (apply (partial car/rpush :refine) (if (= 0 strlen) ["epsilon"] exploded-string))
           (car/rpush :mustrefine ::ready)))

  ;; block for a response
  (let [[_ refined-path] (tufte/p ::wait-for-refinement (wcar* (car/brpop :refined 0)))
        [accepted path-condition] (str/split refined-path #"\n")]
    (wcar* (car/del :refined))
    (log/trace "Refinement received:" accepted)
    [(read-string accepted) path-condition]))

(defn path->constraints
  "Return a seq of constraints extracted from `path-condition`, each of
  the form [idx op bound], where idx is the index into the array, op is
  the operation of the assertion (EQ, NEQ, GE, etc.), and bound is the
  argument given to the assertion."
  [path-condition]
  (let [constraints (re-seq #"A\$(\d+)(<=|==|>=|!=|>|<)(\d+)" path-condition)]
    (->> constraints
         (map #(vec (drop 1 %))) ;; drop the full match
         (sort-by first)
         (set))))

(defn suffixes*
  "Return the suffixes of the path condition pc"
  [constraint-sets]
  (let [suffixes (map #(drop % constraint-sets)
                      (range 1 (count constraint-sets)))]
    (set suffixes)))

(defn prefixes*
  "Return the suffixes of the path condition pc"
  [constraint-sets]
  (let [prefixes (map (fn [n] (take n constraint-sets))
                      (range 1 (count constraint-sets)))]
    (set prefixes)))

(assert (= #{[#{:a} #{:b}] [#{:a}]} (prefixes* [#{:a} #{:b} #{:c}])))
(assert (= #{[#{:b} #{:c}] [#{:c}]} (suffixes* [#{:a} #{:b} #{:c}])))

(defprotocol IPathCondition
  (accepted? [this] "Return true if `this` represents a successful parse.")
  (length [this] "Return the number of indices bounded by `this`.")
  (constraints [this] "Returns the bounds on each index bounded by `this`.")
  (witness [this] "Return a string that satisfies the constraints in `this`.")
  (suffixes [this] "Return the path conditions that are suffixes of `this`. Exludes `this` and epsilon.")
  (prefixes [this] "Return the path conditions that are prefixes of `this`. Exludes `this` and epsilon."))

;; process management

(defn stop!
  "Stop a Coastal process running in `coastal`."
  []
  (let [coastal-pid (str/trim (:out (sh/sh "pgrep" "-f" "COASTAL.*Regex")))]
    (sh/sh "kill" "-9" coastal-pid))
  (wcar* (car/flushall))
  (if coastal-instance
    (do
      (.destroyForcibly coastal-instance)
      (while (.isAlive coastal-instance))
      (alter-var-root #'coastal-instance (constantly nil)))
    (log/info "No coastal to stop"))
  ::ok)

(defn coastal-pid
  "Argument can be :mem for membership or :eqv for equivalence"
  [mem-or-eqv]
  (let [mem (= :mem mem-or-eqv)
        ident (if mem "Regex" "Example")]
    (str/trim (:out (sh/sh "pgrep" "-f" (str "COASTAL.*" ident))))))

(defn kill-pid!
  [pid]
  (sh/sh "kill" "-9" pid))

(defn coastal-emergency-stop
  []
  (map kill-pid! (map coastal-pid #{:mem :eqv})))

(defn ^Process start!
  "Launch a Coastal process with a config file called `filename` as an argument."
  []
  (when coastal-instance
    (stop!))
  (tufte/p
   ::start-coastal
   (let [path-in-docker (System/getenv "MEMBERSHIP_CONFIG_PATH")
         args-in (or path-in-docker (str (System/getProperty "user.dir") "/resources/Regex.xml"))
         args (into-array ["./gradlew" "run" (str "--args=" args-in)])
         builder (ProcessBuilder. ^"[Ljava.lang.String;" args)
         coastal-dir (File. "mem-coastal")]
     (.directory builder coastal-dir)
     (let [new-coastal-instance (.start builder)]
       (alter-var-root #'coastal-instance (constantly new-coastal-instance))
       ::ok))))

(defn timeout
  [timeout-ms callback]
  (let [fut (future (callback))
        ret (deref fut timeout-ms ::timed-out)]
    (when (= ret ::timed-out)
      (future-cancel fut)
      (stop!))
    ret))

(defn install-parser!
  [regex]
  (when coastal-instance
    (stop!))
  (log/trace "Generating Parser:" {:target regex})
  (let [parser-src (intervals/sfa->java (intervals/regex->sfa regex) "examples.tacas2017" "Regex")]
    (spit "mem-coastal/src/main/java/examples/tacas2017/Regex.java" parser-src)
    (log/trace "Compiling Parser:" {:target regex})
    (codegen/compile-parsers!)
    (log/trace "Starting Parser:" {:target regex})
    (start!)
    (log/trace "Flushing Parser:" {:target regex})
    (dotimes [_ 3]
      (refine-string "")) ;; flush the default run from the membership oracle
    (log/info "Parser Ready:" {:target regex})
    ::ok))

(declare query)

(defrecord PathCondition [accepted constraints]
  IPathCondition
  (accepted? [this] (:accepted this))
  (constraints [this] (:constraints this))
  (length [this] (count (:constraints this)))
  (witness [this] (str/join (map char (z3/witness (:constraints this)))))
  (suffixes [this]
    (map query (map #(str/join (map char %)) (map z3/witness (suffixes* (:constraints this))))))
  (prefixes [this]
    (map query (map #(str/join (map char %)) (map z3/witness (prefixes* (:constraints this)))))))

(def s-mem-queries (atom 0))
(def newest-table (atom nil))

(defn query
  "Return a map with a set of assertions against `string`, and the parser's
  acceptance status."
  ([string count?]
   (when count?
     (swap! s-mem-queries inc))
   (query string))
  ([string]
   (let [[accepted path] (tufte/p ::refine-string (refine-string string))
         constraints (doall (->> path
                                 path->constraints
                                 (map (fn [[idx op guard]]
                                        [(Integer/parseInt idx) op (Integer/parseInt guard)]))
                                 (sort-by first)
                                 (partition-by first)
                                 (map (fn [constraints] (vec (map #(vec (drop 1 %)) constraints))))
                                 (map set)
                                 (vec)))]
     (->PathCondition accepted constraints))))

(defn check-equivalence!
  [{:keys [depth target _]}]
  (log/info "Starting Equivalence Check:" {:target target, :depth depth})
  (let [coastal-log (:out (sh/sh "./coastal/bin/coastal"
                                 "learning/Example.properties"
                                 :dir "eqv-coastal/build/classes/java/main"))
        ce (re-seq #"<<Counter Example: (.*)>>" coastal-log)]
    (log/info coastal-log)
    (log/info "Finished Equivalence Check:" {:target target, :depth depth})
    (when ce
      (let [ce (map second ce)]
        (set ce)))))

(defn check-equivalence-concolic
  [{:keys [symbolic-length ^SFA oracle ^SFA hypothesis timeout-ms]}]
  (codegen/install-equivalence-oracle! hypothesis oracle symbolic-length) ;; don't include compilation time
  (log/info "Minutes for this check:" (time/ms->m timeout-ms))
  (let [f (future (check-equivalence! {:depth symbolic-length
                                       :target oracle
                                       :candidate hypothesis}))
        ce (deref f
                  timeout-ms
                  ::timeout)]
    (if (= ce ::timeout)
      (do
        (log/trace "Equivalence Check for" oracle "timed out after" timeout-ms "ms")
        (future-cancel f)
        (kill-pid! (coastal-pid :eqv))
        ::timeout)
      (do
        (log/trace "Target" oracle "learnt successfully")
        ce))))

(defn check-equivalence-perfect
  "Use symbolicautomata to check our candidate against a perfect target."
  [{:keys [^SFA oracle ^SFA hypothesis]}]
  (let [check (SFA/areEquivalentPlusWitness oracle hypothesis intervals/solver (time/m->ms 10))
        equivalent? (.getFirst check)]
    (when-not equivalent?
      #{(str/join "" (.getSecond check))})))
