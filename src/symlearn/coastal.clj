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
            [clojure.java.shell :as sh]
            [clojure.set :as set])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

(defonce ^Process *coastal-instance* nil)
(defonce *current-parser* nil)

(def string-config "Regex.xml")

(defmacro wcar*
  "Wraps Redis commands in a `car/wcar`."
  [& body]
  `(let [redis-conn# {:pool {} :spec {:host "127.0.0.1" :port 6379}}]
     (car/wcar redis-conn# ~@body)))


(defn refine-string
  "Returns [boolean list] of accepted? and path conditions"
  [string]
  ;; enqueue the string for solving
  (let [exploded-string (map int (.toCharArray string))] ;; avoid "first byte is null" encoding issues
    (wcar* (car/del :refine)
           (car/del :refined)
           (apply (partial car/rpush :refine) exploded-string)))

  ;; wait for a solved response
  (while (not= 1 (wcar* (car/exists :refined))))

  ;; process reponse
  (let [refined-path (tufte/p ::refine-path (wcar* (car/get :refined)))
        _ (println refined-path)
        [accepted path-condition] (str/split refined-path #"\n")]
    (wcar* (car/del :refined))
    [(read-string accepted) path-condition]))

;; (refine-string "\u0000h\u0000e\uffffll")

;; (let [bytes (first (wcar* (car/lrange :refine 0 -1)))
;;       input (str/join (map char bytes))]
;;   input)

;; (install-parser! "a|b")
;; (refine-string "\u0000")
;; (stop!)


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

(defn constraint-suffixes
  "Return the suffixes of the path condition pc"
  [constraint-sets]
  (for [n (range 1 (count constraint-sets))]
    (->> constraint-sets
         (drop n)
         vec)))

(defn constraint-prefixes
  "Return every prefix of `path`, including `path`."
  [constraint-sets]
  (for [n (range 1 (count constraint-sets))]
    (->> constraint-sets
         reverse
         (drop n)
         reverse
         vec)))

(assert (= [[#{:a} #{:b}] [#{:a}]] (constraint-prefixes [#{:a} #{:b} #{:c}])))
(assert (= [[#{:b} #{:c}] [#{:c}]] (constraint-suffixes [#{:a} #{:b} #{:c}])))

(defprotocol IPathCondition
  (accepted? [this] "Return true if `this` represents a successful parse.")
  (length [this] "Return the number of indices bounded by `this`.")
  (constraints [this] "Returns the bounds on each index bounded by `this`.")
  (witness [this] "Return a string that satisfies the constraints in `this`.")
  (suffixes [this] "Return the path conditions that are suffixes of `this`. Exludes `this` and epsilon.")
  (prefixes [this] "Return the path conditions that are prefixes of `this`. Exludes `this` and epsilon."))

(defn compile-parsers!
  "Compile the parsers installed in the Coastal system."
  []
  (let [coastal-dir (File. "coastal")
        args (into-array ["./gradlew" "compileJava"])
        builder (ProcessBuilder. ^"[Ljava.lang.String;" args)]
    (.directory builder coastal-dir)
    (let [compiler (.start builder)]
      (while (.isAlive compiler))
      ::ok)))

(defn stop!
  "Stop a Coastal process running in `coastal`."
  []
  (when *coastal-instance*
    (.destroyForcibly *coastal-instance*)
    (while (.isAlive *coastal-instance*))
    (alter-var-root #'*coastal-instance* (constantly nil)))
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
  (alter-var-root #'*current-parser* (constantly regex))
  (let [parser-src (intervals/sfa->java (intervals/regex->sfa regex) "examples.tacas2017" "Regex")]
    (spit "coastal/src/main/java/examples/tacas2017/Regex.java" parser-src)
    (compile-parsers!)
    (start!)))

(defn running?
  []
  (and *coastal-instance*
       (.isAlive *coastal-instance*)))

(defn active-parser
  []
  *current-parser*)

(declare query)

(defrecord PathCondition [accepted constraints]
  IPathCondition
  (accepted? [this] (:accepted this))
  (constraints [this] (:constraints this))
  (length [this] (count (:constraints this)))
  (witness [this] (str/join (map char (z3/witness (:constraints this)))))
  (suffixes [this]
    (map z3/witness (constraint-suffixes (:constraints this))))
  (prefixes [this]
    (map query (map #(str/join (map char %)) (map z3/witness (constraint-prefixes (:constraints this)))))))

(defn query
  "Return a map with a set of assertions against `string`, and the parser's
  acceptance status."
  [string]
  (if-not *coastal-instance*
    (start!))
  (let [[accepted path] (refine-string string)
        constraints (->> path
                         path->constraints
                         (map (fn [[idx op guard]]
                                [(Integer/parseInt idx) op (Integer/parseInt guard)]))
                         (sort-by first)
                         (partition-by first)
                         (map (fn [constraints] (vec (map #(vec (drop 1 %)) constraints))))
                         (map set)
                         (vec))]
    (->PathCondition accepted constraints)))

;; create and fill the table

(defn make-table
  []
  {:S {(query "") []}
   :R {}
   :E [""]})

(defn fill-row
  [[path row] evidence]
  (let [row-length (count row)
        evidence-count (count evidence)]
    (if (= row-length evidence-count)
      row
      (let [new-row (reduce (fn [row e]
                              (conj row (accepted? (query (str (witness path) e)))))
                            row
                            (drop row-length evidence))]
        new-row))))

(defn fill
  [table]
  (let [{:keys [S R E]} table]
    (-> table
        (assoc :S (reduce
                   (fn [new-s [path row]] (assoc new-s path (fill-row [path row] E)))
                   {}, S))
        (assoc :R (reduce
                   (fn [new-r [path row]] (assoc new-r path (fill-row [path row] E)))
                   {}, R)))))

;; adding new information to the table

(defn add-path-condition
  [table {:keys [constraints accepted] :as path}]
  (update table :R #(assoc % path [accepted])))

(defn add-evidence
  [table evidence]
  (update table :E #(conj % evidence)))

;; closing a table

(defn closed?
  [{:keys [S R]}]
  (let [s-rows (set (vals S))
        r-rows (set (vals R))]
    (set/subset? r-rows s-rows)))

(defn open-entries
  "Return a entries from R in `table` that do not appear in S. Return nil
  if no open entries are present"
  [{:keys [S R]}]
  (let [s-rows (set (vals S))
        r-rows (set (vals R))]
    (let [candidate-rows (set/difference r-rows s-rows)
          entries (filter (fn [[_ row]] (candidate-rows row)) R)]
      (set (keys entries)))))

(defn close
  [{:keys [R] :as table}]
  (if-not (closed? table)
    (let [promotee (first (open-entries table))
          row (R promotee)]
      (-> table
          (update-in [:R] #(dissoc % promotee))
          (update-in [:S] #(assoc % promotee row))))
    table))

;; (prefixes (map->PathCondition (query "0-")))

;; (-> (make-table)
;;     (fill)
;;     (add-path-condition (query "a"))
;;     (add-path-condition (query "0-0-0"))
;;     (add-evidence "b")
;;     (fill)
;;     (close)
;;     (close)
;;     ;; (fill)
;;     ;; (closed?)
;;     ;; (open-entries)
;;     ;;  (close)
;;     ;; (close)
;;     ;; (keys)
;;     (clojure.pprint/pprint)
;;     ;; (fill)
;;     ;; (fill)
;;     ;; (:R)
;;     ;; section->map
;;     ;; (closed?)
;;     ;; (open-entries)
;;     #_(as-> $ (map (comp length :path) $)))


(comment



  ;; install the castle move parser
  (install-parser! "0-0(-0)?\\+?")

  (map (comp println :constraints) (suffixes (query "0-0-0")))

  (println "----")
  (clojure.pprint/pprint (suffixes (query "0-0l")))

  (prn *current-parser*)

  (query "")

  ;; Query Coastal for path information of some input given to the parser
  (->> (query "\uFFFF")
       constraints)
;; TODO Apply a constraint set to a characer qqq
  (-> (make-table)
      (fill)
      (add-path-condition (query "0-"))
      (add-path-condition (query "0-0-"))
      (add-evidence "0+")
      (fill)
      (add-evidence "fill")
      (add-path-condition (query "lolk"))
      (fill)
      ;; (closed?)
      clojure.pprint/pprint)

  ;; Install an arbitrary regex
  (install-parser! "(fill)(ed)?")
  (install-parser! "(a|b)?")

  (compile-parsers!)
  ;; Stop Coastal
  (stop!)

)

