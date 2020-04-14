(ns symlearn.table
  (:require [clojure.set :as set]
            [symlearn.coastal :as coastal]
            [symlearn.intervals :as intervals]
            [symlearn.sfa :as sfa]
            [taoensso.tufte :as tufte]
            [taoensso.timbre :as log])
  (:import automata.sfa.SFA))

(set! *warn-on-reflection* true)

(def mem-queries (atom 0))
(def target-sfa (atom nil))

(defn make-table
  "Table"
  []
  (let [epsilon (coastal/query "")]
    {:S {epsilon [(coastal/accepted? epsilon)]}
     :R {}
     :E [""]}))

(defn quick-check
  [string]
  (swap! mem-queries inc)
  (.accepts ^SFA @target-sfa (vec (.toCharArray ^String string)) intervals/solver))

(defn fill-row
  [[path row] evidence]
  (let [row-length (count row)
        evidence-count (count evidence)]
    (if (= row-length evidence-count)
      row
      (let [new-row (reduce (fn [row e]
                              (conj row (tufte/p ::membership-query (quick-check (str (coastal/witness path) e))) #_(accepted? (tufte/p ::query-execution (query (str (witness path) e) :count)))))
                            row
                            (drop row-length evidence))]
        new-row))))

(defn fill
  "Table -> Table"
  [table]
  (let [{:keys [S R E]} table]
    (-> table
        (assoc :S (reduce
                   (fn [new-s [path row]] (assoc new-s path (fill-row [path row] E)))
                   {}, S))
        (assoc :R (reduce
                   (fn [new-r [path row]] (assoc new-r path (fill-row [path row] E)))
                   {}, R)))))

(defn open-entries
  "Return a entries from R in `table` that do not appear in S. Return nil
  if no open entries are present"
  [{:keys [S R]}]
  (let [s-rows (set (vals S))
        r-rows (set (vals R))
        candidate-rows (set/difference r-rows s-rows)
        entries (filter (fn [[_ row]] (candidate-rows row)) R)]
    (set (keys entries))))

(defn closed?
  "Table -> Boolean"
  [{:keys [S R]}]
  (let [s-rows (set (vals S))
        r-rows (set (vals R))]
    (set/subset? r-rows s-rows)))

(defn close
  "Table -> Table"
  [{:keys [R] :as table}]
  (if-not (closed? table)
    (let [promotee (first (open-entries table))
          row (R promotee)]
      (-> table
          (update :R #(dissoc % promotee))
          (update :S #(assoc % promotee row))))
    table))

(defn close-totally
  "Keep closing a table until it is closed."
  [table]
  (loop [table table]
    (if (closed? table)
      table
      (recur (close table)))))

(defn add-path-condition
  "Table -> PathCondition -> Table"
  [table {:keys [accepted] :as path}]
  (let [prefixes (coastal/prefixes path)
        table-with-ce (update table :R #(assoc % path [accepted]))
        table-with-prefixes (reduce (fn [table* {:keys [accepted] :as path}]
                                      (if (or (get (:R table-with-ce) path)
                                              (get (:S table-with-ce) path))
                                        table*  ;; laziness being useful
                                        (update table* :R #(assoc % path [accepted]))))
                                    table-with-ce
                                    prefixes)
        filled-table (fill table-with-prefixes)]
    (close-totally filled-table)))

(defn make-evidence
  "String -> [String]"
  [word]
  (let [ns (range 0 (count word))]
    (map #(apply str (drop % word)) ns)))

(defn add-evidence
  "Table -> Evidence -> Table"
  [table evidence]
  (let [new-table (-> table
                      (update :E #(conj % evidence))
                      fill)]
    (close-totally new-table)))

(defn add-evidence*
  "Like `add-evidence`, but adds all the suffixes to E" ;; TODO Better suffix selection
  [table counter-example]
  (reduce (fn [table evidence]
            (add-evidence table evidence))
          table
          (make-evidence counter-example)))

(defn process-counter-example
  "Table -> String -> Table"
  [table counter-example]
  (let [unique-evidence (set (:E table))
        new-table
        (add-path-condition table (tufte/p ::s-membership-query
                                                 (coastal/query counter-example :count)))]
    (if (= 1 (count unique-evidence))
      (tufte/p ::add-evidence (add-evidence* new-table counter-example))
      (if-not (tufte/p ::check-determinism (sfa/deterministic? new-table))
        (do
          (log/trace "Table has non-deterministic transitions, applying evidence.")
          (tufte/p ::add-evidence (add-evidence* new-table counter-example)))
        (do
          (log/trace "Table is deterministic, not applying evidence.")
          new-table)))))

(defn shrink-table
  "Remove junk entries from R in `table`."
  [table iterations]
  (loop [table table
         iterations iterations]
    (let [dropout (nth (keys (:R table)) ;; select a key to drop
                       (int (rand (count (:R table)))))
          new-table (update-in table [:R] dissoc dropout)
          t (count (:R table))
          nt (count (:R new-table))
          eqv? (sfa/equivalent? (sfa/make-sfa* table)
                                (sfa/make-sfa* new-table))
          next (if eqv? new-table table)]
      (print \.)
      ;; ensure the number of nodes decreases by one
      (assert (= (dec t) nt))

      ;; loop until budget exhausted
      (if (< 0 iterations)
        (recur next (dec iterations))
        (do
          (log/info "New R Size: " nt)
          next)))))
