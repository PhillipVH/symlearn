(ns symlearn.evaluation
  (:require [symlearn.coastal :as coastal]
            [symlearn.sfa :as sfa]
            [symlearn.time :as time]
            [symlearn.core :as symlearn]
            [symlearn.intervals :as intervals]
            [symlearn.table :as table]
            [taoensso.timbre :as log]
            [taoensso.tufte :as profile]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [aero.core :as aero])
  (:import automata.sfa.SFA))

(set! *warn-on-reflection* true)

;; evaluation

(defn load-benchmark
  [filename]
  (-> filename
      slurp
      str/split-lines))

(defn evaluate!
  "A wrapper around `learn` that insulates us from some of the harsh realities of
  evaluation. If `target` causes the underlying `RegexParserProvider` to crash, or
  the provider takes too long to produce a parser. Calls `learn` as the last step,
  given no error state."
  [{:keys [target depth timeout-ms oracle] :or {oracle :coastal}}]
  (let [?sfa (sfa/regex->sfa* target)]
    (cond
      (= ::timed-out ?sfa)
      {:target target
       :equivalence ::timed-out}

      (= ::unsupported-regex ?sfa)
      {:target target
       :equivalence ::unsupported-regex}

      :else
      (-> (symlearn/learn {:target target,
                           :depth-limit depth
                           :timeout-ms timeout-ms
                           :oracle oracle})
          (assoc :target target)))))

(defn evaluate-benchmark!
  "Evaluate a given benchmark file, learning to `max-depth`, with a timeout
  on each equivalence check of `timeout-ms`."
  [benchmark max-depth timeout-ms oracle]
  (let [regexes (str/split-lines (slurp benchmark))
        n (count regexes)
        i (atom 0)
        results (reduce (fn [results target]
                          (let [evaluation (evaluate! {:target target
                                                       :depth max-depth
                                                       :timeout-ms timeout-ms
                                                       :oracle oracle})
                                new-results (conj results evaluation)]
                            (log/info (str "<" @i "/" n ">"))
                            (swap! i inc)
                            (log/info (dissoc evaluation :table))
                            (Thread/sleep 5000) ;; rest a bit between experiments
                            (spit "results/results.edn" (pr-str new-results))
                            new-results))
                        []
                        regexes)]
    results))

(defn prepare-evaluations
  [{:keys [benchmark-file benchmark-config n-evaluations]}]
  (let [subjects (str/split-lines (slurp (io/resource benchmark-file)))
        n-subjects (count subjects)
        subjects-per-file (quot n-subjects n-evaluations)
        remainder (rem n-subjects n-evaluations)
        {:keys [global-timeout]} (aero/read-config (io/resource benchmark-config))
        completion-estimate (float (* global-timeout
                                      (+ (/ subjects-per-file
                                            n-evaluations)
                                         remainder)))]
    (doall
     (map-indexed (fn [idx subjects]
                    (let [filename (format "%s-%d" benchmark-file idx)
                          content (str/join "\n" subjects)]
                      (spit filename content)))
                  (partition subjects-per-file subjects)))
    {:subjects-per-file subjects-per-file
     :completion-estimate completion-estimate}))

(defn evaluate-regexlib
  ([]
   (log/info "Starting regexlib Evaluation")
   (let [{:keys [max-string-length oracle global-timeout]}
         (aero/read-config "results/benchmark-config.edn")

         results (evaluate-benchmark! "results/benchmark.re"
                                      max-string-length
                                      (time/m->ms global-timeout)
                                      oracle)]
     (sh/sh "mkdir" "-p" "results")
     (spit "results/results.edn" (pr-str results))
     (log/info "Finished regexlib Evaluation")))
  ([file depth timeout-ms oracle]
   (log/info "Starting regexlib Evaluation")
   (let [results (evaluate-benchmark! file
                                      depth
                                      timeout-ms
                                      oracle)]
     (sh/sh "mkdir" "-p" "results")
     (spit "results/results.edn" (pr-str results))
     (log/info "Finished regexlib Evaluation"))))

;; reporting

(defn load-results
  []
  (let [files (map #(format "results/help-%d/results.edn" %) (range 8))
        results (map (comp read-string slurp) files)]
    results))

(defn pad-results
  []
  (reduce (fn [results result-set]
            (let [n-results (count result-set)
                  n-pad (- 25 n-results)]
              (conj results (concat result-set (repeat n-pad {:padding true})))))
          []
          (load-results)))

(defn merge-results
  []
  (apply concat (pad-results)))

(defn escape-string
  [string]
  (let [escapes (map char-escape-string string)
        chars (str/split string #"")]
    (reduce (fn [string idx]
              (if (nth escapes idx)
                (str string (nth escapes idx))
                (str string (nth chars idx))))
            ""
            (range (count string)))))

(defn benchmarks->csv
  [benchmarks]
  (let [indexed (map-indexed (fn [idx benchmark]
                               (assoc benchmark :target-id idx))
                             benchmarks)
        header (str/join \, ["TargetID"
                             "Model.StateCount"
                             "Model.TransitionCount"
                             "Model.CEDistance"
                             "Memb.Queries"
                             "Memb.SQueries"
                             "Equiv.CE"
                             "Equiv.Time"
                             "Equiv.MaxDepth"
                             "Learner.Type"
                             "Time(ms)"
                             "TargetRegex"
                             "\n"])]
    (reduce (fn [csv benchmark]
              (if (:padding benchmark)
                (str csv (:target-id benchmark) ",Padding\n")
                (if (or (= ::timed-out (:equivalence benchmark))
                        (= "Regex is not supported" (:equivalent? benchmark))
                        (nil? (:table benchmark)))
                  (str csv (:target-id benchmark) ",Timeout\n")
                  (let [candidate ^SFA (sfa/make-sfa* (:table benchmark))
                        state-count (.stateCount candidate)
                        target (:target benchmark)
                        target-sfa (sfa/regex->sfa* target)
                        ce-distance (count (first (coastal/check-equivalence-perfect target-sfa candidate)))
                        transition-count (.getTransitionCount candidate)
                        target-id (:target-id benchmark)
                        {:keys [s-mem mem eqv max-eqv-depth]} (:queries benchmark)
                        line (str/join \, [target-id
                                           state-count
                                           transition-count
                                           ce-distance
                                           mem
                                           s-mem
                                           eqv
                                           (:eq-time benchmark)
                                           max-eqv-depth
                                           (:equivalence benchmark)
                                           (:time benchmark)
                                           (escape-string target)])]
                    (str csv line "\n")))))
            header
            indexed)))

(defn path->str
  [path]
  (let [constraints (map intervals/constraint-set->CharPred (coastal/constraints path))]
    (str/join constraints)))

(defn row->str
  [row]
  (reduce (fn [row accepted]
            (str row (if accepted "+" "-") ""))
          ""
          row))

(defn entry->str
  [[path row]]
  (let [path-str (path->str path)
        row-str (row->str row)]
    (format "%-51s | %s \n" path-str row-str)))

(defn table->str
  [table]
  (doseq [entry (:S table)]
    (print (entry->str entry)))
  (println "---------------------------------------------")
  (doseq [entry (:R table)]
    (print (entry->str entry))))

(defn -main
  []
  (evaluate-regexlib)
  (coastal/stop!)
  (shutdown-agents))

(comment

  ;; get some statistics about the interal workings
  (profile/add-basic-println-handler! {})

  ;; load the (stratified + filtered) regexlib benchmark
  (def bench (load-benchmark "regexlib-stratified.re"))

  ;; learn a target and report profiling information
  (def evaluation
    (profile/profile
     {}
     (evaluate!
      {:target (nth bench 20)
       :depth 30
       :timeout-ms (coastal/m->ms 10)
       :oracle :perfect})))

  ;; get some stats from the evaluation
  (pprint (select-keys evaluation [:queries :time :eq-time]))

  (def trimmed (table/shrink-table (:table evaluation) 1000))

  (log/set-level! :info)

  ;; stop all coastal instances
  (coastal/coastal-emergency-stop)

  (def sfas (doall (map coastal/regex->sfa* bench)))

  ;; evaluate the full benchmark
  (def eval-fullsetset (future (evaluate-regexlib "regexlib-stratified.re" 30 (coastal/m->ms 10) :perfect)))

  ;; cancel the evaluation
  (future-cancel eval-fullsetset)

  ;; load the results and check some stats
  (def results (read-string (slurp "results/results.edn")))
  (count results)

  ;; write results to CSV
  (spit "results.csv" (coastal/benchmarks->csv results))

  ;; this might be useful one day
  (java.util.Collections/binarySearch [0 1 2 8] 8 compare))
