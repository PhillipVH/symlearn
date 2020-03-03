(ns symlearn.core
  (:require [taoensso.timbre :as log]
            [taoensso.tufte :as tufte]
            [symlearn.coastal :as c]
            [symlearn.intervals :as intervals]
            [symlearn.table :as table]
            [symlearn.time :as time]
            [symlearn.sfa :as sfa]))

(defn learn
  "Learn `target` to `depth`."
  [{:keys [target depth-limit timeout-ms oracle] :or {oracle :coastal}}]
  (log/info "Learning" {:target target, :depth depth-limit, :timeout-ms timeout-ms})

  ;; install the membership oracle
  (tufte/p
   ::install-membership-oracle
   (reset! table/mem-queries 0)
   (reset! c/s-mem-queries 0)
   (reset! table/target-sfa (sfa/regex->sfa* target))
   (c/install-parser! target))

  ;; parser installed, begin learning
  (let [target-sfa (.minimize (intervals/regex->sfa target) intervals/solver)
        equivalence-queries (atom 0)
        start (System/currentTimeMillis)
        max-depth (atom 1)
        eq-time (atom 0)]
    (loop [table (table/make-table)]
      (let [conjecture (sfa/make-sfa* table)
            new-table (loop [depth 1]
                        ;; update the max depth
                        (swap! max-depth (fn [max]
                                           (if (> depth max)
                                             depth
                                             max)))
                        (let [eq-start (System/currentTimeMillis)
                              counter-example
                              (if (= :perfect oracle)
                                (tufte/p ::equivalence-query (c/check-equivalence-perfect target-sfa conjecture))
                                (tufte/p ::equivalence-query
                                         (c/check-equivalence-timed!
                                          {:depth depth,
                                           :target target
                                           :candidate conjecture
                                           :timeout-ms (time/ms-to-timeout start
                                                                        (System/currentTimeMillis)
                                                                      (time/ms->m timeout-ms))})))]
                          ;; update equivalence time
                          (swap! eq-time + (- (System/currentTimeMillis) eq-start))

                          (cond
                            ;; no counter example, search deeper or yield table
                            (nil? counter-example)
                            (if (< depth depth-limit)
                              (recur (inc depth))
                              table)

                            ;; equivalence check timed out
                            (= counter-example ::timeout)
                            ::timeout

                            ;; found a counter example, process and return new table
                            :else
                            (do
                              (log/trace "Got counter example(s):" counter-example)
                              (swap! equivalence-queries inc)
                              (tufte/p ::apply-counter-example
                                       (table/process-counter-example table (first counter-example)))))))]
        (cond
          ;; equivalence check timed out
          (= ::timeout new-table)
          (do
            (log/info "\nTimeout")
            {:table table
             :queries {:eqv @equivalence-queries
                       :mem @table/mem-queries
                       :s-mem @c/s-mem-queries
                       :max-eqv-depth @max-depth}
             :time (- (System/currentTimeMillis) start)
             :eq-time @eq-time
             :status :incomplete
             :equivalence :timeout})

          ;; the two SFAs are entirely equivalent
          (sfa/equivalent? target-sfa (sfa/make-sfa* new-table))
          (do
            (println)
            (log/info "Total Equivalence")
            {:table new-table
             :queries {:eqv @equivalence-queries
                       :mem @table/mem-queries
                       :s-mem @c/s-mem-queries
                       :max-eqv-depth @max-depth}
             :time (- (System/currentTimeMillis) start)
             :eq-time @eq-time
             :status :complete
             :equivalence :total})

          ;; the candidate SFA is equivalent to a bounded
          (= table new-table)
          (do
            (println)
            (log/info "Bounded Equivalence:" {:depth depth-limit})
            {:table new-table
             :queries {:eqv @equivalence-queries
                       :mem @table/mem-queries
                       :s-mem @c/s-mem-queries
                       :max-eqv-depth @max-depth}
             :time (- (System/currentTimeMillis) start)
             :eq-time @eq-time
             :status :complete
             :equivalence :bounded})

          ;; continue learning
          :else
          (let [now (System/currentTimeMillis)
                diff (- now start)
                minutes (/ diff 60000)]
            (if (> minutes (time/ms->m timeout-ms))
              (do
                (println)
                (log/info "Timeout (Global)")
                {:table table
                 :queries {:eqv @equivalence-queries
                           :mem @table/mem-queries
                           :eq-time @eq-time
                           :s-mem @c/s-mem-queries
                           :max-eqv-depth @max-depth}
                 :time (- (System/currentTimeMillis) start)
                 :status :incomplete
                 :equivalence :timeout})
              (do
                (log/trace "Beginning next learning cycle (time left:" (float (- (time/ms->m timeout-ms) minutes)) "minutes)")
                (print \.)
                (recur new-table)))))))))
