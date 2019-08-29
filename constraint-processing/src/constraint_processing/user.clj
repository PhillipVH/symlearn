(ns constraint-processing.user
  (:require [com.rpl.specter :as sp]
            [constraint-processing.learner :as learner]
            [constraint-processing.paths :as paths]
            [constraint-processing.sfa :as sfa]
            [constraint-processing.ranges :as ranges]
            [constraint-processing.table :as table]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [taoensso.tufte :as tufte]
            [constraint-processing.coastal :as coastal])
  (:import LearnLarge
           TacasParser
           PaperExample))

(defmacro with-profiling
  "Wrap `body` in a profiling form."
  [& body]
  `(taoensso.tufte/profile {} ~@body))

(defonce stats-accumulator (tufte/add-basic-println-handler! {:ns-pattern "*"}))

(defn learn
  "Learn with `parse-fn` as the oracle, halting when both a fixed-point is
  reached with the SFAs." ;; TODO Bootstrap the learning process with another table and path db
  [parse-fn & [{:keys [display?] :or {display? true}}]]
  (loop [previous-sfa nil
         depth 1]
    (let [new-table (learner/learn {:depth depth, :parse-fn parse-fn})
          new-sfa (table/table->sfa new-table)]
      (when display
        (sfa/sfa->img new-sfa))
      (if (= previous-sfa new-sfa)
        {:sfa new-sfa, :table new-table, :depth depth}
        (recur new-sfa (inc depth))))))

(comment

  (learn #(TacasParser/parse %))

  (learn #(TacasParser/parse %) {:display? false})

  ;; TACAS -- works well! learning stalls at depth 3, finds evidence at depth 4
  (with-profiling
    (let [table (learner/learn {:depth 3, :parse-fn #(TacasParser/parse %)})
          learnt (table/table->sfa table)]
      (sfa/sfa->img learnt)))

  ;; Paper example -- works well!
  (with-profiling
    (let [table (learner/learn {:depth 4, :parse-fn #(PaperExample/parse %)})
          learnt (table/table->sfa table)]
      (sfa/sfa->img learnt)))

  ;; Learn Large -- gets very slow
  (with-profiling
    (let [table (learner/learn {:depth 4, :parse-fn #(LearnLarge/parse %)})
          learnt (table/table->sfa table)]
      (sfa/sfa->img learnt)))

 )
