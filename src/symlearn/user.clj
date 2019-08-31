(ns symlearn.user
  (:require [com.rpl.specter :as sp]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [symlearn.coastal :as coastal]
            [symlearn.learner :as learner]
            [symlearn.paths :as paths]
            [symlearn.ranges :as ranges]
            [symlearn.sfa :as sfa]
            [symlearn.table :as table]
            [taoensso.tufte :as tufte])
  (:import LearnLarge
           TacasParser
           PaperExample))

(set! *warn-on-reflection* true)

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
    (let [{new-table :table} (learner/learn {:depth depth, :parse-fn parse-fn})
          new-sfa (table/table->sfa new-table)]
      (if (= previous-sfa new-sfa)
        {:sfa new-sfa, :table new-table, :depth depth}
        (do
          (when display?
            (sfa/sfa->img new-sfa))
          (recur new-sfa (inc depth)))))))

(comment

  ;; TACAS parser incremental learning
  (learn #(TacasParser/parse %))

  ;; Don't display images as the learning proceeds
  (learn #(TacasParser/parse %) {:display? false})

  ;; Learning Symbolic Automata parser, incremental learning
  (learn #(PaperExample/parse %))
  (learn #(PaperExample/parse %) {:display? false})

  ;; Large Automata parser, incremental learning
  (learn #(LearnLarge/parse %))

  ;; TACAS -- works well! learning stalls at depth 3, finds evidence at depth 4
  (with-profiling
    (let [coastal (coastal/start-coastal! "TACAS.xml")
          {:keys [table db]} (learner/learn {:depth 4, :parse-fn #(TacasParser/parse %)})
          learnt (table/table->sfa table)]
      (sfa/sfa->img learnt)
      (coastal/stop-coastal! coastal)))

  ;; Paper example -- works well!
  (with-profiling
    (let [coastal (coastal/start-coastal! "PaperExample.xml")
          {:keys [table db]} (learner/learn {:depth 3, :parse-fn #(PaperExample/parse %)})
          learnt (table/table->sfa table)]
      (println "Database Size: " (count db))
      (coastal/stop-coastal! coastal)
      (sfa/sfa->img learnt)))

  ;; Learn Large -- gets very slow
  (with-profiling
    (let [coastal (coastal/start-coastal! "LearnLarge.xml")
          {:keys [table db]} (learner/learn {:depth 2, :parse-fn #(LearnLarge/parse %)})
          learnt (table/table->sfa table)]
      (println "Database Size: " (count db))
      (coastal/stop-coastal! coastal)
      (sfa/sfa->img learnt)))

  )
