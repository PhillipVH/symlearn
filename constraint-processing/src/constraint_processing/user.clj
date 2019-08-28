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
  `(tufte/profile {} ~@body))

;; TACAS -- works well! learning stalls at depth 3, finds evidence at depth 4
(with-profiling
  (let [table (learner/learn 4 #(TacasParser/parse %))
        learnt (table/table->sfa table)]
    (sfa/sfa->img learnt)))

;; Paper example -- works well!
(with-profiling
  (let [table (learner/learn 4 #(PaperExample/parse %))
        learnt (table/table->sfa table)]
    (pprint table)
    (sfa/sfa->img learnt)))

;; Learn Large -- gets very slow
(with-profiling
  (let [table (learner/learn 2 #(LearnLarge/parse %))
        learnt (table/table->sfa table)]
    (pprint table)
    (sfa/sfa->img learnt)))

(defonce stats-accumulator (tufte/add-basic-println-handler! {:ns-pattern "*"}))
