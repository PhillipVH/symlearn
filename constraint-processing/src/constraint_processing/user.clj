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

;; TACAS -- works well! learning stalls at depth 3, finds evidence at depth 4
(tufte/profile {} (binding [table/*parse-fn* #(TacasParser/parse %)]
   (let [table (learner/learn 3)
         learnt (sfa/table->sfa table)]
     (pprint (meta table))
     (sfa/sfa->img learnt))))

;; Paper example -- works well!
(tufte/profile {} (binding [table/*parse-fn* #(PaperExample/parse %)]
                    (let [table (learner/learn 4)
                          learnt (sfa/table->sfa table)]
                      (pprint table)
                      (sfa/sfa->img learnt))))

;; Learn Large -- gets very slow
(tufte/profile {} (binding [table/*parse-fn* #(LearnLarge/parse %)]
   (let [table (learner/learn 1)
         learnt (sfa/table->sfa table)]
     (pprint table)
     (sfa/sfa->img learnt))))

(defonce stats-accumulator (tufte/add-basic-println-handler! {:ns-pattern "*"}))
