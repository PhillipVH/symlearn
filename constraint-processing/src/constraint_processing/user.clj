(ns constraint-processing.user
  (:require [clojure.pprint :refer [pprint]]
            [constraint-processing.learner :as learner]
            [constraint-processing.core :as paths]
            [constraint-processing.sfa :as sfa]
            [clojure.java.shell :as sh]
            [taoensso.tufte :as tufte]
            [constraint-processing.ranges :as ranges]
            [com.rpl.specter :as sp]
            [clojure.set :as set])
  (:import LearnLarge
           TacasParser
           PaperExample))

;; TACAS -- errors at depth three
(tufte/profile {} (binding [learner/*parse-fn* #(TacasParser/parse %)]
   (let [db (paths/load-db-from-prefix "tacas-parser-" 1)
         table (learner/learn-with-coastal-dynamic db 3)
         learnt (learner/build-sfa table)]
     (pprint (meta table))
     (sfa/sfa->img learnt))))

;; Paper example -- works well!
(tufte/profile {} (binding [learner/*parse-fn* #(PaperExample/parse %)]
                    (let [db (paths/load-db-from-prefix "paper-example-" 1)
                          table (learner/learn-with-coastal-dynamic db 4)
                          learnt (learner/build-sfa table)]
                      (pprint table)
                      (sfa/sfa->img learnt))))

;; Learn Large -- gets very slow
(tufte/profile {} (binding [learner/*parse-fn* #(LearnLarge/parse %)]
   (let [db (paths/load-db-from-prefix "learn-large-" 1)
         table (learner/learn-with-coastal-dynamic db 6)
         learnt (learner/build-sfa table)]
     (pprint table)
     (sfa/sfa->img learnt))))

(defonce stats-accumulator (tufte/add-basic-println-handler! {:ns-pattern "*"}))
