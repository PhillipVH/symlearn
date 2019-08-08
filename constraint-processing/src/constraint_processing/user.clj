(ns constraint-processing.user
  (:require [clojure.pprint :refer [pprint]]
            [constraint-processing.learner :as learner]
            [constraint-processing.core :as paths]
            [constraint-processing.sfa :as sfa]
            [clojure.java.shell :as sh]
            [constraint-processing.ranges :as ranges]
            [com.rpl.specter :as sp]))


(let [db (paths/load-db-from-prefix "tacas-parser-" #_"learn-large-" 1)
      obs-table (learner/learn-with-coastal db)
      learnt (learner/build-sfa obs-table)]
  (sfa/sfa->img learnt)
  #_(sfa/complete? learnt))



