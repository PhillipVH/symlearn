(ns constraint-processing.user
  (:require [clojure.pprint :refer [pprint]]
            [constraint-processing.learner :as learner]
            [constraint-processing.core :as paths]
            [constraint-processing.sfa :as sfa]
            [clojure.java.shell :as sh]))


(let [db (paths/load-db-from-prefix "learn-large-" 3)
      obs-table (learner/learn-with-coastal db)
      learnt (learner/build-sfa obs-table)]
  (sfa/sfa->img learnt)
  #_(sfa/complete? learnt))
