(ns constraint-processing.user
  (:require [clojure.pprint :refer [pprint]]
            [constraint-processing.learner :as learner]
            [constraint-processing.core :as paths]
            [constraint-processing.sfa :as sfa]
            [clojure.java.shell :as sh]
            [constraint-processing.ranges :as ranges]
            [com.rpl.specter :as sp])
  (:import LearnLarge
           TacasParser))


(binding [learner/parse-fn #(TacasParser/parse %)]
    (let [db (paths/load-db-from-prefix "tacas-parser-" 2)
          obs-table (learner/learn-with-coastal db 3)
          learnt (learner/build-sfa obs-table)]
      (sfa/sfa->img learnt)))


