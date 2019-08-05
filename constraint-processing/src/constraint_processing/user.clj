(ns constraint-processing.user
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.java.shell :as sh]
            [constraint-processing.learner :as learner]
            [constraint-processing.core :as paths]))

(let [db (paths/load-db-from-prefix "tacas-parser-" 1)
      table (learner/learn-with-coastal db)
      sfa (learner/build-sfa table)]
  (learner/sfa->img sfa))









