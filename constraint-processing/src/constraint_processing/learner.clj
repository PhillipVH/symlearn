(ns constraint-processing.learner
  (:require [constraint-processing.core :as paths])
  (:import Parser))

(defn make-obs-table
  []
  {:S [[]]
   :R []
   :E []})


(Parser/parse (into-array Integer/TYPE [0 2 3]))



(count paths/)
