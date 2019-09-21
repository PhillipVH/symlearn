(ns symlearn.demo
  (:gen-class)
  (:require [clojure.java.io :as io]
            [ring.adapter.jetty :as jetty]
            [ring.util.response :refer [file-response]]
            [ring.middleware.params :refer [wrap-params]]
            [symlearn.coastal :as coastal]
            [symlearn.learner :as learner]
            [symlearn.table :as table]
            [symlearn.sfa :as sfa])
  (:import LearnLarge
           PaperExample))

(def parse-fn-for
  {"LearnLarge" #(LearnLarge/parse %)
   "PaperExample" #(PaperExample/parse %)})

(def config-for
  {"LearnLarge" "LearnLarge.xml"
   "PaperExample" "PaperExample.xml"})

(defn learn-parser-to-depth
  [parser depth]
  (assert (config-for parser) (str parser " is not a known parser"))
  (let [dse-engine (coastal/start-coastal! (config-for parser))
        {:keys [table]} (learner/learn {:depth depth, :parse-fn (parse-fn-for parser)})
        hypothesis (table/table->sfa table)]
    (sfa/sfa->img hypothesis)
    (prn hypothesis)
    (coastal/stop-coastal! dse-engine)))

(defn handler [request]
  (let [{:strs [parser depth]} (:query-params request)]
    (learn-parser-to-depth parser (read-string depth))
    (file-response "tmp.png")))

(def app
  (wrap-params handler))

(defn -main
  [& args]
  (jetty/run-jetty app {:port 3000}))
