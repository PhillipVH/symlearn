(ns constraint-processing.paths-test
  (:require [constraint-processing.paths :as paths]
            [clojure.test :refer :all]))


(deftest single-constraint-path-test
   (let [path [[50 60]]]
     (testing "has one prefix, itself"
       (is (= 1 (count (paths/prefixes path))))
       (is (= [[50 60]] (first (paths/prefixes path)))))
     (testing "has one suffix, itself"
       (is (= 1 (count (paths/suffixes path))))
       (is (= [[50 60]] (first (paths/suffixes path)))))
     (testing "has a concrete representation of [50]"
       (is (= [50] (paths/make-concrete path))))))
