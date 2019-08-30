(ns symlearn.paths-test
  (:require [symlearn.paths :as paths]
            [clojure.test :refer :all]))

(deftest single-constraint-path-test
   (let [path [[50 60]]]
     (testing "has one prefix, itself"
       (is (= 1 (count (paths/prefixes path))))
       (is (= path (first (paths/prefixes path))))
       (is (= true (paths/prefix? [] path)))) ;; the empty string is not returned by prefixes
     (testing "has one suffix, itself"
       (is (= 1 (count (paths/suffixes path))))
       (is (= path (first (paths/suffixes path)))))
     (testing "has a concrete representation of [50]"
       (is (= [50] (paths/make-concrete path)))
       (is (= [50] (paths/mixed->concrete path))))
     (testing "has [] as a prefix"
       (is (= true (paths/prefix? [] path))))))
