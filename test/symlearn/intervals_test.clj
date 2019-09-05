(ns symlearn.intervals-test
  (:require [symlearn.intervals :as intervals]
            [clojure.test :refer :all]))

(deftest bounds-test
  (let [interval (intervals/make-interval \a \g)]
    (testing "[a g]"
     (testing "has a as left"
       (is (= \a (intervals/left interval))))
     (testing "has g as right"
       (is (= \g (intervals/right interval)))))))

(deftest negation-test
  (let [i1 (intervals/make-interval Character/MIN_VALUE \g)]
    (testing "[\\u0000 g]"
      (testing "has a negation of [h \\uFFFF]"
        (is (= (intervals/make-interval \h Character/MAX_VALUE)
               (intervals/negate i1)))))))
