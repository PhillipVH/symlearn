(ns symlearn.intervals-test
  (:require [symlearn.intervals :as intervals]
            [clojure.test :refer :all])
  (:import theory.characters.CharPred
           com.google.common.collect.ImmutableList
           org.apache.commons.lang3.tuple.ImmutablePair
           benchmark.regexconverter.RegexConverter
           (RegexParser RegexNode
                        RegexParserProvider)))

(deftest bounds-test
  (let [interval (intervals/make-interval \a \g)]
    (testing "[a g]"
     (testing "has a as left"
       (is (= \a (intervals/left interval))))
     (testing "has g as right"
       (is (= \g (intervals/right interval)))))))

(deftest negation-test
  (let [i1 (intervals/make-interval \a \g)]
    (testing "[a g]"
      (testing "has a negation of [h z]"
        (is (= (intervals/make-interval \h \z) (intervals/negate i1)))))))




