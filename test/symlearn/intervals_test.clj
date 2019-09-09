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

(deftest regex->sfa-test
  (let [sfa (intervals/regex->sfa "a|b")]
    (testing "(regex->sfa \"a|\"b)"
      (testing "has four states"
        (is (= 4 (intervals/state-count sfa))))
      (testing "has 0 as the initial state"
        (is (= 0 (intervals/initial-state sfa))))
      (testing "has two final states, 1 and 2"
        (is (= 2 (count (intervals/final-states sfa))))
        (is (= #{1 2} (intervals/final-states sfa))))
      (testing "goes to trap state on [\\u0000-`c-\\uFFFF]"
        (let [transitions (filter
                           #(= (intervals/to %) 3)
                           (intervals/transitions-from (intervals/regex->sfa "a|b") 0))]
          (is (= 1 (count (map intervals/guard transitions))))
          (is (= (intervals/union
                  (intervals/make-interval \u0000 \`)
                  (intervals/make-interval \c \uFFFF))
                 (first (map intervals/guard transitions)))))))))




