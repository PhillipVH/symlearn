(ns symlearn.integration-tests)

(comment

  (defn membership-integration-tests
    "Test the integration between the learner and the membership oracle."
    []
    (log/info "Testing Membership Oracle")
    (install-parser! "abc|g")
    (let [should-accept (query "abc")
          should-reject (query "ggwp")]
      (assert (:accepted should-accept))
      (assert (not (:accepted should-reject))))

    (install-parser! "ggwp?")
    (let [should-reject (query "abc")
          should-accept (query "ggwp")]
      (assert (not (:accepted should-reject)))
      (assert (:accepted should-accept)))

    (let [results (for [i (range 1000)]
                    (query (str i)))]
      (assert (= 1000 (count results))))
    (log/info "All Membership Tests Pass"))

  (defn equivalence-integration-tests
    "Test the integration between the learner and the equivalence oracle."
    []
    (log/info "Testing Equivalence Oracle")
    (let [ce (check-equivalence-timed! {:depth 2
                                        :target "gz"
                                        :candidate (intervals/regex->sfa "g")
                                        :timeout-ms (m->ms 4)})]
      (assert (= #{"gz"} ce)))

    (let [ce (check-equivalence-timed! {:depth 2
                                        :target "g(z|a)"
                                        :candidate (intervals/regex->sfa "g")
                                        :timeout-ms (m->ms 4)})]
      (assert (= #{"gz" "ga"} ce)))

    (let [ce (check-equivalence-timed! {:depth 2
                                        :target "g"
                                        :candidate (intervals/regex->sfa "ga")
                                        :timeout-ms (* 1000 2)})]
      (assert (= ::timeout ce)))

    (let [ce (check-equivalence-timed! {:depth 2
                                        :target "g(z|a)"
                                        :candidate (intervals/regex->sfa "g")
                                        :timeout-ms (m->ms 4)})]
      (assert (= #{"gz" "ga"} ce)))
    (log/info "All Equivalence Tests Pass"))

  (defn learner-integration-tests
    []
    (log/info "Testing Learner")
    (let [{:keys [table queries time status equivalence]} (learn {:target "b|aa"
                                                                  :depth-limit 1
                                                                  :timeout-ms (m->ms 30)})
          conjecture (make-sfa* table)
          bounded-target (intervals/regex->sfa "b")
          total-target (intervals/regex->sfa "b|aa")]
      (assert (equivalent? bounded-target conjecture))
      (assert (not (equivalent? total-target conjecture)))
      (assert (= :complete status))
      (assert (= :bounded equivalence)))

    (let [{:keys [table queries time status equivalence]} (learn {:target "[^\"]+"
                                                                  :depth-limit 2
                                                                  :timeout-ms (m->ms 30)})
          target (intervals/regex->sfa "[^\"]+")
          conjecture (make-sfa* table)]
      (assert (equivalent? target conjecture))
      (assert (= :complete status))
      (assert (= :total equivalence)))

    (let [{:keys [table queries time status equivalence]} (learn {:target "[^\"]+"
                                                                  :depth-limit 2
                                                                  :timeout-ms  100})
          target (intervals/regex->sfa "[^\"]+")
          conjecture (make-sfa* table)]
      (assert (not (equivalent? target conjecture)))
      (assert (= :incomplete status))
      (assert (= :timeout equivalence)))
    (log/info "All Learner Tests Pass"))

  (defn regex->sfa*-tests
    []
    (assert (= ::timed-out (regex->sfa* "\\w{5,255}")))
    (assert (= ::unsupported-regex (regex->sfa* "\\p{N}]")))
    (assert (equivalent? (intervals/regex->sfa "b")
                         (regex->sfa* "b"))))

  (defn integration-tests
    "Checks integration between the learner and the equivalence + membership oracles."
    []
    (log/info "Running Integration Tests")
    (membership-integration-tests)
    (equivalence-integration-tests)
    (learner-integration-tests)
    (regex->sfa*-tests)
    (log/info "All Integration Tests Pass")))

