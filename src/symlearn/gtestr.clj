(ns symlearn.gtestr
  (:require [clojure.string :as str]
            [clojure.java.shell :as sh]
            [org.satta.glob :as glob]
            [symlearn.intervals :as i])
  (:import (automata.sfa SFA SFAInputMove)))

(set! *warn-on-reflection* true)

;; conversion of a symbolic state machine into a gtestr grammar

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn escape-char [char]
  (case char
    \' "\\'"
    \\ "\\\\"
    char))

(defn transition->rule [^SFAInputMove transition]
  (let [from (.from transition)
        to (.to transition)
        guard (i/printable (.guard transition))
        expanded-range (char-range (i/left guard)
                                   (i/right guard))]
    (format "s%s :- '%s', s%s." from (str guard) to)
    #_(format "\\mathit{s%s} & \\rightarrow \\texttt{%s} \\mathit{s%s}\\\\[0.2ex]" from (str guard) to)
    #_(for [symbol expanded-range]
        (format "s%s :- '%s', s%s." from (escape-char symbol) to))))


(defn epsilon-rules [^SFA sfa]
  (map #(format "s%s :- epsilon." %)
       (.getFinalStates sfa)))

(defn sfa->gtestr [^SFA sfa]
  (let [rules (flatten (map transition->rule (.getTransitions sfa)))
        epsilon-rules (epsilon-rules sfa)
        header "gtestr :- assert(writer:token_separator(_,_,'') :- true).\n"
        grammar (str/join \newline (concat rules epsilon-rules))]
    (str header grammar)))

(defn regex->gtestr [regex]
  (let [machine (i/regex->sfa regex)]
    (sfa->gtestr machine)))

(defn save-grammar [grammar path]
  (spit path grammar))

;; compile gtestr

(defn compile-gtestr! []
  (sh/with-sh-dir "gtestr"
    (sh/sh "make")))

;; generate a test suite from a grammar file

(defn options->seq [options]
  (let [nested-seqs (map (fn [[k v]] [(str "--" (name k))
                                      (if-not (= true v) (str v))])
                         options)]
    (filter (complement nil?)
            (flatten nested-seqs))))

(defn generate-test-suite [grammar-file options]
  (sh/sh "cp" grammar-file "gtestr/")
  (sh/with-sh-dir "gtestr"
    (apply sh/sh (concat ["./gtestr" grammar-file]
                         (options->seq options)))))

(defn clean []
  (sh/with-sh-dir "gtestr"
    (sh/sh "rm" "-rf" "pos" "neg" "rand")))

(def negative-opts {:dir "neg"
                    :dl-mutation true
                    :rule true
                    :negative-only true
                    :rule-mutation true
                    :nll true
                    :strict 3
                    :no-explanations true})

(def positive-opts {:dir "pos"
                    :full-cdrc true
                    :strict 3
                    :no-explanations true})

(def random-opts {:dir "rand"
                  :random-size 10
                  :random-depth 25
                  :strict 3
                  :no-explanations true})

;; run a test suite over an oracle

(defn run-test-suite [{:keys [name ^SFA membership-oracle ^SFA hypothesis]}]
  (let [tests (glob/glob (format "gtestr/%s/**/*.hypothesis" name))]
    (println "Running " (count tests))
    (reduce (fn [counter-examples test]
              (let [input (->> test
                               slurp
                               str/trim
                               .toCharArray
                               (map char))]
                (if (not= (.accepts hypothesis input i/solver)
                          (.accepts membership-oracle input i/solver))
                  (conj counter-examples (str/join input))
                 counter-examples)))
            nil
            tests)))

;; public equivalence API

(defn check-equivalence [{:keys [^SFA oracle ^SFA hypothesis]}]
  ;; delete old tests TODO cache
  (clean)

  ;; generate the grammar
  (save-grammar (sfa->gtestr hypothesis) "hypothesis.pl")

  ;; generate the test suites
  (doseq [opts [positive-opts negative-opts random-opts]]
    (println (generate-test-suite "hypothesis.pl" opts)))

  ;; run the test suites and report counter examples
  (set (apply concat
              (map #(run-test-suite {:name %
                                     :membership-oracle oracle
                                     :hypothesis hypothesis})
                   ["pos" "neg" "rand"]))))
