(ns symlearn.gtestr
  (:require [clojure.string :as str]
            [clojure.java.shell :as sh]
            [org.satta.glob :as glob]
            [symlearn.intervals :as intervals]))

;; conversion of a symbolic state machine into a gtestr grammar

(def ascii-printable (apply intervals/make-interval [\u0020 \u007E]))

(defn only-printable [interval]
  (intervals/intersection ascii-printable interval))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn escape-char [char]
  (case char
    \' "\\'"
    \\ "\\\\"
    char))

(defn transition->rule [transition]
  (let [from (.from transition)
        to (.to transition)
        guard (only-printable (.guard transition))
        expanded-range (char-range (intervals/left guard)
                                   (intervals/right guard))]
    (for [symbol expanded-range]
      (format "s%s :- '%s', s%s." from (escape-char symbol) to))))

(defn epsilon-rules [sfa]
  (map #(format "s%s :- epsilon." %)
       (.getFinalStates sfa)))

(defn regex->gtestr [regex]
  (let [machine (intervals/regex->sfa* regex)
        rules (flatten (map transition->rule (.getTransitions machine)))
        epsilon-rules (epsilon-rules machine)
        header "gtestr :- assert(writer:token_separator(_,_,'') :- true).\n"
        grammar (str/join "\n" (concat rules epsilon-rules))]
    (str header grammar)))

(defn save-grammar [grammar path]
  (spit path grammar))

(comment
  (regex->gtestr "[ab]|[cd]e+"))

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
                    :no-explanations true})

(def positive-opts {:dir "pos"
                    :full-cdrc true
                    :no-explanations true})

(def random-opts {:dir "rand"
                  :random-size 5
                  :random-depth 25
                  :no-explanations true})

(comment
  (save-grammar (regex->gtestr "[ab]|[cd]e+") "test.pl")
  (clean)
  (generate-test-suite "test.pl" negative-opts))

;; run a test suite over an oracle

(defn run-test-suite [{:keys [name expected-output membership-oracle]}]
  (let [tests (glob/glob (format "gtestr/%s/**/*.test" name))]
    (doseq [test tests]
      (let [input (str/trim (slurp test))]
        (if-not (= expected-output (.accepts membership-oracle
                                             (map char (.toCharArray input)) intervals/solver))
          (println "counter example: " input)
          (print \.))))))

(comment
  (run-test-suite {:name "pos"
                   :expected-output true
                   :membership-oracle (intervals/regex->sfa "[ab]|[cd]e+")}))
