(ns symlearn.bytecode
  (:require [insn.core :as insn]
            [symlearn.intervals :as intervals]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(defn parse-bytecode
  "Extract a bytecode from its text representation."
  [bytecode]
  (let [bc-pattern #"(\d+):\s+([a-zA-Z0-9_]*)\s*(#?\d+)?(?:,\s)?(\d+)?(?:\s+)?(?://\sint\s(\d+))?"
        [label opcode arg1 arg2 hint] (drop 1 (re-find bc-pattern bytecode))
        opcode (str/replace opcode "_" "-")
        arg1 (when arg1 (if (str/starts-with? arg1 "#") (read-string hint) (read-string arg1)))
        arg2 (when arg2 (read-string arg2))
        label+opcode [(read-string label) (keyword opcode)]
        with-args (cond-> label+opcode
                    arg1 (conj arg1)
                    arg2 (conj arg2))]
    (if (or (str/starts-with? opcode "iload")
            (str/starts-with? opcode "aload")
            (str/starts-with? opcode "istore"))
      (let [[opcode arg] (str/split opcode #"-")]
        [(Integer/parseInt label) (keyword opcode) (read-string arg)])
      with-args)))

(defn regex->bytecode
  "Return the bytecode representation for a function that parses `regex`."
  [regex]
  (let [java-src (intervals/sfa->java (intervals/regex->sfa regex) "examples.tacas2017" "Parser" false)]
    (spit "Parser.java" java-src)
    (sh/sh "javac" "Parser.java")
    (let [dump (map str/trim (str/split (:out (sh/sh "javap" "-c" "Parser.class")) #"\n"))
          parse-dump (drop-while #(not (str/includes? % "parse(char[])")) dump)
          stripped (reverse (drop 1 (reverse (drop 2 parse-dump))))
          bc (map parse-bytecode stripped)]
      bc)))

(defn jump-targets
  "Return a set of labels that are the targets of branching instructions in the bytecode."
  [bytecode]
  (let [bytecode (java->bytecode)
        jumpers (filter (fn [[_ opcode _ _ _]]
                          (or (= "goto" (name opcode))
                              (str/starts-with? (name opcode) "if")))
                        bytecode)]
    (set (map last jumpers))))

(defn drop-nil-args
  "Remove args that are nil from the instruction."
  [[opcode arg1 arg2]]
  (cond-> [opcode]
    arg1 (conj arg1)
    arg2 (conj arg2)))

(defn emit-insn
  "Transform `bytecode` into the ASM/insn compatible format."
  [bytecode]
  (let [targets (jump-targets bytecode)]
    (reduce (fn [bytecode [label opcode arg1 arg2]]
              (if (targets label)
                (conj bytecode [:mark label] (drop-nils [opcode arg1 arg2]))
                (conj bytecode (drop-nils [opcode arg1 arg2]))))
            []
            bytecode)))

(comment

  ;; the bytecode for the parse fn
  (regex->bytecode "hel")

  ;; the class representating the parser
  (def class-data
    {:name 'symlearn.java.Parser
     :fields [#_{:flags #{:public :static}, :name "VALUE", :type :long, :value 42}]
     :methods [{:flags #{:public}, :name "parse", :desc [:int :boolean]
                :emit (emit-insn (regex->bytecode "l"))}]})

  ;; create an instance of the parser
  (def parser (insn/new-instance class-data))

  ;; parse some input
  (.parse parser (into-array Character/TYPE "hello"))

  )
