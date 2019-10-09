(ns symlearn.bytecode
  (:require [insn.core :as insn]
            [symlearn.intervals :as intervals]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(comment (magic-parser-creation))
(sh/sh "javac" "Parser.java")

(defn parse-bytecode
  [bytecode] ;; FIXME istore-1 becomes istore 1 
  (let [bc-pattern #"(\d+):\s+([a-zA-Z0-9_]*)\s*(#?\d+)?(?:,\s)?(\d+)?(?:\s+)?(?://\sint\s(\d+))?"
        [label opcode arg1 arg2 hint] (drop 1 (re-find bc-pattern bytecode))
        opcode (str/replace opcode "_" "-")
        arg1 (when arg1 (if (str/starts-with? arg1 "#") (read-string hint) (read-string arg1)))
        arg2 (when arg2 (read-string arg2))
        label+opcode [(read-string label) (keyword opcode)]
        with-args (cond-> label+opcode
                    arg1 (conj arg1)
                    arg2 (conj arg2))]
    (if (or #_(str/starts-with? opcode "iconst")
            (str/starts-with? opcode "iload")
            (str/starts-with? opcode "aload")
            (str/starts-with? opcode "istore"))
      (let [[opcode arg] (str/split opcode #"-")]
        [(Integer/parseInt label) (keyword opcode) (read-string arg)])
      with-args)))

(defn java->bytecode
  []
  (let [bytecode-dump (str/split (:out (sh/sh "javap" "-c" "Parser.class")) #"\n" )
        start 22
        finish 87
        bc-pattern #"(\d+)\s+(.*)\s+(\d+)\s(//.*)" ;; label: op dest
        parser-bytecode (->> bytecode-dump
                             (drop start)
                             (take finish)
                             (reverse)
                             (drop 1)
                             (reverse)
                             (map str/trim))]
    (map parse-bytecode parser-bytecode)))


(defn jump-targets
  [bytecode]
  (let [bytecode (java->bytecode)
        jumpers (filter (fn [[_ opcode _ _ _]]
                          (or (= "goto" (name opcode))
                              (str/starts-with? (name opcode) "if")))
                        bytecode)]
    (set (map last jumpers))))

(defn drop-nils
  [[opcode arg1 arg2]]
  (if (nil? arg1)
    [opcode]
    (if (nil? arg2)
      [opcode arg1]
      [opcode arg1 arg2])))

(defn emit-insn
  [bytecode]
  (let [targets (jump-targets bytecode)]
    (reduce (fn [bytecode [label opcode arg1 arg2]]
              (if (targets label)
                (do
                  (println "jumping to " label)
                  (conj bytecode [:mark label] (drop-nils [opcode arg1 arg2])))
                (conj bytecode (drop-nils [opcode arg1 arg2]))))
            []
            bytecode)))

(comment


  
  (println (jump-targets (java->bytecode)))
  ;; iconst istore iload
  ;; lconst
  (clojure.pprint/pprint (emit-insn (java->bytecode)))


  (def class-data
    (let [label-1 (insn.util/label)]
      {:name 'my.pkg.Adder
       :fields [{:flags #{:public :static}, :name "VALUE", :type :long, :value 42}]
       :methods [{:flags #{:public}, :name "parse", :desc [[:char] :boolean]
                  :emit (emit-insn (java->bytecode))}]}))


  (def parser (insn/new-instance class-data))

  (-> parser
      (.parse 0)))

