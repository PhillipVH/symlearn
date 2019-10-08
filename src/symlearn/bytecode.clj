(ns symlearn.bytecode
  (:require [insn.core :as insn]
            [symlearn.intervals :as intervals]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(def class-data
  {:name 'my.pkg.Adder
   :fields [{:flags #{:public :static}, :name "VALUE", :type :long, :value 42}]
   :methods [{:flags #{:public}, :name "add", :desc [:long :long]
              :emit [[:getstatic :this "VALUE" :long]
                     [:lload 1]
                     [:ladd]
                     [:lreturn]]}
             {:flags #{:public}, :name "parse", :desc [[:char] :boolean]
              :emit [[:ldc 0]
                     [:ireturn]]}]})


(def parser (insn/new-instance class-data))

(-> parser
    (.parse (into-array Character/TYPE "hello world")))

(comment (magic-parser-creation))
(sh/sh "javac" "Parser.java")


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
  (doseq [bc parser-bytecode]
    (let [fields (parse-bytecode bc)]
      (if (empty? fields)
        (println "Could not parse " bc)
        (do
          (println (parse-bytecode bc))
          (println bc)
          (println))))))

(defn parse-bytecode
  [bytecode]
  (let [bc-pattern #"(\d+):\s+([a-zA-Z0-9_]*)\s*(#?\d+)?(?:,\s)?(\d+)?(?:\s+)?(//.+)?"

        fields #_[label opcode arg1 arg2 hint] (drop 1 (re-find bc-pattern bytecode))]
    fields))
