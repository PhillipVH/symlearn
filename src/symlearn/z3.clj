(ns symlearn.z3
  (:require [clojure.string :as str]
            [clojure.java.shell :as sh]
            [symlearn.coastal :as coastal]))

(defn- prepare-z3-cmd
  [string]
  (let [const-decl "(declare-const a Int)\n"
        [_ path] (coastal/refine-string string)
        assertions (for [[_ op bound] (coastal/path->constraints path)]
                     (format "(assert (%s a %s))\n" (if (= "==" op) "=" op) bound))
        sat-call "(check-sat)\n"
        model-call "(get-model)\n"]
    (str const-decl (str/join "\n" assertions) sat-call model-call)))

(defn generate-witness
  []
  (sh/sh "z3" "-in" :in (prepare-z3-cmd "b")))
