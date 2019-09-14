(ns symlearn.z3
  (:require [clojure.string :as str]
            [clojure.java.shell :as sh]
            [symlearn.coastal :as coastal]))

(defn- char-witness
  [constraints]
  (let [const-decl "(declare-const a Int)\n"
        assertions (for [[_ op bound] constraints]
                     (let [negation (= "!=" op)
                           op (if (or (= "!=" op)
                                      (= "==" op)) "=" op)]
                       (if negation
                         (format "(assert (not (%s a %s)))\n" op bound)
                         (format "(assert (%s a %s))\n" op bound))))
        sat-call "(check-sat)\n"
        model-call "(get-model)\n"
        prog (str const-decl (str/join "\n" assertions) sat-call model-call)
        result (str/split (:out (sh/sh "z3" "-in" :in prog)) #"\n")
        char-section (->> result
                          reverse
                          (drop 1)
                          (take 1)
                          (map str/trim))
        trimmed (map #(subs % 0 (dec (.length %))) char-section)]
    (map #(char (Integer/parseInt %)) trimmed)))

(defn make-context
  []
  "(declare-const a Int)\n")

(defn assert
  [ctx op bound]
  (let [negation (= "!=" op)
        op (if (or (= "!=" op)
                   (= "==" op)) "=" op)]
    (if negation
      (str ctx (format "(assert (not (%s a %s)))\n" op bound))
      (str ctx (format "(assert (%s a %s))\n" op bound)))))

(defn solve
  [ctx]
  (let [prog (str ctx "(check-sat)\n(get-model)\n")]
    (:out (sh/sh "z3" "-in" :in prog))))

;; (-> (make-context)
;;      (assert ">=" 2)
;;      (assert "!=" 3)
;;      (assert ">=" 500)
;;      (assert ">=" 500)
;;      (solve))

(defn model
  "Return a (String) model for the given `path-condition`."
  [path-condition]
  (str/join (flatten (map #(char-witness %) (vals (:constraints path-condition))))))
