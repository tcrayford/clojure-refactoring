(ns local_binding_test
  (:use clojure_refactoring.local_binding clojure.test))

(deftest wraps_code_as_local
  (is (= (local-wrap "(defn a [b] (+ b (/ b 1)))"
                     "(/ b 1)"
                     "c")
         "(defn a [b] (let [c (/ b 1)] (+ b c)))\n")))


