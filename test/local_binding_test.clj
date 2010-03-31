(ns local_binding_test
  (:use clojure.test)
  (:use clojure_refactoring.local_binding :reload))

(deftest wraps_code_as_local
  (is (= (local-wrap "(defn a [b] (+ b (/ b 1)))"
                     "(/ b 1)"
                     "c")
         "(defn a [b] (let [c (/ b 1)] (+ b c)))\n")))

(deftest wraps_in_a_local_let_block
  (is (= (local-wrap "(defn a [b] (let [c 1] (+ b (/ b c))))"
                     "(/ b c)"
                     "d")
         "(defn a [b] (let [c 1 d (/ b c)] (+ b d)))\n")))

(deftest let_wrap
  (is (= (let-wrap '(let [a 1] a) 1 'b)
         '(let [a 1 b 1] a))))
