(ns clojure-refactoring.rename-fn-test
  (:use clojure.test)
  (:use clojure.contrib.mock)
  (:use clojure-refactoring.rename-fn))

(deftest renames-basic-function-call
  (is (= (rename-fn "(defn a [b] (+ b 1))" "a" "c") "(defn c [b] (+ b 1))"))
  (is (= (rename-fn "(defn c [d] (+ d 2))" "c" "z") "(defn z [d] (+ d 2))")))

(deftest renames-recursive-function-call
  (is (= (rename-fn "(defn f [n] (if (<= n 1) 1 (f (dec n))))" "f" "fact")
         "(defn fact [n] (if (<= n 1) 1 (fact (dec n))))")))
