(ns clojure-refactoring.rename-binding-test
  (:use clojure-refactoring.rename-binding clojure-refactoring.core clojure.test clojure.contrib.str-utils))

(use-fixtures :once #(time (%)))

(deftest renames_single_let
  (is (= (rename-binding "(let [a 1] a)" "a" "b")
         "(let [b 1] b)\n"))
  (is (= (rename-binding "(let [b 2] b)" "b" "a")
         "(let [a 2] a)\n")))

(deftest renames_multiple_let
  (is (= (rename-binding "(let [a 1 b 2] (+ a b))" "a" "c")
         "(let [c 1 b 2] (+ c b))\n")))
