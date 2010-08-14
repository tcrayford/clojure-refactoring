(ns clojure-refactoring.extract-method-test
  (:use clojure-refactoring.extract-method
        clojure-refactoring.support.core :reload)
  (:use clojure.test clojure.contrib.str-utils))

(deftest fn_name
  (is (= (fn-name '(defn a [c] c)) 'a)))

(deftest fn_call
  (is (= (fn-call '(defn a [b] (+ 1 2))) '(a b))))


;; Acceptance level testing below
(deftest uses_variables_from_let
  (is (= (extract-method
          "(defn add [s]
(let [a 1] (+ a 1)))" "(+ a 1)" "add-number")
"(defn add-number [a] (+ a 1))

(defn add [s]
(let [a 1] (add-number a)))")))


(deftest works_in_list_comprehensions
	(is (= (extract-method
                "(defn add [s]
(for [x (re-split #\",\" s)] (Integer. x)))"
"(Integer. x)"
"to-i")
"(defn to-i [x] (Integer. x))

(defn add [s]
(for [x (re-split #\",\" s)] (to-i x)))")
)
(is (= (extract-method
        "(defn add [s]
(for [x (re-split #\",\" s)] (Integer. x)))"
"(re-split #\",\" s)"
"split-string")
"(defn split-string [s] (re-split #\",\" s))

(defn add [s]
(for [x (split-string s)] (Integer. x)))"
)))

(deftest no_duplicated_bindings
  (is (= (extract-method
          "(defn a [s] (if (.contains s \",\") 1 s))"
          "(if (.contains s \",\") 1 s)"
          "b")
"(defn b [s] (if (.contains s \",\") 1 s))\n\n(defn a [s] (b s))")))
