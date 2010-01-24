(ns clojure_refactoring_test
  (:use clojure_refactoring.core clojure.test))

(defn fixture [f]
  (def test-fn-string

       "(defn valid-message? [msg]
  (partial every? #(and (:user msg) (:text msg) (:id msg))))")

(def test-fn-node (read-string test-fn-string))
(f))

(use-fixtures :once fixture)

(deftest fn_args
  (is (= (fn-args test-fn-node) ['msg])))

(deftest let_bind
  (is (= (find-bindings '(defn myfn [a] (let [a 1] (+ 1 a)))
                    '(+ 1 a))
         '[a 1])))

(deftest binding_node?
  (is (= (binding-node? '(let [a 1] a)) true)))

(deftest no_other_binding_forms
  (is (= (no-other-binding-forms '(let [a 1] a)) false))
  (is (= (no-other-binding-forms '(+ 1 2)) nil)))

(deftest find_bindings
  (is (= (find-bindings '(defn myfn [a] (+ 1 a)) '(+ 1 a)) '[a])))