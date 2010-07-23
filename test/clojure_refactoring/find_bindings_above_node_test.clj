(ns clojure-refactoring.find-bindings-above-node-test
  (:use clojure-refactoring.find-bindings-above-node :reload)
  (:use clojure.test))

(deftest let_bind
  (is (= (find-bindings-above-node '(defn myfn [a] (let [a 1] (+ 1 a)))
                                   '(+ 1 a))
         '[a])))

(deftest find_bindings
  (is (= (find-bindings-above-node '(defn myfn [a] (+ 1 a)) '(+ 1 a)) '[a]))
  (is (= (find-bindings-above-node '(let [a 1] a) 'a) '[a]))
  (testing "nested binding"
    (is (= (find-bindings-above-node '(let [a 1] (let [b 2] (+ a b))) '(+ a b)) '[a b])))
  (is (= (find-bindings-above-node '(do (let [a 1] (+ a 1)) (let [b 1] (+ 1 b)))
                                   '(+ 1 b)) '[b])))
