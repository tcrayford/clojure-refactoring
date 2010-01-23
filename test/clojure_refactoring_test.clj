(ns clojure_refactoring_test
  (:use clojure_refactoring.core clojure.test))

(defn fixture [f]
  (def test-fn-string

       "(defn valid-message? [msg]
  (partial every? #(and (:user msg) (:text msg) (:id msg))))")

(def test-fn-node (read-string test-fn-string))
(f))

(deftest fn_args
  (is (= (fn-args test-fn-node) ['msg])))

(deftest node_readlines
  (is (= (count (node-readlines test-fn-node)) 2)))


(use-fixtures :once fixture)

