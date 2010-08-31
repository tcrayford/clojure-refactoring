(ns clojure-refactoring.support.core-test
  (:use clojure-refactoring.support.core :reload)
  (:use clojure.test))

(defn fixture [f]
  (def test-fn-string
       "(defn valid-message? [msg]
  (partial every? #(and (:user msg) (:text msg) (:id msg))))")

  (def test-fn-node
       (read-string test-fn-string))
  (f))

(use-fixtures :once fixture #(time (%)))

(deftest fn_args
  (is (= (bindings test-fn-node) ['msg])))

(deftest sub_nodes
  (is (= (sub-nodes '(defn a [b] (+ b 1)))
         '((defn a [b] (+ b 1)) defn a [b] b (+ b 1) + b 1)))
  (is (some #{:a} (sub-nodes '(defn a [b] {:a 1 :b b}))))
  (is (not (some #{[:a 1]} (sub-nodes {:a 1}))))
  (is (= (sub-nodes #{:a :b})
         '(#{:a :b} :a :b))))

(deftest binding_form
  (is (= (bindings '(let [b 2] b)) '[b 2]))
  (is (= (bindings '(defn foo [a] a)) '[a])))

(deftest rec_contains?
  (is  (tree-contains? '(let [a 1] (let [b 2] (+ a b))) '(+ a b)))
  (is  (tree-contains? '(let [a 1] (+ a 2)) '(+ a 2)) true)
  (is (not (tree-contains? '(let [a 1] (if (= b a) true 0)) '(= b 12))))
  (is  (tree-contains? '(let [a 1] (if (= b a) true 0)) 'true)))

(deftest binding_node?
  (is (= (binding-node? '(let [a 1] a)) 'let))
  (is (= (binding-node? '(defn myfn [a] (+ 1 a))) 'defn))
  (is (= (binding-node? '(let [a 1] (let [b 2] (+ a b)))) 'let))
  (is (= (binding-node? (nth '(let [a 1] (let [b 2] (+ a b))) 2)) 'let)))

(deftest orf-test
  (is ((any-of? map? :a nil?) {:a 1}))
  (is ((any-of? map? :b) {:a 1})))

(deftest andf-test
  (is ((all-of? map? :a) {:a 1}))
  (is (not ((all-of? map? :b) {:a 1}))))

(deftest but_second
  (is (= (but-second [1 2 3]) [1 3])))
