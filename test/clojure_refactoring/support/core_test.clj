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
  (is (= (fn-args test-fn-node) ['msg])))

(deftest sub_nodes
  (is (= (sub-nodes '(defn a [b] (+ b 1)))
         '((defn a [b] (+ b 1)) defn a [b] b (+ b 1) + b 1)))
  (is (some #{:a} (sub-nodes '(defn a [b] {:a 1 :b b}))))
  (is (not (some #{[:a 1]} (sub-nodes {:a 1})))))

(deftest is_defn?
  (is (defn? '(defn foo [] 1))))

(deftest binding_form
  (is (= (extract-binding-form '(let [b 2] b)) '[b 2]))
  (is (= (extract-binding-form '(defn foo [a] a)) '[a])))

(deftest unique_vec
  (is (= (unique-vec [1 2 3 1 2 3]) [1 2 3])))

(deftest bound_symbols
  (is (= (bound-symbols '(let [b 2] b)) '[b]))
  (is (= (bound-symbols '(defn foo [a b] (+ a b))) '[a b])))

(deftest rec_contains?
  (is  (rec-contains? '(let [a 1] (let [b 2] (+ a b))) '(+ a b)))
  (is  (rec-contains? '(let [a 1] (+ a 2)) '(+ a 2)) true)
  (is (not (rec-contains? '(let [a 1] (if (= b a) true 0)) '(= b 12))))
  (is  (rec-contains? '(let [a 1] (if (= b a) true 0)) 'true))
  (is (rec-contains? '(defn what [s] (re-split #"," s))
                     '(re-split #"," s))))

(deftest binding_node?
  (is (= (binding-node? '(let [a 1] a)) 'let))
  (is (= (binding-node? '(defn myfn [a] (+ 1 a))) 'defn))
  (is (= (binding-node? '(let [a 1] (let [b 2] (+ a b)))) 'let))
  (is (= (binding-node? (nth '(let [a 1] (let [b 2] (+ a b))) 2)) 'let)))

(deftest last_binding_form?
  (is (last-binding-form? '(let [a 1] a)))
  (is (not (last-binding-form? '(+ 1 2))))
  (is (not (last-binding-form? '(let [a 1] (fn [z] (+ z a))))))
  (is (not (last-binding-form? '(let [a 1 (let [b 2] (+ a b))])))))
