(ns clojure-refactoring.core-test
  (:use clojure-refactoring.core :reload)
  (:use clojure.test))

(defn fixture [f]
  (def test-fn-string
       "(defn valid-message? [msg]
  (partial every? #(and (:user msg) (:text msg) (:id msg))))")

  (def test-fn-node
       (read-string test-fn-string))
  (f))

(use-fixtures :once fixture)

(deftest fn_args
  (is (= (fn-args test-fn-node) ['msg])))

(deftest is_defn?
  (is (= (is-defn? '(defn foo [] 1)) true)))


(deftest binding_form
  (is (= (extract-binding-form '(let [b 2] b)) '[b 2]))
  (is (= (extract-binding-form '(defn foo [a] a)) '[a])))

(deftest unique_vec
  (is (= (unique-vec [1 2 3 1 2 3]) [1 2 3])))

(deftest bound_symbols
  (is (= (bound-symbols '(let [b 2] b)) '[b]))
  (is (= (bound-symbols '(defn foo [a b] (+ a b))) '[a b])))

(deftest rec_contains?
  (is  (rec-contains? '(let [a 1] (let [b 2] (+ a b))) '(+ a b)) true)
  (is  (rec-contains? '(let [a 1] (+ a 2)) '(+ a 2)) true)
  (is (not (rec-contains? '(let [a 1] (if (= b a) true 0)) '(= b 12))))
  (is  (rec-contains? '(let [a 1] (if (= b a) true 0)) 'true) true))

(deftest binding_node?
  (is (= (binding-node? '(let [a 1] a)) 'let))
  (is (= (binding-node? '(defn myfn [a] (+ 1 a))) 'defn))
  (is (= (binding-node? '(let [a 1] (let [b 2] (+ a b)))) 'let))
  (is (= (binding-node? (nth '(let [a 1] (let [b 2] (+ a b))) 2)) 'let)))

(deftest last_binding_form?
  (is  (last-binding-form? '(let [a 1] a)))
  (is (not (last-binding-form? '(+ 1 2))))
  (is (not (last-binding-form? '(let [a 1] (fn [z] (+ z a))))))
  (is (not (last-binding-form? '(let [a 1 (let [b 2] (+ a b))])))))

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

