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

(deftest is_defn?
  (is (= (is-defn? '(defn foo [] 1)) true)))

(deftest let_bind
  (is (= (find-bindings-above-node '(defn myfn [a] (let [a 1] (+ 1 a)))
                    '(+ 1 a))
         '[a])))

(deftest binding_form
  (is (= (binding-form '(let [b 2] b)) '[b 2]))
  (is (= (binding-form '(defn foo [a] a)) '[a])))

(deftest bound_symbols
  (is (= (bound-symbols '(let [b 2] b)) '[b]))
  (is (= (bound-symbols '(defn foo [a b] (+ a b))) '[a b])))

(deftest rec_contains?
  (is (= (rec-contains? '(let [a 1] (let [b 2] (+ a b))) '(+ a b)) true))
  (is (= (rec-contains? '(let [a 1] (+ a 2)) '(+ a 2)) true))
  (is (= (rec-contains? '(let [a 1] (if (= b a) true 0)) '(= b 12)) nil))
  (is (= (rec-contains? '(let [a 1] (if (= b a) true 0)) 'true) true)))


(deftest binding_node?
  (is (= (binding-node? '(let [a 1] a)) 'let))
  (is (= (binding-node? '(defn myfn [a] (+ 1 a))) 'defn))
  (is (= (binding-node? '(let [a 1] (let [b 2] (+ a b)))) 'let))
  (is (= (binding-node? (nth '(let [a 1] (let [b 2] (+ a b))) 2)) 'let)))

(deftest last_binding_form?
  (is (= (last-binding-form? '(let [a 1] a)) true))
  (is (= (last-binding-form? '(+ 1 2)) nil))
  (is (= (last-binding-form? '(let [a 1] (fn [z] (+ z a)))) false))
  (is (= (last-binding-form? '(let [a 1 (let [b 2] (+ a b))])))))

(deftest find_bindings
  (is (= (find-bindings-above-node '(defn myfn [a] (+ 1 a)) '(+ 1 a)) '[a]))
  (is (= (find-bindings-above-node '(let [a 1] a) 'a) '[a]))
  (is (= (find-bindings-above-node '(defn add [s] (if (.contains s "//") "3" s)) '(if (.contains s "//") "3" s)) '[s])))

(deftest nested_binding
  (is (= (find-bindings-above-node '(let [a 1] (let [b 2] (+ a b))) '(+ a b)) '[a b])))
