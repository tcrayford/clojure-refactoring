(ns clojure-refactoring.support.find-bindings-above-node-test
  (:use clojure-refactoring.support.find-bindings-above-node :reload)
  (:use clojure.test))

(use-fixtures :once #(time (%)))

(deftest extract_destructured_maps
  (is (= (extract-destructured-maps '[{a :a}])
         '[a]))
  (is (= (extract-destructured-maps '[{b :a}])
         '[b])))

(deftest munge_anonymous_fns
  (is (= (munge-anonymous-fn '#(inc %)) (munge-anonymous-fn '#(inc %)))))

(deftest let_bind
  (is (= (find-bindings-above-node '(defn myfn [a] (let [a 1] (+ 1 a)))
                                   '(+ 1 a))
         '[a])))

(deftest add_multiple_keys
  (testing "keys"
    (is (= (add-multiple-keys {:keys '[bar blah foo]})
           '(bar blah foo))))
  (testing "syms"
    (is (= (add-multiple-keys {:syms '[bar blah foo]}) '(bar blah foo))))
  (testing "strs"
    (is (= (add-multiple-keys {:strs '[bar blah foo]}) '(bar blah foo)))))

(deftest find_bindings
  (is (= (find-bindings-above-node
          '(defn myfn [a] (+ 1 a)) '(+ 1 a))
         '[a]))
  (is (= (find-bindings-above-node
          '(let [a 1] a) 'a)
         '[a]))
  (testing "nested binding"
    (is (= (find-bindings-above-node
            '(let [a 1] (let [b 2] (+ a b))) '(+ a b))
           '[a b])))
  (is (= (find-bindings-above-node
          '(do (let [a 1] (+ a 1)) (let [b 1] (+ 1 b)))
          '(+ 1 b))
         '[b]))
  (testing "destructured bindings"
    (is (= (find-bindings-above-node
            '(let [{a :a :as c :or {:a 1}} {:a 1}] (+ 1 a))
            '(+ 1 a))
           '[a c]))
    (is (= (find-bindings-above-node
            '(let [{:keys [a b]} {:a 1 :b 2}] (+ a b))
            '(+ a b))
           '[a b])))
  (testing "regex in find-bindings"
    (is (=
         (find-bindings-above-node
          '(defn add [s] (for [x (re-split #"," s)] (Integer. x)))
          '(re-split #"," s))
         '[s x]))))

(deftest do_blocks
  (is (= (find-bindings-above-node
          '(do (let [a 1] (do (inc (let [b 2] (+ a b))))))
          '(+ a b))
         '[a b])))

(deftest loop_as_bindings_with_anonymous_fns
  (is (= (find-bindings-above-node
          '(loop [foo 1]
             (recur #(inc foo)))
          '(recur #(inc foo)))
         '[foo])))

(deftest anonymous-fns-with-reader-macro
  (is (= (find-bindings-above-node '(let [a 1] #(inc %))
                                   '#(inc %))
         '[a])))
