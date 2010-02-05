(ns destructuring_test
  (:use clojure_refactoring.destructuring clojure.test))

(deftest is_map_lookup?
  (is (= (is-map-lookup? '(:foo a)) true))
  (is (= (is-map-lookup? '(a (:foo b))) false)))

(deftest find_lookups
  (is (= (find-lookups '(:foo a)) '#{(:foo a)}))
  (is (= (find-lookups '(a (:foo b))) '#{(:foo b)}))
  (is (= (find-lookups '(defn a [b] (+ (:foo b) (:bar b)))) '#{(:foo b) (:bar b)})))

(deftest lookups->binding_map
  (is (= (lookups->binding-map '#{(:a a)}) '{a {a :a}}))
  (is (= (lookups->binding-map '#{(:a a) (:b a)}) '{a {a :a b :b}})))

 (deftest destructure_map_test
   (is (= (destructure-map "(defn a [b] (:foo b))" "b")
          "(defn a [{foo :foo}] foo)\n"))
   (is (= (destructure-map "(defn a [b] (+ (:foo b) (:bar b)))" "b")
          "(defn a [{foo :foo, bar :bar}] (+ foo bar))\n")))
