(ns clojure-refactoring.destructuring-test
  (:use clojure-refactoring.destructuring :reload)
  (:use clojure.test))

(deftest is_map_lookup?
  (is (map-lookup? '(:foo a)))
  (is (not (map-lookup? '(a (:foo b)))))
  (is (map-lookup? '(b :foo)))
  (is (not (map-lookup? '(:foo :bar))))
  (is (not (map-lookup? '(:foo a :bar b)))))

(deftest find_lookups
  (is (= (find-lookups '(a (:foo b))) '#{(:foo b)}))
  (is (= (find-lookups '(defn a [b] (+ (:foo b) (:bar b)))) '#{(:foo b) (:bar b)})))

(deftest lookups->binding_map
  (is (= (lookups->binding-map '#{(:a a)}) '{a {a :a}}))
  (is (= (lookups->binding-map '#{(:a a) (:b a)}) '{a {a :a b :b}}))
  (is (= (lookups->binding-map '#{(a :a) (a :b)}) '{a {a :a b :b}})))


;;Integration level tests below here.
 (deftest destructure_map_test
   (is (= (destructure-map "(defn a [b] (:foo b))" "b")
          "(defn a [{foo :foo}] foo)\n"))
   (is (= (destructure-map "(defn a [b] (+ (:foo b) (:bar b)))" "b")
          "(defn a [{foo :foo, bar :bar}] (+ foo bar))\n"))
   (is (= (destructure-map "(defn a [b] (+ (b :foo) (b :bar)))" "b")
          "(defn a [{bar :bar, foo :foo}] (+ foo bar))\n")))
