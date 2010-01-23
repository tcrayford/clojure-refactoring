(ns extract_method_test
  (:use clojure_refactoring.extract_method clojure_refactoring.core clojure.test clojure.contrib.str-utils))

(defn fixture [f]
  (def start-src
       "(defn add [s] (reduce + (map #(Integer. %) s)))")
  (def end-src
       "(defn add-numbers [s] (reduce + (map #(Integer. %) s)))

(defn add [s] (add-numbers s))")
(def extract-string "(reduce + (map #(Integer. %) s))")
(def helper (extract-method start-src
                         extract-string
                         "add-numbers"))
(def f-string (str "(defn add [s a] " extract-string ")"))
(f))


(use-fixtures :once fixture)

(deftest extract_method
  (is (= helper
         end-src))
  (is (=
       (extract-method f-string
                       extract-string
                       "add-numbers")
       (str "(defn add-numbers [s] " extract-string ")

(defn add [s a] (add-numbers s))"))))