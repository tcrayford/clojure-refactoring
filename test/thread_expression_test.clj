(ns thread_expression_test
  (:use clojure_refactoring.thread_expression clojure_refactoring.core clojure.test clojure.contrib.str-utils))

(defn fixture [f]
  (def start-src "(reduce + (map #(Integer. %) s))")
  (def end-src "(->> s (map #(Integer. %)) (reduce +))\n")
(f))

(use-fixtures :once fixture #(time (%)))

(deftest thread_last
  (is (= (thread-last start-src) end-src)))


(deftest thread_first
  (is (= (thread-first "(+ (* c 1.8) 32)") "(-> c (* 1.8) (+ 32))\n")))