(ns clojure-refactoring.thread-expression-test
  (:use clojure-refactoring.thread-expression :reload)
  (:use clojure-refactoring.support.core
        clojure.test clojure.contrib.str-utils))

(use-fixtures :once #(time (%)))

(deftest but_second
  (is (= (but-second [1 2 3]) [1 3])))

(deftest threaded?_test
  (is (threaded? '(->> foo bar arr)))
  (is (threaded? '(-> foo bar arr)))
  (is (not (threaded? '(arr barr arr))))
  (is (not (threaded? :atom))))

(deftest threading-fns-from-type-test
  (is (= (threading-fns-from-type '->)
         {:position-f second
          :all-but-position-f but-second}))
  (is (= (threading-fns-from-type '->>)
         {:position-f last
          :all-but-position-f butlast})))

(deftest finish_threading
  (is (= (finish-threading '(x y z) '(a b c) '->>)
         '(z (x y) a b c)))
  (is (= (finish-threading '(x y z) '(a b c) '->)
         '(y (x z) a b c))))

;; Integration level tests below here

(def start-src "(reduce + (map #(Integer. %) s))")
(def end-src "(->> (map #(Integer. %) s) (reduce +))\n")

(deftest thread_last
  (is (= (thread-last start-src) end-src)))

(deftest thread_first
  (is (= (thread-first "(+ (* c 1.8) 32)") "(-> (* c 1.8) (+ 32))\n")))

(deftest thread_unthread_last
  (is (= ( thread-unthread
           "(->> sym (/ 1))")
         "(/ 1 sym)\n"))
  (is (= (thread-unthread
          "(->>
    1
    (rec-contains? (rest node))
    (for [sym *binding-forms*])
    (some #(= % true))
    (not))")
         "(not\n  (some\n    #(= % true)\n    (for [sym *binding-forms*] (rec-contains? (rest node) 1))))\n")))

(deftest thread_unthread_first
  (is (= (thread-unthread
          "(-> 1
 (/ 2))")

         "(/ 1 2)\n"))
  (is (= (thread-unthread
          "(-> 1\n(/ 2) (+ 1))")
         "(+ (/ 1 2) 1)\n")))
