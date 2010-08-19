(ns clojure-refactoring.support.vars-test
  (:use clojure-refactoring.support.vars :reload)
  (:use clojure-refactoring.support.source)
  (:use clojure.contrib.mock)
  (:use clojure.test))

(deftest does_var_call_fn
  (expect [get-source-from-cache (times 1 (returns '(+ 1 a)))]
          (is (does-var-call-fn? 'a 'a))))
