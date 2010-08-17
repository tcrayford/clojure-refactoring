(ns clojure-refactoring.support.vars-test
  (:use clojure-refactoring.support.vars :reload)
  (:use clojure-refactoring.support.namespaces)
  (:use clojure.test)
  (:use clojure.contrib.mock))

(use-fixtures :once #(time (%)))

(def a nil) ;; used to test does-ns-refer-to-var? below.

(deftest all_ns_that_refer_to
  (testing "it requires all of them"
    (expect [find-ns-in-user-dir (returns '[a])
             require-and-return (times 1 (returns 'a))
             does-ns-refer-to-var? (returns true)
             clojure-refactoring.support.paths/slime-find-file (returns "")]
            (doall (all-ns-that-refer-to 'b))))
  (testing "it is empty when there are no namespaces that resolve the var"
    (expect [find-ns-in-user-dir (returns '[a])
             require-and-return (returns 'a)
             does-ns-refer-to-var? (returns false)]
            (is (empty? (all-ns-that-refer-to 'a))))))

(deftest does_ns_refer_to_var
  (let [this-ns (find-ns 'clojure-refactoring.support.vars-test)]
    (is (does-ns-refer-to-var? this-ns #'a))
    (testing "same named var in another ns"
      (is (not (does-ns-refer-to-var?
                this-ns
                (find-var
                 'clojure-refactoring.support.replace-test/a)))))
    (testing "var named something that doesn't exist in the current ns"
      (is (not (does-ns-refer-to-var?
                this-ns
                (find-var
                 'clojure-refactoring.support.replace/line-from-var)))))
    (testing "non existent var"
      (is (not (does-ns-refer-to-var?
                this-ns
                (find-var
                 'clojure-refactoring.support.source-test/boo)))))))
