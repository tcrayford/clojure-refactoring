(ns clojure-refactoring.support.source-test
  (:use clojure-refactoring.support.source :reload)
  (:import clojure-refactoring.support.source.NameSpaceCacheEntry)
  (:use clojure-refactoring.support.parsley)
  (:use clojure-refactoring.test-helpers)
  (:use clojure.test)
  (:require clojure-refactoring.support.replace)
  (:require clojure-refactoring.support.replace-test)
  (:use clojure.contrib.mock))

(use-fixtures :once #(time (%)))

(def a nil)

(defmacro modified? [reference & exprs]
  "Checks if a reference is modified whilst running exprs.
   Use can be made readable by doing
   (modified reference :during expr)"
  `(let [intial# @~reference]
     (do ~@exprs)
     (not= @~reference intial#)))

(use-fixtures :once #(time (%)))

(def a nil) ;; used to test does-ns-refer-to-var? below.

(deftest caching
  (testing "cache with an entry in time"
   (binding [ns-cache (atom {'a (NameSpaceCacheEntry. 0
                                                      (parse "(+ 1 2)")
                                                      ["a"])})]
     (expect [last-modified (times 1 (returns 0))
              filename-from-ns (returns "")
              slurp (returns "")
              new-ns-entry (times 0)]
             (parsley-from-cache 'a))))

  (testing "cache with an entry not in time"
   (binding [ns-cache (atom {'a (NameSpaceCacheEntry. 0
                                                      (parse "(+ 1 2)")
                                                      ["a"])})]
     (expect [last-modified (times 1 (returns 1))
              filename-from-ns (returns "")
              slurp (returns "")
              new-ns-entry (times 1 (returns nil))]
             (entry-from-ns-cache 'a))))
  (testing "empty cache"
    (binding [ns-cache (atom {})]
      (expect [filename-from-ns (returns "")
               slurp (returns "")
               new-ns-entry (times 1)]
              (entry-from-ns-cache 'a)))))

(deftest parsley_extract_symbols
  (is (= (extract-symbols (parse "(+ 1 2)")) #{"+"})))

(deftest filename_from_ns
  (let [path "clojure_refactoring/support/namespaces.clj"]
    (expect [clojure-refactoring.support.paths/slime-find-file
             (returns path
                      (has-args [#(= path %)] (times 1)))]
            (is (= (filename-from-ns 'clojure-refactoring.support.namespaces)
                   path)))))

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
  (let [this-ns (find-ns 'clojure-refactoring.support.source-test)]
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
