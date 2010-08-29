(ns clojure-refactoring.support.source-test
  (:use clojure-refactoring.support.source :reload)
  (:import clojure-refactoring.support.source.NameSpaceCacheEntry)
  (:use [clojure-refactoring.support.parsley :only [parse]])
  (:use clojure-refactoring.test-helpers)
  (:use clojure-refactoring.support.paths)
  (:use clojure.test)
  (:require [clojure-refactoring.support replace replace-test])
  (:use clojure.contrib.mock))

(use-fixtures :once #(time (%)))

(def a nil) ;; used to test does-ns-refer-to-var? below.

(def cache-with-one-entry
     (atom {'a (NameSpaceCacheEntry. 0
                                     (parse "(+ 1 2)"))}))

(defn test-entry-from-cache []
  (entry-from-cache 'a))

(deftest caching
  (binding [ns-cache cache-with-one-entry]
    (fact "with an in time entry, doesn't add a new entry"
          (test-entry-from-cache)
          (provided
           [cache-entry-in-time? (times 1 (returns true))
            filename-from-ns (returns "")
            new-ns-entry (times 0)])))

  (binding [ns-cache cache-with-one-entry]
    (fact "adds a new entry when the entry isn't in time"
          (test-entry-from-cache)
          (provided
           [cache-entry-in-time? (times 1 (returns false))
            new-ns-entry (times 1 (returns nil))])))

  (binding [ns-cache (atom {})]
    (fact "always adds a new entry with an empty cache"
          (test-entry-from-cache)
          (provided [filename-from-ns (returns "")
                     slurp (returns "")
                     new-ns-entry (times 1)]))))

(deftest namespaces_who_refer_to
  (know "it requires all the namespaces in the user dir"
        (doall (namespaces-who-refer-to 'b))
        (provided
         [find-ns-in-user-dir (returns '[a])
          require-and-return (times 1 (returns 'a))
          does-ns-refer-to-var? (returns true)]))

  (fact "it is empty when there are no namespaces that resolve the var"
        (is (empty? (namespaces-who-refer-to 'a)))
        (provided
         [find-ns-in-user-dir (returns '[a])
          require-and-return (returns 'a)
          does-ns-refer-to-var? (returns false)])))

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
