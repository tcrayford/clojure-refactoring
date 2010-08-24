(ns clojure-refactoring.support.source-test
  (:use clojure-refactoring.support.source :reload)
  (:import clojure-refactoring.support.source.CachedSource)
  (:use [clojure-refactoring.support core paths])
  (:use clojure.test clojure.contrib.mock)
  (:require [clojure-refactoring.support.namespaces :as namespaces])
  (:use clojure.contrib.def)
  (:require clojure-refactoring.support.replace-test))

(use-fixtures :once #(time (%)))

(def a nil)

(defmacro modified? [reference & exprs]
  "Checks if a reference is modified whilst running exprs.
   Use can be made readable by doing
   (modified reference :during expr)"
  `(let [intial# @~reference]
     (do ~@exprs)
     (not= @~reference intial#)))

(def ns-cache-has-one-entry)

(defn proxy-file [time]
  (proxy [java.io.File] ["~/"] (lastModified [] time)
         (getCanonicalPath [] "absolute-path")))

(deftest in_time
  (testing "true when last modified is equal to time"
    (binding [namespaces/ns-cache (atom {'clojure-refactoring.support.source-test 0})]
            ;; Given ns-cache has one entry and it's in time
            (is (in-time? #'a (CachedSource. 0
                                         ""
                                         "~/"
                                         {})))))
  (testing "false when last modified is greater than original time"
    (expect [new-file (returns (proxy-file 1))]
            (is (not (in-time? #'a (CachedSource. 0
                                              ""
                                              "~/"
                                              {})))))))

(deftest absolute_file_from_var
  (expect [file-from-var (times 1 (returns "filename"))
           new-file (times 1 (returns (proxy-file 0)))
           slime-find-file (times 1 (returns ""))]
          (is (absolute-file-from-var #'a))))

(deftest new_cached_source
  (testing "empty cache means we run get-source-from-var"
   (binding [source-cache (atom {})]
     (expect [get-source-from-var (times 1 (returns "(+ 1 2)"))
              absolute-file-from-var (returns (proxy-file 0))]
             (is (= (:source (new-cached-source 'a))
                    '(+ 1 2)))))))

(deftest get_source_from_cache
  (testing "if the cache is invalid, reload the source"
    (binding [source-cache (atom {'a (CachedSource. 0 "(+ a b)" "~/" {})})]
      (expect [get-source-from-var (returns "(+ 1 2)")
               new-file (returns (proxy-file 1))
               absolute-file-from-var (returns (proxy-file 1))]
              (is (= (get-source-from-cache #'a) '(+ 1 2))))))

  (testing "if the cache is valid, return the cached value"
    (binding [source-cache
              (atom {#'a (CachedSource. 0 "(+ a b)" "~/" {})})]
      (expect [get-source-from-var (times 0)
               namespaces/force-ns-name (returns 'a)
               namespaces/get-cached-time (returns 0)]
              (is (= (get-source-from-cache #'a) "(+ a b)"))))))

(deftest caching
  (expect [get-source-from-var (times 1 (returns "(+ 1 2)"))
           absolute-file-from-var (times 1 (returns (proxy-file 0)))]
          (cache #'a))
  (expect [get-source-from-var (returns "(+ 1 2)")
           absolute-file-from-var (returns (proxy-file 0))]
          (binding [source-cache (atom {})]
            (is (modified? source-cache :during (cache #'a)))))
  (expect [get-source-from-var (times 1 (returns "(+ 1 2)"))
           absolute-file-from-var (times 1 (returns (proxy-file 0)))]
          (is (cache-source #'a))))
