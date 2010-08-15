(ns clojure-refactoring.support.source-test
  (:use clojure-refactoring.support.source :reload)
  (:import clojure-refactoring.support.source.CachedSource)
  (:use clojure-refactoring.support.core)
  (:use clojure.test clojure.contrib.mock)
  (:require clojure-refactoring.support.replace-test))

(use-fixtures :once #(time (%)))

(def a nil) ;; used to test does-ns-refer-to-var? below.

(defn proxy-file [time]
  (proxy [java.io.File] ["~/"] (lastModified [] time)
         (getCanonicalPath [] "absolute-path")))

(deftest in_time
  (testing "true when last modified is equal to time"
    (expect [new-file (returns (proxy-file 0))]
            (is (in-time? (CachedSource. 0
                                         ""
                                         "~/"
                                         {})))))
  (testing "false when last modified is greater than original time"
    (expect [new-file (returns (proxy-file 1))]
            (is (not (in-time? (CachedSource. 0
                                              ""
                                              "~/"
                                              {})))))))

(deftest absolute_file_from_var
  (expect [file-from-var (times 1 (returns "filename"))
           new-file (times 1 (returns (proxy-file 0)))
           slime-find-file (times 1(returns ""))]
          (is (absolute-file-from-var #'a))))

(deftest new_cached_source
  (binding [source-cache (atom {})]
    (expect [get-source-from-var (returns "(+ 1 2)")
             absolute-file-from-var (returns (proxy-file 0))]
            (is (= (:source (new-cached-source 'a))
                   "(+ 1 2)")))))

(deftest get_source_from_cache
  (testing "if the cache is invalid, reload the source"
    (binding [source-cache (atom {'a (CachedSource. 0 "(+ a b)" "~/" {})})]
      (expect [get-source-from-var (returns "(+ 1 2)")
               new-file (returns (proxy-file 1))
               absolute-file-from-var (returns (proxy-file 1))]
              (is (= (get-source-from-cache 'a) "(+ 1 2)")))))

  (testing "if the cache is valid, return the cached value"
    (binding [source-cache (atom {'a (CachedSource. 0 "(+ a b)" "~/" {})})]
      (expect [get-source-from-var (returns "(+ 1 2)")
               file-from-var (returns "filename")
               new-file (returns (proxy-file 0))]
              (is (= (get-source-from-cache 'a) "(+ a b)"))))))

(deftest all_ns_that_refer_to
  (testing "it requires all of them"
    (expect [find-ns-in-user-dir (returns '[a])
             require-and-return (times 1 (returns 'a))
             does-ns-refer-to-var? (returns true)
             slime-find-file (returns "")]
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
