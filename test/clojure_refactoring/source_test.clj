(ns clojure-refactoring.source-test
  (:use clojure-refactoring.source :reload)
  (:import clojure-refactoring.source.CachedSource)
  (:use clojure-refactoring.core)
  (:use clojure.test clojure.contrib.mock))

(def a nil) ;; used to test does-ns-refer-to-var? below.

(defn proxy-file [time]
  (proxy [java.io.File] ["~/"] (lastModified [] time)))

(deftest in_time
  (testing "true when last modified is equal to time"
    (expect [new-file (returns (proxy [java.io.File] ["~/"]
                                 (lastModified [] 0)))
             slime-find-file (returns "")]
            (is (in-time? (CachedSource. 0
                                         ""
                                         "~/")))))
  (testing "false when last modified is greater than original time"
    (expect [new-file (returns (proxy-file 1))
             slime-find-file (returns "")]
            (is (not (in-time? (CachedSource. 0
                                              ""
                                              "~/")))))))

(deftest new_cached_source
  (binding [source-cache (atom {})]
    (expect [get-source-from-var (returns "(+ 1 2)")
             file-from-var (returns "filename")
             new-file (returns (proxy-file 0))
             slime-find-file (returns "")]
            (is (= (:source (new-cached-source 'a))
                   "(+ 1 2)")))))

(deftest get_source_from_cache
  (testing "if the cache is invalid, reload the source"
    (binding [source-cache (atom {'a (CachedSource. 0 "(+ a b)" "~/")})]
      (expect [get-source-from-var (returns "(+ 1 2)")
               file-from-var (returns "filename")
               new-file (returns (proxy-file 1))
               slime-find-file (returns "")]
              (is (= (get-source-from-cache 'a) "(+ 1 2)")))))

  (testing "if the cache is valid, return the cached value"
    (binding [source-cache (atom {'a (CachedSource. 0 "(+ a b)" "~/")})]
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
  (let [this-ns (find-ns 'clojure-refactoring.source-test)]
    (is (does-ns-refer-to-var? this-ns #'a))
    (testing "same named var in another ns"
      (is (not (does-ns-refer-to-var?
                this-ns
                (find-var 'clojure-refactoring.replace-test/a)))))
    (testing "var named something that doesn't exist in the current ns"
      (is (not (does-ns-refer-to-var?
                this-ns
                (find-var 'clojure-refactoring.replace/line-from-var)))))
    (testing "non existent var"
      (is (not (does-ns-refer-to-var?
                this-ns
                (find-var 'clojure-refactoring.source-test/boo)))))))



