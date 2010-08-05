(ns clojure-refactoring.source-test
  (:use clojure-refactoring.source :reload)
  (:import clojure-refactoring.source.CachedSource)
  (:use clojure-refactoring.core)
  (:use clojure.test clojure.contrib.mock))

(defn proxy-file [time]
  (proxy [java.io.File] ["~/"] (lastModified [] time)))

(deftest in_time
  (testing "true when last modified is equal to time"
    (expect [new-file (returns (proxy [java.io.File] ["~/"]
                                 (lastModified [] 0)))]
            (is (in-time? (CachedSource. 0
                                         ""
                                         "~/")))))
  (testing "false when last modified is greater than original time"
    (expect [new-file (returns (proxy-file 1))]
            (is (not (in-time? (CachedSource. 0
                                              ""
                                              "~/")))))))

(deftest new_cached_source
  (binding [source-cache (atom {})]
    (expect [get-source-from-var (returns "(+ 1 2)")
             file-from-var (returns "filename")
             new-file (returns (proxy-file 0))]
            (is (= (:source (new-cached-source 'a))
                   "(+ 1 2)")))))

(deftest get_source_from_cache
  (testing "if the cache is invalid, reload the source"
    (binding [source-cache (atom {'a (CachedSource. 0 "(+ a b)" "~/")})]
      (expect [get-source-from-var (returns "(+ 1 2)")
               file-from-var (returns "filename")
               new-file (returns (proxy-file 1))]
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
             does-ns-refer-to-var? (returns true)]
            (doall (all-ns-that-refer-to 'b))))
  (testing "it is empty when there are no namespaces that resolve the var"
    (expect [find-ns-in-user-dir (returns '[a])
             require-and-return (returns 'a)
             does-ns-refer-to-var? (returns false)]
            (is (empty? (all-ns-that-refer-to 'a))))))
