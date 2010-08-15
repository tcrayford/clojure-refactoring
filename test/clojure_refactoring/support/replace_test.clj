(ns clojure-refactoring.support.replace-test
  (:use clojure-refactoring.support.replace :reload)
  (:use [clojure-refactoring.support source parsley])
  (:use clojure.test)
  (:use clojure.contrib.mock))

(defn replace-test-fn [top-level-node]
  (second (first (sexp "a"))))

(def a nil) ;; needed so we can use the var below

(deftest map_to_alist
  (testing "empty map gives an empty alist"
    (is (= (map-to-alist {}) '())))
  (testing "pairs"
    (is (= (map-to-alist {:a 1}) '((:a 1))))))

(defn replacement-map-for-tests []
  (build-replacement-map #'a replace-test-fn))

(deftest build_replacement_map
  (testing "it has the right attributes"
    (expect [get-source-from-cache (times 1 (returns "(+ a 1)"))]
            (let [m (replacement-map-for-tests)]
              (is (:file m))
              (is (:var-name m))
              (is (:line m))
              (is (:new-source m)))))
  (testing "populates the attributes correctly"
    (expect [get-source-from-cache (times 1 (returns "(+ a 1)"))]
            (is (= (:new-source (replacement-map-for-tests)) "a")))
    (expect [get-source-from-cache (returns "(+ a 1)")
             slime-file-from-var (times 1 (returns "foo"))]
            (is (= (:file (replacement-map-for-tests)) "foo")))
    (expect [get-source-from-cache
             (returns "(+ a 1)")
             line-from-var
             (returns 1)]
            (is (= (:line (replacement-map-for-tests)) 1)))))

(deftest replace_callers
  (expect
   [vars-who-call (returns [#'a])
    map-to-alist (times 1 (returns :replacement-alist))
    build-replacement-map (times 1 (returns :replacement-map))]
   (is (= (replace-callers #'a replace-test-fn) [:replacement-alist]))))
