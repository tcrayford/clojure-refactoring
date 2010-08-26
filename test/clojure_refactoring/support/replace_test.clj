(ns clojure-refactoring.support.replace-test
  (:use clojure-refactoring.support.replace :reload)
  (:use [clojure-refactoring.support source parsley
         paths])
  (:use clojure.test)
  (:use clojure.contrib.mock))

(use-fixtures :once #(time (%)))

(defn replace-test-fn [top-level-node]
  (parse "a"))

(deftest map_to_alist
  (testing "empty map gives an empty alist"
    (is (= (map-to-alist {}) '())))
  (testing "pairs"
    (is (= (map-to-alist {:a 1}) '((:a 1))))))

(defn replacement-map-for-tests []
  (build-replacement-map 'a replace-test-fn))

(deftest build_replacement_map
  (testing "it has the right attributes"
    (expect [parsley-from-cache (returns (parse "(+ a 1)"))
             filename-from-ns  (returns "")]
            (let [m (replacement-map-for-tests)]
              (is (:file m))
              (is (:new-source m)))))
  (testing "populates the attributes correctly"
      (expect [parsley-from-cache (times 1 (returns (parse "(+ a 1)")))
               filename-from-ns (returns "")]
              (is (= (:new-source (replacement-map-for-tests)) "a")))
      (expect [parsley-from-cache (returns (parse "(+ a 1)"))
               filename-from-ns (returns "foo")]
              (is (= (:file (replacement-map-for-tests)) "foo")))))

(deftest replace_callers
  (expect
   [namespaces-who-refer-to (returns [a])
    map-to-alist (times 1 (returns :replacement-alist))
    build-replacement-map (times 1 (returns :replacement-map))]
   (is (= (replace-callers a replace-test-fn) [:replacement-alist]))))
