(ns clojure-refactoring.replace-test
  (:use clojure-refactoring.replace :reload)
  (:use clojure-refactoring.source)
  (:use clojure.test)
  (:use clojure.contrib.mock))

(defn replace-test-fn [top-level-node]
  'a)

(def a nil) ;; needed so we can use the var below

(deftest renaming_fn
  (testing "it replaces occurences of the var name"
    (is (= ((renaming-fn #'a 'z) '(defn b [c] (a 1 2)))
           '(defn b [c] (z 1 2)))))) ;;eventually, this should only
;;replace things that resolve to vars in that namespace

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

(deftest replace_all_who_call ;;TODO: better tests
  (expect [vars-who-call (returns [#'a])
           map-to-alist (times 1 (returns :replacement-alist))
           build-replacement-map (times 1 (returns :replacement-map))]
   (is (=
        (replace-all-who-call #'a replace-test-fn)
        [:replacement-alist]))))
