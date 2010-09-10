(ns clojure-refactoring.destructuring-test
  (:use clojure-refactoring.destructuring :reload)
  (:use clojure-refactoring.support.parsley
        clojure.test)
  (:require [clojure-refactoring.support.parser :as parser]))

(use-fixtures :once #(time (%)))

(deftest removing-whitespace
  (is (empty? (remove ignored-node?
                      (parser/parse " ")))))

(deftest parsley_map_lookup
  (testing "map lookups"
    (are [s] (parsley-map-lookup? (first (parser/parse s)))
         "(:a a)"
         "(b :foo)"))

  (are [s] (not (parsley-map-lookup? (first (parser/parse s))))
       "(a (:a a))"
       "(:a a a)"
       "(:foo :bar)"
       "(:foo a :bar b)"))

(deftest parsley_key_to_sym
  (is (= (parsley-key->sym '{:tag :atom :content (":a")})
         '{:tag :atom :content ("a")})))

(deftest parsley_find_map_lookups
  (is (= (map parsley-to-string
              (parsley-find-lookups (parser/parse "(defn a [b] (:a b))")))
         '("(:a b)"))))

(deftest parsley_lookup_to_proper_form
  (is (= (parsley-to-string (parsley-lookup-to-canoninical-form
                             (first (parser/parse "(a :a)"))))
         "(:a a)")))

(deftest add_to_parsley_map
  (is (= (parsley-to-string
          (add-to-parsley-map '{:tag :map :content ("{" "}")}
                              '{:tag :atom :content ("a")}
                              '{:tag :atom :content ("b")})))))

(deftest parsley_lookups_to_binding_map
  (is ((lookups-to-binding-map (parsley-find-lookups (parser/parse "(defn a [b] (:a b))")))
           '{:tag :atom :content ("b")})))

;;Integration level tests below here.
(deftest destructure_map_test
  (is (= (destructure-map "(defn a [b] (:foo b))")
         "(defn a [{foo :foo }] foo)"))
  (is (= (destructure-map "(defn a [b] (+ (:foo b) (:bar b)))")
         "(defn a [{bar :bar foo :foo }] (+ foo bar))"))
  (is (= (destructure-map "(defn a [b] (+ (b :foo) (b :bar)))")
         "(defn a [{bar :bar foo :foo }] (+ foo bar))")))
