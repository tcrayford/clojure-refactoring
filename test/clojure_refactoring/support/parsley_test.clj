(ns clojure-refactoring.support.parsley-test
  (:use clojure-refactoring.support.parsley :reload)
  (:use clojure-refactoring.test-helpers)
  (:use clojure.test)
  (:use [clojure-refactoring.support core source])
  (:require [clojurecheck.core :as cc]))

(use-fixtures :once #(time (%)))

(deftest parsley_to_string
  (cc/property "parsley to string is an inverse of parsing"
               [s random-sexp-from-core]
               (is (= (parsley-to-string (parse s))
                      s)))
  (testing "parsley-to-string once called on a file gives
               the same result as reading that file"
    (for [namespace (find-ns-in-user-dir)]
      (let [slurped (slurp
                     (filename-from-ns namespace))]
        (is (= (parsley-to-string (parse slurped))
               slurped))))))

(defn parsley-rec-contains [obj ast]
  "Works out if a parsley-ast contains a sexp object"
  (-> (parsley-to-string ast)
       read-string
      (tree-contains? obj)))

(deftest replace_symbol_in_ast_node
  (cc/property "replaces the symbol"
               [s random-sexp-from-core
                new random-symbol]
               (let [parsed (parse s)
                     old (first (read-string s))]
                 (is (not
                      (->>
                       (replace-symbol-in-ast-node old new parsed)
                       (parsley-rec-contains old))))))

  (cc/property "replacing a symbol with itself produces the same tree"
               [s random-sexp-from-core]
               (let [parsed (parse s)
                     old (first (read-string s))]
                 (is (= (replace-symbol-in-ast-node old old parsed)
                        parsed))))

  (cc/property "replacing a symbol with another one, then replacing the new one with the original produces the same node"
               [s random-sexp-from-core
                new random-symbol]
               (let [parsed (parse s)
                     old (first (read-string s))]
                 (is (= (->>
                         (replace-symbol-in-ast-node old new parsed)
                         (replace-symbol-in-ast-node new old))
                        parsed))))

  (is (= (parsley-to-string
          (replace-symbol-in-ast-node 'a 'z
                                      (parse "(defn b [c] (a 1 2))")))
         "(defn b [c] (z 1 2))")))

(deftest parsley_walk
  (cc/property "parsley-walk with identity returns the same ast it was passe"
               [s random-sexp-from-core]
               (let [parsed (parse s)]
                 (is (= (parsley-walk identity parsed)
                        parsed)))))
