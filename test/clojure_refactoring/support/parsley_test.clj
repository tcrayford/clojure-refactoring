(ns clojure-refactoring.support.parsley-test
  (:use clojure-refactoring.support.parsley :reload)
  (:use clojure-refactoring.test-helpers)
  (:use clojure.test)
  (:use clojure-refactoring.support.core)
  (:require [clojurecheck.core :as cc]))

(use-fixtures :once #(time (%)))


(deftest parsley_to_string
  (cc/property "parsley to string is an inverse of sexp"
               [s random-sexp-from-core]
               (is (= (parsley-node-to-string (parse s))
                      s))))

(defn parsley-rec-contains [obj ast]
  (-> (parsley-node-to-string ast)
      read-string
      (tree-contains? obj)))

(deftest replace_sexp_in_ast
  (cc/property "replace-sexp-in-ast where
         both new and old sexps are the same returns the ast"
               [s random-sexp-with-comments]
               (is (= (parse s)
                      (replace-sexp-in-ast-node '(a) '(a) (parse s)))))
  (cc/property "after replacing, the old node is no longer present"
               [a random-symbol
                b random-symbol]
               (let [s (pr-str
                        `(~a ~@(random-sexp)))]
                 (is (not
                      (parsley-rec-contains 'a
                                            (replace-sexp-in-ast-node a b (parse s)))))))
  (cc/property "replacing lists"
               [a random-sexp
                b random-sexp
                c random-sexp]
               (let [s (pr-str `(~@a ~@c))]
                 (is (not
                      (parsley-rec-contains
                       a
                       (replace-sexp-in-ast-node a b (parse s)))))))

  (cc/property "replacing more lists"
               [old random-sexp
                new random-sexp]
               (let [s (pr-str `(defn a [b] ~old))]
                 (is (not
                      (parsley-rec-contains
                       old
                       (replace-sexp-in-ast-node old new (parse s)))))))

  (is (= (parsley-node-to-string
          (replace-sexp-in-ast-node
           '(inc b)
           '(arr b)
           (parse "(defn a [b] (inc b))")))
         "(defn a [b] (arr b))"))

  (is (= (parsley-node-to-string
          (replace-sexp-in-ast-node
           '(re-split #"," s)
           '(string-split s)
           (parse "(re-split #\",\" s)")))
         "(string-split s)")))

(deftest replace_symbol_in_ast_node
  (cc/property "replaces the symbol"
               [s random-sexp-from-core
                new random-symbol]
               (let [parsed (parse s)
                     old (first (read-string s))]
                 (is (not
                      (->>
                       (replace-symbol-in-ast-node old new parsed)
                       (parsley-rec-contains old)))))))


(deftest match_parsley
  (cc/property "parsley matches sexp on the read string"
               [s random-sexp-from-core]
               (is (match-parsley (read-string s) (parse s)))))

(deftest parsley_walk
  (cc/property "parsley-walk with identity returns the same ast it was passe"
               [s random-sexp-with-comments]
               (let [parsed (parse s)]
                 (is (= (parsley-walk identity parsed)
                        parsed)))))
