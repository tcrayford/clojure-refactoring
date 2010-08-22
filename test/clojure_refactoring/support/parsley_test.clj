(ns clojure-refactoring.support.parsley-test
  (:use clojure-refactoring.support.parsley :reload)
  (:use clojure-refactoring.test-helpers)
  (:use clojure.test)
  (:use clojure-refactoring.support.core)
  (:require [clojurecheck.core :as cc]))

(use-fixtures :once #(time (%)))


(deftest parsley_to_string
  (cc/property "parsley to string is an inverse of parsing"
               [s random-sexp-from-core]
               (is (= (parsley-node-to-string (parse s))
                      s))))

(defn parsley-rec-contains [obj ast]
  "Works out if a parsley-ast contains a sexp object"
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
                      (->> (parse s)
                           (replace-sexp-in-ast-node a b)
                           (parsley-rec-contains a))))))
  (cc/property "replacing lists"
               [a random-sexp
                b random-sexp
                c random-sexp]
               (let [s (pr-str `(~@a ~@c))]
                 (is (not
                      (->> (parse s)
                           (replace-sexp-in-ast-node a b)
                           (parsley-rec-contains a))))))

  (cc/property "replacing more lists"
               [old random-sexp
                new random-sexp]
               (let [s (pr-str `(defn a [b] ~old))]
                 (is (not
                      (->> (parse s)
                           (replace-sexp-in-ast-node old new)
                           (parsley-rec-contains old))))))

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

  (is (= (parsley-node-to-string
          (replace-symbol-in-ast-node 'a 'z
                                      (parse "(defn b [c] (a 1 2))")))
         "(defn b [c] (z 1 2))")))

(deftest match_parsley
  (cc/property "parsley matches sexp on the read string"
               [s random-sexp-from-core]
               (is (match-parsley (read-string s) (parse s)))))

(deftest parsley_walk
  (cc/property "parsley-walk with identity returns the same ast it was passe"
               [s random-sexp-from-core]
               (let [parsed (parse s)]
                 (is (= (parsley-walk identity parsed)
                        parsed)))))
