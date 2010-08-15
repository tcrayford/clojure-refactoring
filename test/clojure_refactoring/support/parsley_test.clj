(ns clojure-refactoring.support.parsley-test
  (:use clojure-refactoring.support.parsley :reload)
  (:use clojure.test)
  (:use [clojure-refactoring.support.core :only [rec-contains?]])
  (:require [clojurecheck.core :as cc]))

(use-fixtures :once #(time (%)))

(def symbol-chars (vec "abcdefghijklmnopqrstuvwxyz"))

(defn random-symbol-char [& args]
  (rand-nth symbol-chars))

(defn random-symbol [& args]
  (symbol (reduce str (take (inc (rand 10))
                            (repeatedly random-symbol-char)))))

(defn random-sexp [& args]
  (into () (take (inc (rand 10))
                 (repeatedly random-symbol))))

(defn random-sexp-with-comments [& args]
  (str (pr-str (random-sexp)) ";;" (random-symbol) ))

(deftest parsley_to_string
  (cc/property "parsley to string is an inverse of sexp"
               [s random-sexp-with-comments]
               (is (= (parsley-to-string (sexp s))
                      s))))

(defn parsley-rec-contains [obj ast]
  (-> (parsley-to-string ast)
      read-string
      (rec-contains? obj)))

(deftest replace_sexp_in_ast
  (cc/property "replace-sexp-in-ast where
         both new and old sexps are the same returns the ast"
               [s random-sexp-with-comments]
               (is (= (second (first (sexp s)))
                      (replace-sexp-in-ast '(a) '(a) (sexp s)))))
  (cc/property "after replacing, the old node is no longer present"
               [a random-symbol
                b random-symbol]
               (let [s (pr-str
                        `(~a ~@(random-sexp)))]
                 (is (not
                      (parsley-rec-contains 'a
                                            (replace-sexp-in-ast a b (sexp s)))))))
  (cc/property "replacing lists"
               [a random-sexp
                b random-sexp
                c random-sexp]
               (let [s (pr-str `(~@a ~@c))]
                 (is (not
                      (parsley-rec-contains
                       a
                       (replace-sexp-in-ast a b (sexp s)))))))
  (cc/property "replacing more lists"
               [old random-sexp
                new random-sexp]
               (let [s (pr-str `(defn a [b] ~old))]
                 (is (not
                      (parsley-rec-contains
                       old
                       (replace-sexp-in-ast old new (sexp s)))))))
  (is (= (parsley-node-to-string
          (replace-sexp-in-ast
           '(inc b)
           '(arr b)
           (sexp "(defn a [b] (inc b))")))
         "(defn a [b] (arr b))"))
  (is (= (parsley-node-to-string
          (replace-sexp-in-ast
           '(re-split #"," s)
           '(string-split s)
           (sexp "(re-split #\",\" s)")))
         "(string-split s)")))

(deftest replace_symbol_in_ast_node
  (cc/property "replaces the symbol"
               [old random-symbol
                new random-symbol]
               (let [ast (sexp (pr-str `(defn a [b] (~old bar))))]
                 (is (not
                      (parsley-rec-contains
                       old
                       (replace-symbol-in-ast-node
                        old
                        new
                        ast)))))))

(deftest match_parsley
  (cc/property "parsley matches sexp on the read string"
               [s random-sexp-with-comments]
               (is (match-parsley (read-string s) (sexp s)))))
