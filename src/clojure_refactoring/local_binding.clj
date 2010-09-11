(ns clojure-refactoring.local-binding
  (:use clojure.walk
        [clojure-refactoring.support core formatter]
        [clojure.contrib.seq-utils :only (find-first)]
        [clojure-refactoring.support.parsley :only [defparsed-fn]])
  (:require [clojure-refactoring.support.parsley :as ast]))

(defn- get-function-definition [defn-ast]
  (find-first (ast/tag= :list) (:content defn-ast)))

(defn- add-to-binding [{content :content :as binding-node}
                       value var-name]
  (ast/replace-content binding-node
    `(~(first content)
      ~@(butlast (drop 1 content))
      ~ast/parsley-whitespace
      ~var-name
      ~ast/parsley-whitespace
      ~value
      ~(last content))))

(defn is-node-the-binding-form [top-level ast]
  (= ast (ast/parsley-fn-args top-level)))

(defn- modify-existing-let-block [form value var-name]
  (ast/parsley-walk
   (fn [ast]
     (if (is-node-the-binding-form form ast)
       (add-to-binding ast value var-name)
       ast))
   form))

(defn let-wrap [form value var-name]
  (if (ast/parsley-binding-node? form)
    (modify-existing-let-block form value var-name)
    (ast/list-without-whitespace
     (ast/symbol 'let)
      ast/parsley-whitespace
      (ast/parsley-vector
       [var-name value])
      ast/parsley-whitespace
      form)))

(defn- wrap-function-body-with-let [defn-form value var-name]
  (let [fn-def (get-function-definition defn-form)]
    (ast/parsley-tree-replace
     fn-def (let-wrap fn-def value var-name)
     defn-form)))

(defparsed-fn local-wrap [top value var-name]
  "Extracts a value as a local variable inside top"
  (-> (wrap-function-body-with-let
        (ast/parsley-tree-replace
         value var-name top)
        value var-name)
      ast/parsley-to-string))
