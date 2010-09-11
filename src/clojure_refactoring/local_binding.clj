(ns clojure-refactoring.local-binding
  (:use clojure.walk
        [clojure-refactoring.support core formatter]
        [clojure.contrib.seq-utils :only (find-first)]
        [clojure-refactoring.ast :only [defparsed-fn]])
  (:require [clojure-refactoring.ast :as ast]))

(defn- get-function-definition [defn-ast]
  (find-first (ast/tag= :list) (:content defn-ast)))

(defn- add-to-binding [{content :content :as binding-node}
                       value var-name]
  (ast/replace-content binding-node
    `(~(first content)
      ~@(butlast (drop 1 content))
      ~ast/whitespace
      ~var-name
      ~ast/whitespace
      ~value
      ~(last content))))

(defn is-node-the-binding-form [top-level ast]
  (= ast (ast/parsley-fn-args top-level)))

(defn- modify-existing-let-block [form value var-name]
  (ast/walk
   (fn [ast]
     (if (is-node-the-binding-form form ast)
       (add-to-binding ast value var-name)
       ast))
   form))

(defn let-wrap [form value var-name]
  (if (ast/binding-node? form)
    (modify-existing-let-block form value var-name)
    (ast/list-without-whitespace
     (ast/symbol 'let)
      ast/whitespace
      (ast/vector
       [var-name value])
      ast/whitespace
      form)))

(defn- wrap-function-body-with-let [defn-form value var-name]
  (let [fn-def (get-function-definition defn-form)]
    (ast/tree-replace
     fn-def (let-wrap fn-def value var-name)
     defn-form)))

(defparsed-fn local-wrap [top value var-name]
  "Extracts a value as a local variable inside top"
  (-> (wrap-function-body-with-let
        (ast/tree-replace
         value var-name top)
        value var-name)
      ast/ast->string))
