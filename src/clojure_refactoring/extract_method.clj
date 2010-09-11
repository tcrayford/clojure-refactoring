(ns clojure-refactoring.extract-method
  (:use [clojure-refactoring.support core formatter
         find-bindings-above-node]
        [clojure-refactoring.ast :only [defparsed-fn]]
        clojure.set
        [clojure.contrib.seq-utils :only [find-first]])
  (:require [clojure-refactoring.ast :as ast]))

(defn- find-occurences [args node]
  "Looks for any occurence of each element of args in the node"
  (seq (intersection (set args) (set (ast/sub-nodes node)))))

(defn fn-name [fn-node]
  (second (ast/relevant-content fn-node)))

(defn fn-call [fn-node]
  "Uses the arguments of a function node to return a call to it."
  (ast/list `(~(fn-name fn-node) ~@(ast/parsley-bindings fn-node))))

(defn- arg-occurences [f-node extracted-node]
  "Finds the bindings from f-node that are also in the extracted node."
  (-> (find-bindings-above-node f-node extracted-node)
      (find-occurences extracted-node)))

(defn- make-fn-node [name args body]
  "Creates an ast representing the new function"
  (format-ast
   (ast/list-without-whitespace
    (ast/symbol 'defn)
    name
    (ast/vector args)
    (ast/strip-whitespace body))))

(defn call-extracted [body toplevel extracted]
  (ast/tree-replace
   body
   (fn-call extracted)
   toplevel))

(defn- nodes-to-string [extract-node fn-node new-fun]
  "Formats the output for extract-method to print"
  (str (ast/ast->string new-fun)
       "\n\n"
       (ast/ast->string
        (call-extracted extract-node fn-node new-fun))))

(defparsed-fn extract-method [function-node extract-node new-name]
  "Extracts extract-string out of fn-string and replaces it with a
function call to the extracted method. Only works on single arity root functions"
  (let [args (arg-occurences function-node
                             extract-node)
        new-fun (make-fn-node new-name
                              args
                              extract-node)]
    (nodes-to-string extract-node function-node new-fun)))
