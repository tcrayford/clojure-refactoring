(ns clojure-refactoring.extract-method
  (:use [clojure-refactoring.support core
         find-bindings-above-node parsley]
        clojure.set
        [clojure.contrib.seq-utils :only [find-first]]))

(defn find-occurences [args node]
  "Looks for any occurence of each element of args in the node
TODO: doesn't handle destructuring properly"
  (intersection (set args) (set (parsley-sub-nodes node))))

(defn fn-name [fn-node]
  (second (relevant-content fn-node)))

(defn parsley-bindings [ast]
  (relevant-content (find-first #(tag= :vector %) (:content ast))))

(defn fn-call [fn-node]
  "Returns a function call to a function
using the names in the function's arguments.
TODO: Won't work for multiple arity functions"
  (parsley-list `(~(fn-name fn-node) ~@(parsley-bindings fn-node))))

(defn arg-occurences [f-node extracted-node]
  "Finds the bindings from f-node that are also in the extracted node.
Works for all binding forms in core/binding-forms"
  (unique-vec
   (filter
    #(not= % nil)
    (find-occurences
     (find-bindings-above-node
      f-node
      extracted-node)
     extracted-node))))

(defn make-fn-node [name args body]
  {:tag :list :content
   (list
    "(" (ast-symbol 'defn) (parse1 " ") (ast-symbol (symbol name))
    (parse1 " ") (parsley-vector args) (parse "\n  ")
    body ")")})

(defn remove-extracted-function [extract-string fn-string new-fun]
  (->> (parsley-tree-replace
        (first (parse extract-string))
        (fn-call new-fun)
        (parse fn-string))
       parsley-to-string))

(defn format-output [extract-string fn-string new-fun]
  "Formats the output for extract-method to print"
  (str
   (parsley-to-string new-fun)
   "\n"
   "\n"
   (remove-extracted-function extract-string fn-string new-fun)))

(defn extract-method [fn-string extract-string new-name]
  "Extracts extract-string out of fn-string and replaces it with a
function call to the extracted method. Only works on single arity root functions"
  (let [function-node (parse1 fn-string)
        extract-node (parse1 extract-string)
        args (arg-occurences function-node
                             extract-node)
        new-fun (make-fn-node new-name
                              args
                              extract-node)]
    (format-output extract-string fn-string new-fun)))
