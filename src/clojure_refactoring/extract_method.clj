(ns clojure-refactoring.extract-method
  (:use [clojure-refactoring.support core
         find-bindings-above-node parsley]
        [clojure.contrib str-utils]))

(defn find-occurences [args node]
  "Looks for any occurence of each element of args in the node
TODO: doesn't handle destructuring properly"
  (flatten (loop [arg-set (set args) node node]
             (for [sub-node node]
               (if (seq? sub-node)
                 (find-occurences arg-set sub-node)
                 (arg-set sub-node))))))

(defn fn-name [fn-node] (nth fn-node 1))

(defn fn-call [fn-node]
  "Returns a function call to a function
using the names in the function's arguments.
TODO: Won't work for multiple arity functions"
  (conj (for [arg (fn-args fn-node)] arg) (fn-name fn-node)))

(defn arg-occurences [f-node extracted-node]
  "Finds the bindings from f-node that are also in the extracted node.
Works for all binding forms in core/binding-forms"
  (unique-vec
   (filter
    #(not= % nil)
    (find-occurences
     (find-bindings-above-node f-node extracted-node)
     extracted-node))))

(defn make-fn-node [name args body]
  `(~'defn ~(symbol name) ~args ~body))

(defn remove-extracted-function [extract-string fn-string new-fun]
  (parsley-node-to-string
   (replace-sexp-in-ast-node (read-string extract-string)
                        (fn-call new-fun)
                        (parse fn-string))))

(defn format-output [extract-string fn-string new-fun]
  "Formats the output for extract-method to print"
  (str
   (format-code new-fun)
   "\n"
   (remove-extracted-function extract-string fn-string new-fun)))

(defn extract-method [fn-string extract-string new-name]
  "Extracts extract-string out of fn-string and replaces it with a
function call to the extracted method. Only works on single arity root functions"
  (let [function-node (read-string fn-string)
        extract-node (read-string extract-string)
        args (arg-occurences function-node
                             extract-node)
        new-fun (make-fn-node new-name
                              args
                              extract-node)]
    (format-output extract-string fn-string new-fun)))
