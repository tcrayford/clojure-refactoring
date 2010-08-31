(ns clojure-refactoring.extract-method
  (:use [clojure-refactoring.support core
         find-bindings-above-node parsley]
        clojure.set
        [clojure.contrib.seq-utils :only [find-first]]))

(defn- find-occurences [args node]
  "Looks for any occurence of each element of args in the node"
  (seq (intersection (set args) (set (parsley-sub-nodes node)))))

(defn fn-name [fn-node]
  (second (relevant-content fn-node)))

(defn fn-call [fn-node]
  "Uses the arguments of a function node to return a call to it."
  (parsley-list `(~(fn-name fn-node) ~@(parsley-bindings fn-node))))

(defn- arg-occurences [f-node extracted-node]
  "Finds the bindings from f-node that are also in the extracted node."
  (-> (find-bindings-above-node f-node extracted-node)
      (find-occurences extracted-node)))

(defn- make-fn-node [name args body]
  "Creates an ast representing the new function"
  {:tag :list :content
   (list
    "(" (ast-symbol 'defn) (parse1 " ") name
    (parse1 " ") (parsley-vector args) (parse "\n  ")
    body ")")})

(defn call-extracted [body toplevel extracted]
  (parsley-tree-replace
   body
   (fn-call extracted)
   toplevel))

(defn- nodes-to-string [extract-node fn-node new-fun]
  "Formats the output for extract-method to print"
  (str (parsley-to-string new-fun)
       "\n\n"
       (parsley-to-string
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
