(ns clojure_refactoring.core
  (use [clojure.contrib str-utils duck-streams seq-utils pprint]))

(defn fn-args [node]
  "Returns (hopefully) the arglist for a function without variable arity
TODO: make this work with variable arity"
  (find-first #(vector? %) node))

(defn pr-code [node]
  (with-pprint-dispatch *code-dispatch* (pprint node)))

(defn node-readlines [node]
  (re-split #"\n" (with-out-str (pr-code node))))

(defn find-occurences [args node]
  "Looks for any occurence of each element of args in the node
TODO: doesn't handle destructuring"
  (flatten (loop [arg-set (set args) node node]
             (for [sub-node node]
               (if (seq? sub-node)
                 (or (find-occurences arg-set sub-node))
                 (arg-set sub-node))))))






