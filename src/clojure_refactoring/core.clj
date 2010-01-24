(ns clojure_refactoring.core
  (:use [clojure.contrib str-utils duck-streams seq-utils pprint] clojure.walk)
  (:require [clojure.zip :as z] [clojure.contrib.zip-filter :as zf]))



(defn fn-args [node]
  "Returns (hopefully) the arglist for a function without variable arity
TODO: make this work with variable arity"
  (find-first #(vector? %) node))

(defn pr-code [node]
  (with-pprint-dispatch *code-dispatch* (pprint node)))

(defn find-occurences [args node]
  "Looks for any occurence of each element of args in the node
TODO: doesn't handle destructuring"
  (flatten (loop [arg-set (set args) node node]
             (for [sub-node node]
               (if (seq? sub-node)
                 (or (find-occurences arg-set sub-node))
                 (arg-set sub-node))))))

(def *binding-forms* #{'let 'fn 'binding})

(defn binding-node? [node]
  (if (*binding-forms* (first node)) true false))

(defn rec-occurrences [coll obj]
  (flatten (for [sub-node coll]
                   (if (= sub-node obj)
                     true
                     (if (seq? sub-node)
                       (rec-occurrences sub-node obj)
                       false)))))

(defn rec-contains? [coll obj]
  "True if coll contains obj at some level of nesting"
  (some #(= % true)
        (rec-occurrences coll obj)))

(defn last-binding-form? [node]
  "Returns true if there are no binding nodes inside node"
  (if (and (binding-node? node)
           (= (count
               (filter #(not= % false) (rec-occurrences node 'let))) 1))
    true
    (rec-contains? node 'let)))



(defn find-bindings [node expr]
  "Returns any let bindings above expr in node
For nested bindings, only contains the top value"
  (vec
   (filter #(if (not= % nil) %)
           (flatten (for [sub-node node]
                      (if (seq? sub-node)
                        (if (and (binding-node? node)
                                 (rec-contains? node expr)
                                 (last-binding-form? node))
                          (nth node 1)
                          (find-bindings sub-node expr))))))))