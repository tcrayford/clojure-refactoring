(ns clojure_refactoring.core
  (:use [clojure.contrib str-utils duck-streams seq-utils pprint] clojure.walk)
  (:require [clojure.zip :as z] [clojure.contrib.zip-filter :as zf]))

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

(defn let-node? [node]
  (= 'let (first node)))

 (defn find-bindings [node expr]
   "Returns any let bindings above expr in node"
   (for [sub-node node]
     (if (seq? sub-node)
       (if (and (let-node? node) (rec-contains? node expr))
         (nth node 1)
         (find-bindings sub-node expr)))))

(defn rec-contains? [coll obj]
  "True if coll contains obj at some level of nesting"
  (some #(= % true)
        (flatten (for [sub-node coll]
                   (if (= sub-node obj)
                     true
                     (if (seq? sub-node)
                       (rec-contains? sub-node obj)
                       false))))))
