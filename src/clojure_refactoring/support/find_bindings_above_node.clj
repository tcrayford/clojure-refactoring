(ns clojure-refactoring.support.find-bindings-above-node
  (:use [clojure-refactoring.support core])
  (:require [clojure-refactoring.support.parsley :as ast]))

(defn extract-binding-syms [ast]
  (if (#{"defmacro" "fn" "defn"}
       (first (:content (second (:content ast)))))
    (ast/relevant-content (ast/first-vector ast))
    (evens (ast/relevant-content (ast/first-vector ast)))))

(defn extract-symbols-from-binding-node [ast]
  (->> (extract-binding-syms ast)
       ast/sub-nodes
       (filter ast/symbol?)))

(defn binding-node-that-contains? [node expr]
     (and (ast/binding-node? node)
          (ast/tree-contains node expr)))

(defn find-bindings-above-node [node expr]
  (->> (ast/sub-nodes node)
       (filter #(binding-node-that-contains? % expr))
       (mapcat extract-symbols-from-binding-node)
       distinct))
