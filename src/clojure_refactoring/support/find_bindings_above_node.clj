(ns clojure-refactoring.support.find-bindings-above-node
  (:use [clojure-refactoring.support core]
        clojure-refactoring.ast.zip)
  (:require [clojure.zip :as zip])
  (:require [clojure-refactoring.ast :as ast]))

(defn extract-binding-syms [ast]
  (if (#{"defmacro" "fn" "defn"}
       (first (:content (second (:content ast)))))
    (ast/relevant-content (ast/first-vector ast))
    (evens (ast/relevant-content (ast/first-vector ast)))))

(defn extract-symbols-from-binding-node [ast]
  (->> (extract-binding-syms ast)
       ast/sub-nodes
       (filter ast/symbol?)))

(defn bindings-above [loc]
  (->> (zip/path loc)
       (filter ast/binding-node?)
       (mapcat extract-symbols-from-binding-node)
       set))

(defn find-bindings-above-node [node expr]
  (->> (find-node (ast-zip node) expr)
       bindings-above))
