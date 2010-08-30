(ns clojure-refactoring.support.find-bindings-above-node
  (:use [clojure-refactoring.support core parsley]))

(defn tag= [x ast]
  (= (:tag ast) x))

(defn parsley-binding-node? [ast]
  (and (map? ast)
       (tag= :list ast)
       (binding-forms
        (symbol (apply str
                       (:content (second (:content ast))))))))

(def parse1 (comp first parse))

(def sexp->parsley (comp parse1 format-code))

(defn first-vector [ast]
  (first (filter #(tag= :vector %) (:content ast))))

(defn extract-binding-syms [ast]
  (if (#{"defmacro" "fn" "defn"}
       (first (:content (second (:content ast)))))
    (relevant-content (first-vector ast))
    (evens (relevant-content (first-vector ast)))))

(defn extract-symbols-from-binding-node [ast]
  (->> (extract-binding-syms ast)
       parsley-sub-nodes
       (filter parsley-symbol?)))

(defn binding-node-that-contains? [node expr]
     (and (parsley-binding-node? node)
          (parsley-tree-contains node expr)))

(defn find-bindings-above-node [node expr]
  (->> (parsley-sub-nodes node)
       (filter #(binding-node-that-contains? % expr))
       (mapcat extract-symbols-from-binding-node)
       distinct))
