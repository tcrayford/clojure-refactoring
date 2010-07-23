(ns clojure-refactoring.find-bindings-above-node
  (:use clojure-refactoring.core))

(declare process-multiple-nodes
         process-remaining-bindings
         process-nested-bindings)

(defn find-bindings-above-node
  "Returns all binding forms above expr in node."
  ([node expr] (find-bindings-above-node node expr []))
  ([node expr bnd-syms]
     (unique-vec
      (flatten
       (cond (more-than-one seq? node)
             (process-multiple-nodes node expr bnd-syms)

             (last-binding-form? node)
             (add-binding-form node bnd-syms)

             (binding-node? node)
             (process-remaining-bindings node expr bnd-syms)

             (seq? (first node))
             (process-nested-bindings node expr bnd-syms)

             :else
             (find-bindings-above-node (rest node) expr bnd-syms))))))

(defn process-multiple-nodes [node expr bnd-syms]
  (->>
   (filter #(rec-contains? % expr) node)
   (map #(find-bindings-above-node % expr bnd-syms))))

(defn process-remaining-bindings [node expr bnd-syms]
  (find-bindings-above-node
   (rest node)
   expr
   (add-binding-form node bnd-syms)))

(defn process-nested-bindings [node expr bnd-syms]
 (find-bindings-above-node (first node) expr bnd-syms))
