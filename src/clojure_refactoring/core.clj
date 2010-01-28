(ns clojure_refactoring.core
  (:use [clojure.contrib str-utils duck-streams seq-utils pprint] clojure.walk)
  (:require [clojure.zip :as z] [clojure.contrib.zip-filter :as zf]))

(defn is-defn? [node]
  (= (first node) 'defn))

(defn format-code [node]
  (with-out-str (with-pprint-dispatch *code-dispatch*
                  (pprint node))))

(defn find-occurences [args node]
  "Looks for any occurence of each element of args in the node
TODO: doesn't handle destructuring"
  (flatten (loop [arg-set (set args) node node]
             (for [sub-node node]
               (if (seq? sub-node)
                 (or (find-occurences arg-set sub-node))
                 (arg-set sub-node))))))

(defmacro if-true [expr]
  "Always returns true or false, depending on the value of its body"
  `(if ~expr true false))

(def *binding-forms*
     #{'let 'fn 'binding 'for 'doseq 'dotimes 'defn 'loop})

(defn binding-node? [node]
  (if-true (*binding-forms* (first node))))

(defn evens [coll]
  "Returns the even items of a collection"
  (->> (indexed coll)
       (filter #(even? (first %)))
       (map #(last %))
       (vec)))

(defn fn-args [node]
  (find-first #(vector? %) node))

(defn binding-form [node]
  "Returns a vector of bindings iff the node is a binding node. Won't work with multiple arity defns"
  (if (binding-node? node)
    (fn-args node)))

(defn unique-vec [v]
  (vec (set v)))

(defn bound-symbols [node]
  (unique-vec
   (if (is-defn? node)
     (binding-form node)
     (evens (binding-form node)))))

(defn rec-contains? [coll obj]
  "True if coll contains obj at some level of nesting"
  (some #(= % true)
        (flatten (for [sub-node coll]
                   (if (= sub-node obj)
                     true
                     (if (seq? sub-node)
                       (rec-find sub-node obj)
                       false))))))

(defn last-binding-form? [node]
  "Returns true if there are no binding nodes inside node"
  (and (binding-node? node)
       (->>
        sym
        (rec-contains? (rest node))
        (for [sym *binding-forms*])
        (some #(= % true))
        (not))))

(defn add-binding-form [node bnd-syms]
  (into bnd-syms (bound-symbols node)))

(defn find-bindings
  "Returns any let bindings above expr in node"
  ([node expr] (find-bindings node expr []))
  ([node expr bnd-syms]
     (unique-vec
      (if (last-binding-form? node)
        (add-binding-form node bnd-syms)
        (if (binding-node? node)
          (find-bindings (rest node)
                         expr
                         (add-binding-form node bnd-syms))
          (if (seq? (first node))
            (find-bindings (first node) expr bnd-syms)
            (find-bindings (rest node) expr bnd-syms)))))))


