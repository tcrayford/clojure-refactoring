(ns clojure-refactoring.support.core
  (:use [clojure.contrib str-utils pprint])
  (:use [clojure.contrib.seq-utils :only (find-first)])
  (:use clojure.walk)
  (:import clojure.lang.Named))

(defn format-code [node]
  "Outputs code roughly how a human would format it."
  (with-out-str
    (with-pprint-dispatch *code-dispatch*
      (pprint node))))

(defn contains-sub-nodes? [tree]
  (or (sequential? tree)
      (map? tree)
      (set? tree)))

(defn expand-sub-nodes [tree]
  (if (map? tree)
    (interleave (keys tree) (vals tree))
    (seq tree)))

(defn sub-nodes [tree]
  (tree-seq contains-sub-nodes?
            expand-sub-nodes tree))

(defn count= [seq n]
  "Checks if the count of seq is equal to n"
  (= (count seq) n))

(def binding-forms
     #{'let 'fn 'binding 'for 'doseq 'dotimes 'defn 'loop 'defmacro
       'if-let 'when-let 'defn-})

(defn binding-node? [[node-type]]
  "Checks if a node is a binding node"
  (binding-forms node-type))

(defn evens [coll]
  "Returns every other item of coll"
  (take-nth 2 coll))

(defn bindings [node]
  "Returns the function arguments from a top-level defn node"
  (find-first vector? node))

(defn unique-vec [coll]
  "Strips all duplicates from coll and forces it into a vector"
  (vec (distinct coll)))

(defn tree-contains? [coll obj]
  "True if coll contains obj at some level of nesting"
  (some #{obj}
        (sub-nodes coll)))

(defn call-when [pred f obj]
  (if (pred obj)
    (f obj)
    obj))

(defn replace-when [pred f coll]
  "Replaces each element of coll if pred returns true on it."
  (map #(call-when pred f %) coll))

(defn replace-in-sexp [old new sexp]
  "Walks over sexp, replacing each element from old with its corresponding element in new."
  (postwalk-replace
   (zipmap old new)
   sexp))
