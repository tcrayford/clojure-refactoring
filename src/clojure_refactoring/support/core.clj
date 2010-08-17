(ns clojure-refactoring.support.core
  (:use [clojure.contrib str-utils pprint])
  (:use [clojure.contrib.seq-utils :only (find-first)])
  (:use clojure.walk)
  (:import clojure.lang.Named))

(defn defn? [[sym]]
  "Returns true if the current node is a function definition"
  (= sym 'defn))

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

(def binding-forms
     #{'let 'fn 'binding 'for 'doseq 'dotimes 'defn 'loop 'defmacro})

(defn binding-node? [[node-type]]
  "Checks if a node is a binding node"
  (binding-forms node-type))

(defn evens [coll]
  "Returns every other item of coll"
  (take-nth 2 coll))

(defn fn-args [node]
  "Returns the function arguments from a top-level defn node"
  (find-first vector? node))

(defn extract-binding-form [node]
  "Returns a vector of bindings iff the node is a binding node. Won't work with multiple arity defns"
  (fn-args node))

(defn unique-vec [coll]
  "Strips all duplicates from coll and forces it into a vector"
  (vec (distinct coll)))

(defn bound-symbols [node]
  "Returns a vector of the bound symbols inside node"
  (if (defn? node)
    (extract-binding-form node)
    (evens (extract-binding-form node))))

(defn some-true? [coll]
  "Returns true if anything in the collection is true"
  (some #{true} coll))

(defn rec-matches? [f coll]
  "True if the result of applying f on any sub-element of coll is true"
  (filter f (sub-nodes coll)))

;; The following protocol/extend is for speed only
(defprotocol ReplaceRegexWithString
  (maybe-replace-with-string [this]))

(extend-type java.util.regex.Pattern
  ReplaceRegexWithString
  (maybe-replace-with-string [this] (.toString this)))

(extend-type java.lang.Object
  clojure-refactoring.support.core/ReplaceRegexWithString
  (maybe-replace-with-string [this] this))

(extend-type nil
  ReplaceRegexWithString
  (maybe-replace-with-string [this] nil))

(defn replace-regex [coll]
  "Returns a copy of coll with all regex replaced by the string given by calling toString on them"
  (postwalk
   maybe-replace-with-string
   coll))

(defn maybe-replace-regex [obj]
  (if (seq? obj)
    (replace-regex obj)
    obj))

(defn rec-contains? [coll obj]
  "True if coll contains obj at some level of nesting"
  (some #{(maybe-replace-regex obj)}
        (sub-nodes (maybe-replace-regex coll))))

(defn contains-binding-nodes? [node]
  (some identity
        (for [sym binding-forms] (rec-contains? (rest node) sym))))

(defn last-binding-form? [node]
  "Returns true if there are no binding nodes inside node"
  (and (binding-node? node)
       (not
        (contains-binding-nodes? node))))

(defn tree-replace-if [pred f coll]
  (postwalk
   (fn [n]
     (if (pred n)
       (f n)
       n))
   coll))
