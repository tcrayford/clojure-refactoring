(ns clojure-refactoring.support.core
  (:use [clojure.contrib pprint]
        [clojure.contrib.seq-utils :only (find-first)]
        [clojure.walk :only [postwalk-replace]]))

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

(defn expand-args-with-parse1 [args]
  "Takes arguments from a function and returns a vector that
  (in a let form) rebinds them by parsing them."
  (->> (mapcat #(list % (list 'parse1 %)) args) vec))

(defmacro defparsed-fn [name args docstring & body]
  "Defines a function in which all of the args are rebound by parsing them using parse1."
  `(defn ~name ~args ~docstring
     (let ~(expand-args-with-parse1 args)
       ~@body)))
