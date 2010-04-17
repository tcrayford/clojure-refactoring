(ns clojure_refactoring.core
  (:use [clojure.contrib str-utils duck-streams seq-utils pprint] clojure.walk)
  (:import clojure.lang.Named))

(defn is-defn? [node]
  "Returns true if the current node is a function definition"
  (= (first node) 'defn))

(defn format-code [node]
  "Prints code roughly how a human would format it"
  (with-out-str (with-pprint-dispatch *code-dispatch*
                  (pprint node))))

(defn find-occurences [args node]
  "Looks for any occurence of each element of args in the node
TODO: doesn't handle destructuring properly"
  (flatten (loop [arg-set (set args) node node]
             (for [sub-node node]
               (if (seq? sub-node)
                 (or (find-occurences arg-set sub-node))
                 (arg-set sub-node))))))

(defmacro if-true [expr]
  "Always returns true or false, depending on the value of its body"
  `(if ~expr true false))

(def binding-forms
     #{'let 'fn 'binding 'for 'doseq 'dotimes 'defn 'loop})

(defn binding-node? [node]
  "Checks if a node is a binding node"
  (binding-forms (first node)))

(defn evens [coll]
  "Returns the even items of a collection"
  (->> (indexed coll)
       (filter #(even? (first %)))
       (map #(last %))
       (vec)))

(defn fn-args [node]
  "Returns the function arguments from a top-level defn node"
  (find-first #(vector? %) node))

(defn binding-form [node]
  "Returns a vector of bindings iff the node is a binding node. Won't work with multiple arity defns"
  (if (binding-node? node)
    (fn-args node)))

(defn unique-vec [coll]
  "Strips all duplicates from coll and forces it into a vector"
  (vec (apply sorted-set coll)))


(defn bound-symbols [node]
  "Returns a vector of the bound symbols inside node"
  (unique-vec
   (if (is-defn? node)
     (binding-form node)
     (evens (binding-form node)))))

(defn some-true? [coll]
  "Returns true if anything in the collection is true"
  (some #(= % true) coll))

(defn rec-contains? [coll obj]
  "True if coll contains obj at some level of nesting"
  (some-true?
        (flatten
         (postwalk
          (fn [node]
            (if (= node obj)
              true
              (if (= node true) false node)))
          coll))))

(defn last-binding-form? [node]
  "Returns true if there are no binding nodes inside node"
  (and (binding-node? node)
       (not
        (some-true?
         (for [sym binding-forms]
           (rec-contains? (rest node) sym))))))

(defn add-binding-form [node bnd-syms]
  "Returns a new binding form from the root node's binding form with"
  (into bnd-syms (bound-symbols node)))

(defn find-bindings-above-node
  "Returns any let bindings above expr in node"
  ([node expr] (find-bindings-above-node node expr []))
  ([node expr bnd-syms]
     (unique-vec
      (if (last-binding-form? node)
        (add-binding-form node bnd-syms)
        (if (binding-node? node)
          (find-bindings-above-node (rest node)
                         expr
                         (add-binding-form node bnd-syms))
          (if (seq? (first node))
            (find-bindings-above-node (first node) expr bnd-syms)
            (find-bindings-above-node (rest node) expr bnd-syms)))))))

;; Taken from compojure.
(defn map-str
  "Map a function to a collection, then concatenate the results into a
  string."
  [func coll]
  (apply str (map func coll)))

(defn str*
  "A version of str that prefers the names of Named objects.
  e.g (str \"Hello \" :World)  => \"Hello :World\"
      (str* \"Hello \" :World) => \"Hello World\""
  [& args]
  (map-str
   #(if (instance? Named %) (name %) (str %))
   args))
