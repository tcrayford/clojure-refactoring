(ns clojure-refactoring.support.core
  (:use [clojure.contrib pprint]
        [clojure.contrib.seq-utils :only [find-first]]
        [clojure.walk :only [postwalk-replace]]))

(defn format-code [node]
  "Outputs code roughly how a human would format it."
  (with-out-str
    (with-pprint-dispatch *code-dispatch*
      (pprint node))))

;; Below stolen from arc
(defn- predicater-by [f] ;; used to build any-of and all-of
  (fn [& fns] (fn [& args]
                (f identity (map apply fns (repeat args))))))

(def any-of? ^{:doc "Takes predicates and returns a function
              that returns true if any of the predicates are true"}
     (predicater-by some))

(def all-of? ^{:doc "Takes predicates and returns a function
              that returns true if all of the predicates are true"}
     (predicater-by every?))

(defn sub-nodes [tree]
  (tree-seq (any-of? sequential? map? set?)
            seq tree))

(defn count= [seq n]
  "Checks if the count of seq is equal to n"
  (= (count seq) n))

(def binding-forms
     #{'let 'fn 'binding 'for 'doseq 'dotimes 'defn 'loop 'defmacro
       'if-let 'when-let 'defn- 'defmethod 'defmethod-})

(defn evens [coll]
  "Returns every other item of coll"
  (take-nth 2 coll))

(defn tree-contains? [coll obj]
  "True if coll contains obj at some level of nesting"
  (some #{obj} (sub-nodes coll)))

(defn replace-when [pred f coll]
  "Replaces each element of coll if pred returns true on it."
  (map
   (fn [elem]
     (if (pred elem) (f elem) elem)) coll))

(defn first= [x y]
  (= (first x) y))

(defn but-second [coll]
  (->> (first coll)
       (conj (drop 2 coll))))

(defn after-each [pred elems coll]
  "After each item in coll that matches predicate
   add elems."
  (reduce
   (fn [accum elem]
     (if (pred elem)
       `(~@accum ~elem ~elems)
       `(~@accum ~elem)))
   ()
   coll))
