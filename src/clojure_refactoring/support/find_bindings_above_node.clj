(ns clojure-refactoring.support.find-bindings-above-node
  (:use clojure-refactoring.support.core)
  (:use clojure.walk))

(defn add-multiple-keys [m]
  "Pull everything out of a binding map that is a symbol"
  (distinct
   (filter symbol? (sub-nodes m))))

(defn- extract-destructured-map [m]
  "Extracts destructuring from map"
  (if (map? m)
    (add-multiple-keys m)
    m))

(def extract-destructured-maps ;;Extracts all symbols out of destructured maps
     (comp flatten (partial map extract-destructured-map)))

(def remove-unwanted-binding-atoms
     (comp unique-vec flatten
           extract-destructured-maps))

(defn binding-node-that-contains? [node expr]
  "Returns true if node is a binding node that
contains expr"
  (and (seq? node)
       (binding-node? node)
       (tree-contains? node expr)))

(defn symbol-from-num [n]
  "Produces a symbol from a number by prepending
% to it"
  (str "%" n))

(defn munge-anonymous-fn [node]
  "Munges anonymous fn args out of a node. Always returns the same
value for a given node"
  (replace-in-sexp
   (bindings node)
   (map symbol-from-num (iterate inc 1))
   node))

(defn anonymous-fn? [node]
  "Returns true if node is an anonymous function"
  (and (seq? node) (= (first node) 'fn*)))

(defn munge-anonymous-fns [node]
  "Replaces all anonymous fns in a node with munged versions"
  (tree-replace-when anonymous-fn? munge-anonymous-fn node))

(defn find-bindings-above-node [node expr]
  "Finds all binding expressions above expr in a particular node"
  (let [munged-node (munge-anonymous-fns node)
        munged-expr (munge-anonymous-fns expr)]
    (->> (sub-nodes munged-node)
         (filter #(binding-node-that-contains? % munged-expr))
         (map (comp extract-destructured-maps bound-symbols))
         remove-unwanted-binding-atoms)))
