(ns clojure-refactoring.support.find-bindings-above-node
  (:use clojure-refactoring.support.core)
  (:use clojure.walk))

(defn maybe-keys [m]
  "If m is a map, call keys on it. Otherwise return it"
  (if (map? m)
    (keys m)
    m))

(defn add-multiple-keys [m]
  "Pull everything out of a binding map that is a symbol"
  (distinct
   (filter symbol? (flatten
                    (sub-nodes m)))))

(defn extract-destructured-map [m]
  "Extracts destructuring from map"
  (if (map? m)
    (add-multiple-keys m)
    (maybe-keys m)))

(def extract-destructured-maps
     "Extracts all symbols out of destructured maps"
     (comp flatten (partial map extract-destructured-map)))

(def remove-unwanted-binding-atoms
     (comp unique-vec flatten
           extract-destructured-maps))

(defn binding-node-that-contains? [node expr]
  "Returns true if node is a binding node that
contains expr"
  (and (seq? node)
       (binding-node? node)
       (rec-contains? node expr)))

(def munge-cache (atom {})) ;; Stores the results of munging anonymous functions, indexed by a
;; printout of that node's code, so that munging
;; the same node always returns the same results.
;; We do this so we can check if a node contains an anonymous fn
;; literal.

(defn munge-node [node]
  (reduce
   (fn [new-node [original munged]]
     (postwalk-replace {original munged} new-node))
   node
   (map vector (fn-args node) (repeatedly gensym))))

(defn munge-anonymous-fn [node]
  "Munges anonymous fn args out of a node. Always returns the same
value for a given node"
  (if-let [output (@munge-cache (format-code node))]
    output
    (let [output (munge-node node)]
      (do (swap! munge-cache assoc (format-code node) output)
          output))))

(defn munge-anonymous-fns [node]
  "Replaces all anonymous fns in a node with munged versions"
  (tree-replace-when anonymous-fn? munge-anonymous-fn node))

(defn anonymous-fn? [node]
  "Returns true if node is an anonymous function"
  (and (seq? node) (= (first node) 'fn*)))

(defn find-bindings-above-node [node expr]
  "Finds all binding expressions above expr in a particular node"
  (let [munged-node (munge-anonymous-fns node)
        munged-expr (munge-anonymous-fns expr)]
    (->> (sub-nodes munged-node)
         (filter #(binding-node-that-contains? % munged-expr))
         (map (comp extract-destructured-maps bound-symbols))
         remove-unwanted-binding-atoms)))
