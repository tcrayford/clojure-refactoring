(ns clojure-refactoring.destructuring
  (:use [clojure.contrib str-utils duck-streams seq-utils pprint] clojure.walk clojure-refactoring.core))

(defn count= [seq n]
  "Checks if the count of seq is equal to n"
  (= (count seq) n))

(defn is-map-lookup?
  "Returns true if node is a map lookup"
  ([node]
     (and (seq? node)
          (or (keyword? (first node)) (keyword? (second node)))
          (= (count node) 2))))

(defn key->sym [kw]
  "Turns a keyword into a symbol"
  (symbol (name kw)))

(defn find-lookups [root-node]
  "Returns all the map lookups in a node as a set of seqs"
  (loop [node root-node accum #{}]
    (let [current-node (first node)]
      (if (count= node 0)
        accum
        (if (is-map-lookup? current-node)
          (recur (rest node) (conj accum current-node))
          (if (seq? current-node)
            (recur current-node accum)
            (recur (rest node) accum)))))))

(defn lookup->canoninical-form [lookup]
  "Forces a lookup of the form (map :key) into (:key map)
TODO: this needs a better name"
  (if (keyword? (first lookup)) lookup (reverse lookup)))

(defn lookups->binding-map [lookups]
  "Converts #{(:a a) (:b a)} to {a {a :a b :a}}"
  (loop [l lookups binding-map {}]
    (if (empty? l)
      binding-map
      (let [lookup (first l)
            [key m] (lookup->canoninical-form lookup)]
        (recur
         (rest l)
         (assoc binding-map m
                (assoc (get binding-map m {}) (key->sym key) key)))))))

(defn destructured-binding-vec [old-vec binding-map]
  "Replaces each key in the binding map found in old-vec with the value
from the binding map"
  (postwalk-replace binding-map old-vec))

(defn replace-lookup-with-destructured-symbol [lookups node]
  "Replaces node with a destructured symbol from lookups
if this node is contained in lookups"
  (if (lookups node)
    (key->sym (first (lookup->canoninical-form node)))
    node))

(defn add-binding-map [lookups root-node]
  "Takes a set of lookups and a function node, and
adds a binding map made from the lookups to the root node"
  (let [args (fn-args root-node)]
    (map
      #(if (= % args)
        (destructured-binding-vec args (lookups->binding-map lookups))
        %)
      root-node)))

(defn destructure-map [fn-code name]
  "Destructures all calls to map called name inside a function node"
  (let [root-node (read-string fn-code)
        lookups (find-lookups root-node)]
    (format-code
     (postwalk
      (partial
       replace-lookup-with-destructured-symbol
       lookups)
      (add-binding-map lookups root-node)))))
