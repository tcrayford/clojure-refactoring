(ns clojure-refactoring.destructuring
  (:use [clojure.contrib str-utils pprint]
        clojure.walk
        [clojure-refactoring.support core parsley]))

(defn map-lookup? [node]
  "Returns true if node is a map lookup using keywords"
  (and (seq? node)
       (count= (filter keyword? node) 1)
       (count= node 2)))

(defn key->sym [kw] ;;TODO: this should go into core
  "Turns a keyword into a symbol"
  (symbol (name kw)))

(defn find-lookups [node]
  "Returns all the map lookups in a node as a set of seqs"
  (->> (sub-nodes node)
       (filter map-lookup?)
       set))

(defn lookup->canoninical-form [lookup]
  "Forces a lookup of the form (map :key) into (:key map)
TODO: this needs a better name"
  (if (keyword? (first lookup))
    lookup
    (reverse lookup)))

(defn lookups->binding-map [lookups]
  "Converts #{(:a a) (:b a)} to {a {a :a b :a}}"
  (reduce
   (fn [binding-map lookup]
     (let [[key m] (lookup->canoninical-form lookup)]
       (assoc binding-map m
              (assoc (get binding-map m {}) (key->sym key) key))))
   {} lookups))

(defn destructured-binding-vec [old-vec lookups]
  "Replaces each key in the binding map found in old-vec with the value\nfrom the binding map"
  (postwalk-replace (lookups->binding-map lookups) old-vec))

;; TODO: refactor all code below this line with a parsley monad
(defn replace-lookups-with-destructured-symbols [lookups ast]
  (reduce
    (fn [ast lookup]
      (replace-sexp-in-ast-node
        lookup
        (key->sym (first (lookup->canoninical-form lookup)))
        ast))
    ast
    lookups))

(defn add-binding-map [lookups root-node root-ast]
  "Takes a set of lookups and a function node, and
adds a binding map made from the lookups to the root node"
  (let [args (fn-args root-node)]
    (replace-sexp-in-ast-node args
                         (destructured-binding-vec
                          args lookups)
                         root-ast)))

(defn destructure-map [fn-code name]
  "Destructures all calls to map called name inside a function node"
  (let [root-ast (parse fn-code)
        root-node (read-string fn-code)
        lookups (find-lookups root-node)]
    (str (parsley-node-to-string
          (replace-lookups-with-destructured-symbols
            lookups
            (add-binding-map lookups root-node root-ast)))
         "\n")))
