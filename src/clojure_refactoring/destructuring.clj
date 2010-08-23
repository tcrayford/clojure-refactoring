(ns clojure-refactoring.destructuring
  (:use [clojure.contrib str-utils pprint]
        clojure.walk
        [clojure-refactoring.support core parsley])
  (:use [clojure.contrib.seq-utils :only [find-first]]))

(defn parsley-keyword? [ast]
  (and (= (:tag ast) :atom)
       (= (first (apply str (:content ast))) \:)))

(defn ignored-node? [ast]
  (or (string? ast)
      (= (:tag ast) :whitespace)
      (= (:tag ast) :comment)))

;;TODO: needs a better name
(defn relevant-content [ast]
  (remove ignored-node? (:content ast)))

(defn parsley-map-lookup? [ast]
  (let [content (relevant-content ast)]
    (and (= (:tag ast) :list)
         (count= (filter parsley-keyword? content) 1)
         (count= content 2))))

(defn parsley-key->sym [kw-node]
  (assoc kw-node
    :content
    (list
     (apply str
            (drop 1 (apply str (:content kw-node)))))))

(defn parsley-find-lookups [node]
  "Returns all the map lookups in a node as a set of parsley asts"
  (->> (parsley-sub-nodes node)
       (filter parsley-map-lookup?)
       set))

(defn swap-first-with-last [ast]
  (let [[first-node last-node] (relevant-content ast)]
    (replace
     {first-node last-node, last-node first-node}
     (:content ast))))

(defn parsley-swap-first-with-last [ast]
  (assoc
      ast
    :content
    (swap-first-with-last ast)))

(defn parsley-lookup-to-canoninical-form [lookup-ast]
  (let [[maybe-keyword] (relevant-content lookup-ast)]
    (if (parsley-keyword? maybe-keyword)
      lookup-ast
      (parsley-swap-first-with-last lookup-ast))))

(defn add-to-parsley-map [m key val]
  "Adds key and value (which should be parsley nodes
  to m, which represents a parsley map."
  (assoc m
    :content
    `("{"
      ~key
      ~parsley-whitespace
      ~val
      ~parsley-whitespace
      ~@(drop 1 (:content m)))))

(def relevant-content-from-canoninical-form
     (comp relevant-content parsley-lookup-to-canoninical-form))

(defn add-lookup-to-binding-map [binding-map lookup]
  "Adds a lookup (a node of the form (:a a)) to a binding map."
  (let [[key m] (relevant-content-from-canoninical-form lookup)]
    (assoc binding-map m
           (add-to-parsley-map
            (get binding-map m parsley-empty-map)
            (parsley-key->sym key) key))))

(defn lookups-to-binding-map [lookups]
  "Turns a set of lookups to a map of map-symbols to lookups"
  (reduce
   add-lookup-to-binding-map
   {}
   lookups))

(defn- destructured-binding-vec [old-vec lookups]
  "Replaces each key in the binding map found in old-vec with the value\nfrom the binding map"
  (postwalk-replace (lookups-to-binding-map lookups) old-vec))

(defn parsley-fn-args [ast]
  (find-first #(= (:tag %) :vector)
              (:content (parsley-get-first-node ast))))

(defn replace-lookups-with-destructured-symbols [lookups ast]
  ;;TODO: this bothers me, because we use this pattern of reduce
  ;;replacing stuff all over the place
  (reduce
   (fn [new-ast lookup]
     (parsley-tree-replace
      lookup
      (parsley-key->sym (first (relevant-content (parsley-lookup-to-canoninical-form lookup))))
      new-ast))
   ast
   lookups))

(defn add-binding-map [lookups root-ast]
  (let [args (parsley-fn-args root-ast)
        new-args (destructured-binding-vec args lookups)]
    (parsley-tree-replace args new-args root-ast)))

(defn destructure-map [fn-code]
  "Destructures all calls to maps"
  (let [root-ast (parse fn-code)
        lookups (parsley-find-lookups root-ast)]
    (parsley-to-string
     (replace-lookups-with-destructured-symbols
       lookups
       (add-binding-map lookups root-ast)))))
