(ns clojure-refactoring.support.find-bindings-above-node
  (:use clojure-refactoring.support.core))

(defn maybe-keys [m]
  (if (map? m)
    (keys m)
    m))

(defn add-multiple-keys [m]
  (flatten
   (map (fn [[k v]]
          (if (keyword? k) v k))
        m)))

(defn extract-destructured-map [m]
  "Extracts destructuring from map"
  (if (and (map? m) (:keys m))
    (add-multiple-keys m)
    (maybe-keys m)))

(def extract-destructured-maps
     (comp flatten (partial map extract-destructured-map)))

(def remove-unwanted-binding-atoms
     (comp unique-vec flatten
           extract-destructured-maps))

(defn binding-node-that-contains? [node expr]
  (and (seq? node)
       (binding-node? node)
       (rec-contains? node expr)))

(defn find-bindings-above-node [node expr]
  (->> (sub-nodes node)
       (filter #(binding-node-that-contains? % expr))
       (map (comp extract-destructured-maps bound-symbols))
       remove-unwanted-binding-atoms))
