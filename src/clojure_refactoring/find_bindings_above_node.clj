(ns clojure-refactoring.find-bindings-above-node
  (:use clojure-refactoring.core))

(declare process-multiple-nodes
         process-remaining-bindings
         process-nested-bindings)

(defn maybe-keys [m]
  (if (map? m)
    (keys m)
    m))

(defn add-multiple-keys [m]
  (->> (dissoc m :keys)
       (keys)
       (conj (:keys m))
       (remove nil?)))

(defn extract-destructured-map [m]
  (if (and (map? m) (:keys m))
    (add-multiple-keys m)
    (maybe-keys m)))

(def extract-destructured-maps
     (comp flatten (partial map extract-destructured-map)))

(def remove-unwanted-binding-atoms
     (comp unique-vec flatten
           extract-destructured-maps))

(defn find-bindings-above-node [node expr]
  (->> (sub-nodes node)
       (filter seq?)
       (filter #(rec-contains? % expr))
       (filter binding-node?)
       (map (comp extract-destructured-maps bound-symbols))
       (remove-unwanted-binding-atoms)))
