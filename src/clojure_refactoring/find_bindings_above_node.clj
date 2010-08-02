(ns clojure-refactoring.find-bindings-above-node
  (:use clojure-refactoring.core))

(defn add-binding-form [node bnd-syms]
  "Returns a new binding form from the root node's binding form"
  (into bnd-syms (bound-symbols node)))

(defn contains-binding-nodes? [node]
  (some #{true} (map #(rec-contains? node %) binding-forms)))

(defn more-than-one [pred coll]
  "True if more than one item of coll matches pred"
  (< 1 (count (filter pred coll))))

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

(defn find-bindings-above-node
  "Returns all binding forms above expr in node."
  ([node expr] (find-bindings-above-node node expr []))
  ([node expr bnd-syms]
     (remove-unwanted-binding-atoms
      (cond (more-than-one seq? node)
            (process-multiple-nodes node expr bnd-syms)

            (last-binding-form? node)
            (add-binding-form node bnd-syms)

            (binding-node? node)
            (process-remaining-bindings node expr bnd-syms)

            (seq? (first node))
            (find-bindings-above-node (first node) expr bnd-syms)

            :else
            (find-bindings-above-node (rest node) expr bnd-syms)))))

(defn process-multiple-nodes [node expr bnd-syms]
  (->>
   (filter #(rec-contains? % expr) node)
   (map #(find-bindings-above-node % expr bnd-syms))))

(defn process-remaining-bindings [[_ & remaining :as node] expr bnd-syms]
  (find-bindings-above-node remaining
                            expr
                            (add-binding-form node bnd-syms)))
