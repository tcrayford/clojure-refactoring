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
     (comp flatten (partial map extract-destructured-map)))

(def remove-unwanted-binding-atoms
     (comp unique-vec flatten
           extract-destructured-maps))

(defn anonymous-fn? [node]
  (and (seq? node) (= (first node) 'fn*)))

(defn binding-node-that-contains? [node expr]
  (and (seq? node)
       (binding-node? node)
       (rec-contains? node expr)))

(def munge-cache (atom {}))

(defn munge-node [node]
  (reduce
    (fn [accum [original munged]]
      (postwalk-replace {original munged} accum))
    node
    (map vector (fn-args node) (repeatedly gensym))))

(defn munge-anonymous-fn [node]
  (if-let [output (@munge-cache (format-code node))]
    output
    (let [output (munge-node node)]
      (do (swap! munge-cache assoc (format-code node) output)
          output))))

(defn munge-anonymous-fns [node]
  (tree-replace-if anonymous-fn? munge-anonymous-fn node))

(defn find-bindings-above-node [node expr]
  (let [munged-node (munge-anonymous-fns node)
        munged-expr (munge-anonymous-fns expr)]
   (->> (sub-nodes munged-node)
        (filter #(binding-node-that-contains? % munged-expr))
        (map (comp extract-destructured-maps bound-symbols))
        remove-unwanted-binding-atoms)))
