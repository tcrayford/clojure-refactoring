(ns clojure_refactoring.destructuring
  (:use [clojure.contrib str-utils duck-streams seq-utils pprint] clojure.walk clojure_refactoring.core))


(defn count= [seq n]
  (= (count seq) n))

(defn is-map-lookup?
  ([n]
     (and
      (seq? n)
      (keyword? (first n))
      (= (count n) 2)))
  ([name n]
     (and
      (seq? n)
      (keyword (first n))
      (= (count n) 2)
      (= (last n (symbol name))))))

(defn key->sym [kw]
  (symbol (str* kw)))

(defn find-lookups [node]
  (if (is-map-lookup? node)
    (into #{} (vector node))
    (loop [n node accum #{}]
      (let [current-node (first n)]
        (if (count= n 0)
          accum
          (if (is-map-lookup? current-node)
            (recur (rest n) (conj accum current-node))
            (if (seq? current-node)
              (recur current-node accum)
              (recur (rest n) accum))))))))

(defn lookups->binding-map [lookups]
  "Converts #{(:a a) (:b a)} to {a {a :a b :a}}"
  (loop [l lookups h {}]
    (if (empty? l)
      h
      (let [lookup (first l)
            [key m] lookup]
        (recur (rest l)
               (assoc
                   h
                 m
                 (assoc (get h m {})
                   (key->sym key)
                   key)))))))

(defn destructured-binding-vec [old-vec binding-map]
  (postwalk-replace binding-map old-vec))

(defn destructure-map [fn-code name]
  (let [node (read-string fn-code)
        lookups (find-lookups node)
        bnd-map (lookups->binding-map lookups)]
    (format-code
     (postwalk
      (fn [n]
        (if (lookups n)
          (key->sym (first n))
          n))
      (postwalk-replace
       (hash-map (fn-args node) (destructured-binding-vec (fn-args node) bnd-map))
       node)))))

