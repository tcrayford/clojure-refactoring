(ns clojure-refactoring.thread-expression
  (:use clojure-refactoring.core
        clojure.walk
        [clojure.contrib str-utils]))

;; TODO: more robust error checking. If we can't thread a function
;; throw an exception instead of trying it anyway
(defn thread-last [code]
  (format-code
   (conj (loop [node (read-string code) new-node '()]
           (if (list? (last node))
             (recur (last node) (conj new-node (butlast node)))
             (conj
              (conj new-node (butlast node))
              (last node))))
         '->>)))

(defn- butsecond [coll]
  (conj (rest (rest coll)) (first coll)))

(defn thread-first [code]
  (format-code
   (conj (loop [node (read-string code) new-node '()]
           (if (list? (second node))
             (recur (second node) (conj new-node (butsecond node)))
             (conj
              (conj new-node (butsecond node))
              (second node))))
         '->)))

(def expression-threaders '#{->> -> clojure.core/->> clojure.core/->})

(defn threaded? [node]
  (and (seq? node) (expression-threaders (first node))))

(defn extract-threaded [coll]
  (if (threaded? coll)
    (macroexpand-1 coll)
    coll))

(defn- unthread-last [node]
  "Unthread an expression threaded with ->>."
  (if (some #(rec-contains? node %) expression-threaders)
    (recur
     (postwalk
      extract-threaded
      node))
    node))

(defn thread-unthread [code]
  "Takes an expression starting with ->> or -> and unthreads it"
  (format-code
   (loop [node (read-string code)]
     (if (some #(rec-contains? node %) expression-threaders)
    (recur
     (postwalk
      extract-threaded
      node))
    node))))
