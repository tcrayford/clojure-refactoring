(ns clojure_refactoring.thread_expression
  (:use clojure_refactoring.core [clojure.contrib str-utils seq-utils]))

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