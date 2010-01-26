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
