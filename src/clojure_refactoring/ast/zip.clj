(ns clojure-refactoring.ast.zip
  (:require [clojure.zip :as zip]))

(clojure.contrib.def/defalias ast-zip zip/xml-zip)

(defn find-node [zipped-ast node]
  "Given a zipped ast, returns the loc of the first occurrence of node
   by a depth-first walk."
  (loop [z zipped-ast]
    (cond (zip/end? z) nil
          (= (zip/node z) node) z
          :else (recur (zip/next z)))))

(defn nodes-leading-to [ast expr]
  "Returns a seq of all nodes inside node leading to expr."
  (-> (find-node (ast-zip ast) expr)
      zip/path))

(defn zip-walk [zipper f]
  "Performs a depth first walk over zipper, calling a function
   on each sub-node."
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (f loc))))))
