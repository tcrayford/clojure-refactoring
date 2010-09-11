(ns clojure-refactoring.support.formatter
  (:use [clojure-refactoring.support core])
  (:require [clojure-refactoring.ast :as ast]))

(defn replace-content-by [f {content :content :as ast}]
  (if (ast/composite-tag? (:tag ast))
    (ast/replace-content ast
                     `(~(first content)
                       ~@(f (ast/drop-first-and-last content))
                       ~(last content)))
    ast))

(defn add-whitespace-to-lists [ast]
  (ast/walk
   #(replace-content-by ast/add-whitespace %)
   ast))

(defn not-replaced-in-threading? [node]
  (or (some #{(ast/first-content node)} ["->>" "->"])
      (ast/tag= :whitespace node)
      (string? node)))

(defn replace-this-when-threading [ast toplevel]
  (not (or (not-replaced-in-threading? ast)
           (= ast (last (ast/relevant-content toplevel))))))

(def threading-spacing
     [ast/newline ast/whitespace ast/whitespace])

(defn format-threaded [ast]
  (let [with-whitespace
        (add-whitespace-to-lists ast)]
    (ast/replace-content
      with-whitespace
     (after-each #(replace-this-when-threading % with-whitespace)
                 threading-spacing
                 (:content with-whitespace)))))

(defmulti format-ast ast/first-symbol)

(defmethod format-ast "->" [ast]
           (format-threaded ast))

(defmethod format-ast "->>" [ast]
           (format-threaded ast))

(defmethod format-ast "defn" [ast]
           (ast/replace-content
            ast
            (after-each #(= % (ast/parsley-fn-args ast))
                        (drop-last threading-spacing)
                        (:content (add-whitespace-to-lists ast)))))

(defn format-sub-nodes [ast]
  (replace-content-by #(ast/add-whitespace (map format-ast %)) ast))

(defmethod format-ast :default [ast]
           (if (some ast/composite-tag? (ast/relevant-content ast))
             (format-sub-nodes ast)
             (add-whitespace-to-lists ast)))
