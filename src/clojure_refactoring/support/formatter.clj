(ns clojure-refactoring.support.formatter
  (:use [clojure-refactoring.support parsley core]))

(defn replace-content-by [f {content :content :as ast}]
  (if (composite-tag? (:tag ast))
    (replace-content ast
                     `(~(first content)
                       ~@(f (drop-first-and-last content))
                       ~(last content)))
    ast))

(defn add-whitespace-to-lists [ast]
  (parsley-walk
   #(replace-content-by add-whitespace %)
   ast))

(defn not-replaced-in-threading? [node]
  (or (some #{(first-content node)} ["->>" "->"])
      (tag= :whitespace node)
      (string? node)))

(defn replace-this-when-threading [ast toplevel]
  (not (or (not-replaced-in-threading? ast)
           (= ast (last (relevant-content toplevel))))))

(def threading-spacing
     [parsley-newline parsley-whitespace parsley-whitespace])

(defn format-threaded [ast]
  (let [with-whitespace
        (add-whitespace-to-lists ast)]
    (replace-content
      with-whitespace
     (after-each #(replace-this-when-threading % with-whitespace)
                 threading-spacing
                 (:content with-whitespace)))))

(defmulti format-ast first-symbol)

(defmethod format-ast "->" [ast]
           (format-threaded ast))

(defmethod format-ast "->>" [ast]
           (format-threaded ast))

(defmethod format-ast "defn" [ast]
           (replace-content
            ast
            (after-each #(= % (parsley-fn-args ast))
                        (drop-last threading-spacing)
                        (:content (add-whitespace-to-lists ast)))))

(defn format-sub-nodes [ast]
  (replace-content-by #(add-whitespace (map format-ast %)) ast))

(defmethod format-ast :default [ast]
           (if (some composite-tag? (relevant-content ast))
             (format-sub-nodes ast)
             (add-whitespace-to-lists ast)))
