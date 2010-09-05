(ns clojure-refactoring.support.formatter
  (:use [clojure-refactoring.support parsley core]))

(def ptag= #(partial tag= %))

(def drop-first-and-last (comp rest butlast))

(def first-content (comp first :content))

(defn first-symbol [ast]
  (first-content (first (relevant-content  ast))))

(defn replace-content [ast new-content]
  (assoc ast
    :content
    new-content))

(defn after-each [pred elems coll]
  (reduce
   (fn [accum elem]
     (if (pred elem)
       `(~@accum ~elem ~elems)
       `(~@accum ~elem)))
   ()
   coll))

(defn replace-content-by [{content :content :as ast} f]
  (if (composite-tag? (:tag ast))
    (replace-content ast
                     `(~(first content)
                       ~@(f (drop-first-and-last (remove (any-of? nil? empty?) content)))
                       ~(last content)))
    ast))

(defmulti format-ast first-symbol)

(defn or= [obj & xs]
  (some #{obj} xs))

(defn replaced-in-threading? [node]
  (or (or= (first-content node) "->>" "->")
      (tag= :whitespace node)
      (string? node)))

(defn add-whitespace-to-lists [ast]
  (parsley-walk
   #(replace-content-by % add-whitespace)
   ast))

(defn replace-this-when-threading [ast toplevel]
  (not (or (replaced-in-threading? ast)
           (= ast (last (relevant-content toplevel))))))

(def threading-spacing `(~parsley-newline ~@(repeat 2 parsley-whitespace)))

(defn format-threaded [ast]
  (let [with-whitespace
        (add-whitespace-to-lists ast)]
    (replace-content
     with-whitespace
     (after-each #(replace-this-when-threading % with-whitespace)
                 threading-spacing
                 (:content with-whitespace)))))

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

(defmethod format-ast :default [ast]
           (add-whitespace-to-lists ast))
