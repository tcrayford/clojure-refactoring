(ns clojure-refactoring.rename
  (:use [clojure-refactoring.support replace]
        clojure-refactoring.ast.zip
        clojure-refactoring.support.find-bindings-above-node)
  (:require [clojure.zip :as zip])
  (:require [clojure-refactoring.support.parser :as parser])
  (:require [clojure-refactoring.ast :as ast]))

(defn rename-in-ast [ast old new]
  (ast/replace-symbol-in-ast-node
   old
   new
   ast))

(defn rename [node old-name new-name]
  (let [ast (parser/parse node)
        old (symbol old-name)
        new (symbol new-name)]
    (ast/ast->string
     (rename-in-ast ast old new))))

(defn rename-node [loc new-name]
  (if ((bindings-above-loc loc) (zip/node loc))
    loc
    (zip/replace loc (ast/symbol new-name))))

(defn renaming-fn [old-var new-sym]
  "Returns a function for renaming nodes"
  (fn [node]
    (loop [loc (ast-zip node)]
      (cond (zip/end? loc)
            (zip/root loc)

            (= (zip/node loc) (ast/symbol (.sym old-var)))
            (recur (zip/next (rename-node loc new-sym)))

            :else
            (recur (zip/next loc))))))

(defn global-rename [ns old-name new-name]
  "Sends a list of alists to emacs for processing as renames"
  (let [old-var (ns-resolve ns old-name)]
    (replace-callers old-var (renaming-fn old-var new-name))))
