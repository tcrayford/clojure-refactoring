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
  (if ((bindings-above loc) (zip/node loc))
    loc
    (zip/replace loc (ast/symbol new-name))))

(defn rename-non-shadowed [new-sym node old-name]
  (zip-walk (ast-zip node)
            #(if (= (zip/node %) (ast/symbol old-name))
               (rename-node % new-sym) %)))

(defn renaming-fn [old-var new-sym]
  "Returns a function for renaming nodes"
  (fn [node]
    (rename-non-shadowed new-sym node (.sym old-var))))

(defn global-rename [ns old-name new-name]
  "Sends a list of alists to emacs for processing as renames."
  (let [old-var (ns-resolve ns old-name)]
    (replace-callers old-var (renaming-fn old-var new-name))))
