(ns clojure-refactoring.rename
  (:use [clojure-refactoring.support replace]
        clojure.walk)
  (:require [clojure-refactoring.support.parser :as parser])
  (:require [clojure-refactoring.support.parsley :as ast]))

(defn rename [node old-name new-name]
  (let [ast (parser/parse node)
        old (symbol old-name)
        new (symbol new-name)]
    (ast/parsley-to-string
     (ast/replace-symbol-in-ast-node
      old
      new
      ast))))

(defn renaming-fn [old-var new-sym]
  "Returns a function for renaming nodes"
  (fn [node]
    (ast/replace-symbol-in-ast-node
     (.sym old-var)
     new-sym
     node)))

(defn global-rename [ns old-name new-name]
  "Sends a list of alists to emacs for processing as renames"
  (let [old-var (ns-resolve ns old-name)]
    (replace-callers old-var (renaming-fn old-var new-name))))
