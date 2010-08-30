(ns clojure-refactoring.rename
  (:use [clojure-refactoring.support replace parsley]
        clojure.walk))

(defn rename [node old-name new-name]
  (let [ast (parse node)
        old (symbol old-name)
        new (symbol new-name)]
    (parsley-to-string
     (replace-symbol-in-ast-node
      old
      new
      ast))))

(defn renaming-fn [old-var new-sym]
  "Returns a function for renaming nodes"
  (fn [node]
    (replace-symbol-in-ast-node
     (.sym old-var)
     new-sym
     node)))

(defn global-rename [ns old-name new-name]
  "Sends a list of alists to emacs for processing as renames"
  #_{:pre [(do (find-ns ns)
             (ns-resolve ns old-name))]}
  (let [old-var (ns-resolve ns old-name)]
    (replace-callers old-var (renaming-fn old-var new-name))))
