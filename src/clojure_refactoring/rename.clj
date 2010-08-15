(ns clojure-refactoring.rename
  (:use [clojure-refactoring.support core replace parsley])
  (:use clojure.walk))

(defn rename [node old-name new-name]
  (let [ast (sexp node)
        old (symbol old-name)
        new (symbol new-name)]
    (str (parsley-node-to-string
      (replace-sexp-in-ast old
                           new
                           ast))
         "\n")))

(defn renaming-fn [old-var new-sym]
  "Returns a function for renaming nodes"
  (fn [node]
    (replace-symbol-in-ast-node
     (.sym old-var)
     new-sym
     (second (first node)))))

(defn global-rename [ns old-name new-name]
  "Sends a list of alists to emacs for processing as renames"
  {:pre [(ns-resolve ns old-name)]}
  (let [old-var (ns-resolve ns old-name)]
    (replace-callers old-var (renaming-fn old-var new-name))))
