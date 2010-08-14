(ns clojure-refactoring.rename
  (:use clojure-refactoring.support.core)
  (:use clojure-refactoring.support.replace)
  (:use clojure.walk))

(defn rename [node old-name new-name]
  (format-code
   (postwalk-replace {old-name new-name} node)))

(defn renaming-fn [old-var new-sym]
  "Returns a function for renaming nodes"
  (fn [node]
    (postwalk-replace
     {(.sym old-var) new-sym}
     node)))

(defn global-rename [ns old-name new-name]
  "Sends a list of alists to emacs for processing as renames"
  {:pre [(ns-resolve ns old-name)]}
  (let [old-var (ns-resolve ns old-name)]
    (replace-callers old-var (renaming-fn old-var new-name))))
