(ns clojure-refactoring.rename
  (:use clojure-refactoring.core)
  (:use clojure-refactoring.replace)
  (:use clojure.walk))

(defn rename [node old-name new-name]
  (format-code
   (postwalk-replace {old-name new-name} node)))

(defn global-rename [ns old-name new-name]
  (let [old-var (ns-resolve ns old-name)]
   (replace-all-who-call old-var
                         (renaming-fn old-var new-name))))


