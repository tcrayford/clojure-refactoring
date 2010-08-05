(ns clojure-refactoring.rename
  (:use clojure-refactoring.core)
  (:use clojure-refactoring.replace)
  (:use clojure.walk))

(defn rename [node old-name new-name]
  (format-code
   (postwalk-replace {old-name new-name} node)))

(defn global-rename [old-var new-name]
  (replace-all-who-call old-var
                        (renaming-fn old-var new-name)))


