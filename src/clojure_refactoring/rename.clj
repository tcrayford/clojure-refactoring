(ns clojure-refactoring.rename
  (:use clojure-refactoring.core)
  (:use clojure.walk))

(defn rename [node old-name new-name]
  (format-code
   (postwalk-replace {old-name new-name} node)))
