(ns clojure-refactoring.rename-binding
  (:use clojure-refactoring.core clojure.walk [clojure.contrib str-utils]))

(defn rename-binding [code old-name new-name]
  (format-code
   (let [node (read-string code)]
     (postwalk-replace
      (hash-map (symbol old-name) (symbol new-name))
      node))))
