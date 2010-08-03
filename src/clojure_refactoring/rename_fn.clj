(ns clojure-refactoring.rename-fn
  (:use clojure-refactoring.core)
  (:use clojure.walk))

(defn rename-fn [fn-string old-name-as-string new-name-as-string]
  (let [node (read-string fn-string)
         new-name (symbol new-name-as-string)]

     (format-code
      (postwalk-replace
       {(symbol old-name-as-string) new-name} node))))
