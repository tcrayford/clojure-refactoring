(ns clojure-refactoring.rename-fn
  (:use clojure.walk))

(defn rename-fn [fn-string old-name-as-string new-name-as-string]
  (let [node (read-string fn-string)
        new-name (symbol new-name-as-string)]
    (str
     (postwalk-replace
      {(second node) new-name} node))))
