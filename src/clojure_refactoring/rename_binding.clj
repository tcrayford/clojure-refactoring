(ns clojure_refactoring.rename_binding
  (:use clojure_refactoring.core clojure.walk [clojure.contrib str-utils seq-utils]))

(defn rename-binding [code old-name new-name]
  (format-code
   (let [node (read-string code)
         [old new] (map symbol [old-name new-name])]
     (postwalk
      (fn [x]
        (if (= x old)
          new
          x))
      node))))

