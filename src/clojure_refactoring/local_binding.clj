(ns clojure_refactoring.local_binding
  (:use clojure.walk clojure_refactoring.core clojure.contrib.seq-utils))

(defn local-wrap [toplevel value var-name]
  (let [top (read-string toplevel)
        value (read-string value)]
    (format-code
     (wrap-function-def-with-let
       (postwalk-replace {value (symbol var-name)} top)
       value
       (symbol var-name)))))

(defn wrap-function-def-with-let [defn-form value var-name]
  (let [fn-def (get-function-definition defn-form)]
    (postwalk-replace {fn-def `(~'let [~var-name ~value] ~fn-def)} defn-form)))


(defn get-function-definition [defn-form]
  (find-first list? defn-form))

(defn wrap [inside outside]
  (concat outside (conj '() inside)))



