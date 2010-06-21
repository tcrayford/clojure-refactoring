(ns clojure-refactoring.local-binding
  (:use clojure.walk clojure-refactoring.core))

(defn get-function-definition [defn-form]
  "Gets the function body out of a defn form"
  (find-first list? defn-form))

(defn add-to-binding [binding-node value var-name]
  (conj binding-node var-name value))

(defn is-node-the-binding-form? [top-level node]
  (= node (extract-binding-form top-level)))

(defn modify-existing-let-block [form value var-name]
  (postwalk
       (fn [node]
         (if (is-node-the-binding-form? form node)
           (add-to-binding node value var-name)
           node))
       form))

(defn let-wrap [form value var-name]
  (if (binding-node? form)
    (modify-existing-let-block form value var-name)
    `(~'let [~var-name ~value] ~form)))

(defn wrap-function-body-with-let [defn-form value var-name]
  (let [fn-def (get-function-definition defn-form)]
    (postwalk-replace
     {fn-def (let-wrap fn-def value var-name)} defn-form)))

(defn local-wrap [toplevel value var-name]
  (let [top (read-string toplevel)
        value (read-string value)]
    (format-code
     (wrap-function-body-with-let
       (postwalk-replace {value (symbol var-name)} top)
       value
       (symbol var-name)))))
