(ns clojure-refactoring.local-binding
  (:use clojure.walk
        [clojure-refactoring.support core parsley formatter]
        [clojure.contrib.seq-utils :only (find-first)]))

(defn- get-function-definition [defn-ast]
  (find-first #(tag= :list %) (:content defn-ast)))

(defn- add-to-binding [{content :content :as binding-node}
                       value var-name]
  (assoc binding-node
    :content
    `(~(first content)
      ~@(butlast (drop 1 content))
      ~parsley-whitespace
      ~var-name
      ~parsley-whitespace
      ~value
      ~(last content))))

(defn is-node-the-binding-form [top-level ast]
  (= ast (parsley-fn-args top-level)))

(defn- modify-existing-let-block [form value var-name]
  (parsley-walk
   (fn [ast]
     (if (is-node-the-binding-form form ast)
       (add-to-binding ast value var-name)
       ast))
   form))

(defn let-wrap [form value var-name]
  (if (parsley-binding-node? form)
    (modify-existing-let-block form value var-name)
    {:tag :list :content
     `("(" ~(ast-symbol 'let)
       ~parsley-whitespace
       ~(parsley-vector
         [var-name value])
       ~parsley-whitespace
       ~form ")")}))

(defn- wrap-function-body-with-let [defn-form value var-name]
  (let [fn-def (get-function-definition defn-form)]
    (parsley-tree-replace
     fn-def (let-wrap fn-def value var-name)
     defn-form)))

(defparsed-fn local-wrap [top value var-name]
  "Extracts a value as a local variable inside top"
  (-> (wrap-function-body-with-let
        (parsley-tree-replace
         value var-name top)
        value var-name)
      parsley-to-string))
