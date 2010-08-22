(ns clojure-refactoring.support.parsley
  (:require [net.cgrand.parsley.glr :as core])
  (:use [clojure-refactoring.support.find-bindings-above-node :only [munge-anonymous-fns]])
  (:use clojure.walk)
  (:use clojure-refactoring.support.core)
  (:use net.cgrand.parsley))

(defonce sexp
  (memoize
   (parser {:space [#{:whitespace :comment :discard}:*]
            :main :expr*}
           :expr- #{:atom :list :vector :set :map :string :regex
                    :meta :deprecated-meta :quote
                    :unquote :syntax-quote :unquote-splicing
                    :deref :var :fn :char}
           :atom1st- #{{\a \z \A \Z \0 \9} (any-of "!$%&*+-./:<=>?_")}
           :atom (token :atom1st #{:atom1st \#}:* (?! #{:atom1st \#}))
           :string (token \" #{(none-of \\ \") [\\ any-char]}:* \")
           :char (token \\ #{any-char "newline" "space" "tab" "backspace"
                             "formfeed" "return"
                             (into [\u] (repeat 4 {\0 \9 \a \f \A \F}))
                             [\u :hex :hex :hex :hex]
                             [\o {\0 \7}]
                             [\o {\0 \7} {\0 \7}]
                             [\o {\0 \3} {\0 \7} {\0 \7}]}
                        (?! #{:atom1st \#}))
           :regex (token \# \" #{(none-of \\ \") [\\ any-char]}:* \")
           :list ["(" :expr* ")"]
           :vector ["[" :expr* "]"]
           :set ["#{" :expr* "}"]
           :map ["{" :expr* "}"]
           :discard ["#_" :expr]
           :meta ["^" :expr :expr]
           :quote [\' :expr]
           :syntax-quote [\` :expr]
           :tilda- [\~ (?! \@)]
           :unquote [:tilda :expr]
           :unquote-splicing ["~@" :expr]
           :deprecated-meta ["#^" :expr :expr]
           :deref [\@ :expr]
           :var ["#'" :expr]
           :fn ["#(" :expr* ")"]

           :comment (token #{"#!" ";"} (none-of \newline):* (?! (none-of \newline)))

           :whitespace (token #{\space \tab \newline \,}:+ (?! #{\space \tab \newline \,})))))

(def parse (comp second first sexp))

(defn ast-symbol [sym]
  {:tag :atom, :content (list (name sym))})

(def composite-tag? (complement
                     #{:atom :regex :space :var :char :string}))

(declare parsley-walk)

(defn- replace-content [f {tag :tag content
                           :content :as ast}]
  (assoc ast
    :content
    (replace-when
     (complement string?)
     (if (composite-tag? tag)
       #(parsley-walk f %)
       f)
     content)))

(defn parsley-walk [f ast]
     (if (map? ast)
       (f
        (replace-content f ast))
       (vec (map #(parsley-walk f %) ast))))

(defn- parsley-sub-nodes [ast]
  (tree-seq #(or (sequential? %)
                 (composite-tag? %))
            #(if (sequential? %)
               (seq %)
               (:content %)) ast))

(defn parsley-node-to-string [ast]
  (apply str (filter string? (parsley-sub-nodes ast))))

(defn- gensym? [s]
  (and (symbol? s)
       (or (.contains (name s) "__auto__")
           (.contains (name s) "p__"))))

(defn- munged-gensym [n]
  (symbol (str "gensym-" n)))

(defn- munge-gensyms [sexp]
  (replace-in-sexp
   (filter gensym? (sub-nodes sexp))
   (map munged-gensym (iterate inc 0))
   sexp))

(def munge-node ;;To replace stuff that read-string changes
     (comp munge-gensyms munge-anonymous-fns maybe-replace-regex))

(defn match-parsley [exp ast]
  (try
    (let [ex (munge-node exp)]
      (= ex (munge-node
             (read-string (parsley-node-to-string ast)))))
    (catch Exception e nil)))

(defn- replace-symbol [node old new]
  (if (= node (ast-symbol old))
    (ast-symbol new)
    node))

(defn replace-symbol-in-ast-node [old new ast]
  (parsley-walk
   #(replace-symbol % old new)
   ast))

(defn replace-sexp-in-ast-node [old new ast]
  "Takes a partial ast and replaces old (as represented by a sexp) with new (also represented by a sexp)"
  (let [new-ast (second (first (sexp (pr-str new))))]
    (parsley-walk
     (fn [{content :content :as node}]
       (if (and (map? node) content (match-parsley old node))
         new-ast
         node))
     ast)))
