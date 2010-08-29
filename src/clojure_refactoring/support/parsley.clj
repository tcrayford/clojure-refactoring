(ns clojure-refactoring.support.parsley
  (:require [net.cgrand.parsley.glr :as core])
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

(def parsley-empty-map {:tag :map :content (list "{" "}") })

(def parsley-whitespace
     '{:tag :whitespace :content (" ")})

(def composite-tag? (complement
                     #{:atom :regex :space :var :char :string}))

(declare parsley-walk)

(defn- replacement-for-composite [tag f]
  (if (composite-tag? tag)
    #(parsley-walk f %)
    f))

(defn- replacement-for-content [tag f content]
  (replace-when
    (complement string?)
    (replacement-for-composite tag f)
    content))

(defn- replace-content [f ast]
  (let [{tag :tag content :content} ast]
   (assoc ast
     :content
     (replacement-for-content tag f content))))

(defn parsley-walk [f ast]
     (if (map? ast)
       (f
        (replace-content f ast))
       (vec (map #(parsley-walk f %) ast))))

(defn parsley-sub-nodes [ast]
  (tree-seq #(or (sequential? %)
                 (composite-tag? %))
            #(if (sequential? %)
               (seq %)
               (:content %)) ast))

(defn parsley-tree-contains [ast obj]
  (some #{obj} (parsley-sub-nodes ast)))

(defn parsley-to-string [ast]
  (apply str (filter string? (parsley-sub-nodes ast))))

(defn parsley-tree-replace [old new ast]
  (parsley-walk
   (fn [node]
     (if (= node old)
       new
       node))
   ast))

(defn replace-symbol-in-ast-node [old new ast]
  (parsley-tree-replace (ast-symbol old) (ast-symbol new) ast))

(defn parsley-get-first-node [ast]
  (if (map? ast) ast (first ast)))

(defn parsley-symbol? [ast]
  (and (map? ast)
       (= (:tag ast) :atom)
       (symbol?
        (read-string (apply str (:content ast))))))

(defn parsley-keyword? [ast]
  (and (= (:tag ast) :atom)
       (= (first (apply str (:content ast))) \:)))

(defn ignored-node? [ast]
  (or (string? ast)
      (= (:tag ast) :whitespace)
      (= (:tag ast) :comment)))

;;TODO: needs a better name
(defn relevant-content [ast]
  (remove ignored-node? (:content ast)))

