(ns clojure-refactoring.support.parsley
  (:require [net.cgrand.parsley.glr :as core])
  (:use clojure.walk
        clojure-refactoring.support.core
        net.cgrand.parsley
        [clojure.contrib.seq-utils :only [find-first]]
        [clojure.contrib.str-utils :only [str-join]]))

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

(defn expand-ast-nodes [ast]
  (if (sequential? ast)
     (seq ast)
     (:content ast)))

(defn parsley-sub-nodes [ast]
  (tree-seq (orf sequential? composite-tag?)
            expand-ast-nodes
            ast))

(defn parsley-tree-contains [ast obj]
  (some #{obj} (parsley-sub-nodes ast)))

(defn parsley-to-string [ast]
  (str-join "" (filter string? (parsley-sub-nodes ast))))

(defn parsley-tree-replace [old new ast]
  (parsley-walk
   (fn [node] (if (= node old) new node))
   ast))

(defn replace-symbol-in-ast-node [old new ast]
  (parsley-tree-replace (ast-symbol old) (ast-symbol new) ast))

(defn parsley-get-first-node [ast]
  (if (map? ast) ast (first ast)))

(defn tag= [x ast]
  (= (:tag ast) x))

(def parsley-atom?
     (partial tag= :atom))

(defn ast-content [ast]
  (str-join "" (:content ast)))

(def parsley-symbol?
     (andf map? parsley-atom?
           (comp symbol? read-string ast-content)))

(def parsley-keyword?
     (andf parsley-atom?
           #(first= (ast-content %) \:)))

(def ignored-node?
     (orf string? #(tag= :whitespace %) #(tag= :comment %)))

;;TODO: needs a better name
(defn relevant-content [ast]
  (remove ignored-node? (:content ast)))

(defn intersperse [coll item]
  (interleave coll (repeat item)))

(defn add-whitespace [coll]
  (butlast (intersperse coll parsley-whitespace)))

(defn coll-fn [tag start end]
  (fn [coll]
    {:tag tag :content `(~start ~@(add-whitespace coll) ~end)}))

(def parsley-list
     (coll-fn :list "(" ")"))

(def parsley-vector
     (coll-fn :vector "[" "]"))

(def parsley-newline
     {:tag :whitespace :content '("\n")})

(def parse1 (comp first parse)) ;;parses one node

(def sexp->parsley (comp parse1 format-code))

(defn first-vector [ast]
  (first (filter #(tag= :vector %) (:content ast))))

(defn parsley-fn-args [ast]
  (first-vector
   (parsley-get-first-node ast)))

(def parsley-bindings
     (comp relevant-content parsley-fn-args))
