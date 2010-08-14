(ns clojure-refactoring.support.parsley
  (:require [net.cgrand.parsley.glr :as core])
  (:use clojure.walk)
  (:use clojure-refactoring.support.core)
  (:use net.cgrand.parsley))

(def sexp
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

    :whitespace (token #{\space \tab \newline \,}:+ (?! #{\space \tab \newline \,}))))

(defn parsley-node-to-string [node]
  (reduce
   str
   (flatten
    (postwalk
     (fn [n]
       (if (map? n)
         (:content n)
         n))
     node))))

(defn parsley-to-string [root-node]
  (apply str (map parsley-node-to-string (second (first root-node)))))
