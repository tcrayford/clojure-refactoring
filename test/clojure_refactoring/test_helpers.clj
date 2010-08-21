(ns clojure-refactoring.test-helpers
  (:use clojure.test)
  (:require [clojurecheck.core :as cc])
  (:require [clojure.contrib.repl-utils :as repl-utils]))

(def memoized-get-source
     (memoize repl-utils/get-source))

(def symbol-chars (vec "abcdefghijklmnopqrstuvwxyz"))

(defn random-symbol-char [& args]
  (rand-nth symbol-chars))

(defn random-symbol [& args]
  (symbol (reduce str (take (inc (rand 10))
                            (repeatedly random-symbol-char)))))

(defn random-sexp [& args]
  (into () (take (inc (rand 10))
                 (repeatedly random-symbol))))

(defn random-sexp-with-comments [& args]
  (str (pr-str (random-sexp)) ";;" (random-symbol) ))

(defn random-sexp-from-core [& args]
  (let [result (memoized-get-source
                (rand-nth
                 (keys (ns-publics 'clojure.core))))]
    (if result result
        (random-sexp-from-core))))
