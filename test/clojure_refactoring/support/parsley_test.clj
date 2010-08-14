(ns clojure-refactoring.support.parsley-test
  (:use clojure-refactoring.support.parsley :reload)
  (:use clojure.test)
  (:require [clojurecheck.core :as cc]))

(def symbol-chars (vec "abcdefghijklmnopqrstuvwxyz"))

(defn random-symbol-char []
  (rand-nth symbol-chars))

(defn random-symbol []
  (symbol (reduce str (take (inc 2)
                            (repeatedly random-symbol-char)))))

(defn random-sexp []
  (into () (take (rand 10)
              (repeatedly random-symbol))))

(defn random-sexp-with-comments []
  (fn [size]
    (str (pr-str (random-sexp)) ";;" (random-symbol) )))

(deftest parsley_to_string
  (cc/property "parsley to string is an inverse of sexp"
               [s (random-sexp-with-comments)]
               (is (= (parsley-to-string (sexp s))
                      s))))
