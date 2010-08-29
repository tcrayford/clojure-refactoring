(ns clojure-refactoring.test-helpers
  (:use clojure.test)
  (:use [clojure.contrib.def :only [defalias]])
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

(defn random-sexp-from-core [& args]
  (let [result (memoized-get-source
                (rand-nth
                 (keys (ns-publics 'clojure.core))))]
    (if result result
        (random-sexp-from-core))))

(defn proxy-file [time]
  (proxy [java.io.File] ["~/"] (lastModified [] time)
         (getCanonicalPath [] "absolute-path")))

(defalias prop cc/property)

(defmacro modified? [reference & exprs]
  "Checks if a reference is modified whilst running exprs.
   Use can be made readable by doing
   (modified reference :during expr)"
  `(let [intial# @~reference]
     (do ~@exprs)
     (not= @~reference intial#)))
