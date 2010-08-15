(ns clojure-refactoring.support.replace
  (:use [clojure-refactoring.support source core parsley])
  (:use clojure.walk)
  (:require clojure.contrib.string)
  (:import (java.io StringReader File)
           (java.util.zip ZipFile)
           (clojure.lang LineNumberingPushbackReader)))

(defn map-to-alist [m]
  (map (fn [[k v]] (list k v)) m))

(def line-from-var (comp :line meta))

(defn slime-file-from-var [v]
  (slime-find-file (file-from-var v)))

(defn build-replacement-map [v f]
  {:file (slime-file-from-var v)
   :var-name (.sym v)
   :line (line-from-var v)
   :new-source (parsley-node-to-string
                (f (:parsley (get-entry-from-cache v))))})

(defn replace-vars [vars f]
  (map #(map-to-alist (build-replacement-map % f)) vars))

(defn replace-callers [v f]
  (replace-vars (vars-who-call v) f))
