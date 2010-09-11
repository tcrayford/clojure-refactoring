(ns clojure-refactoring.support.replace
  (:use [clojure-refactoring.support
         source core paths])
  (:require [clojure-refactoring.ast :as ast]))

(defn map-to-alist [m]
  "Converts a clojure map to an alist suitable for emacs"
  (map (fn [[k v]] (list k v)) m))

(def line-from-var (comp :line meta))

(defn build-replacement-map [ns f]
  "Builds a replacement map for emacs for a given namespace"
  (let [replacement (map f (parsley-from-cache ns))]
    (if (= (parsley-from-cache ns) replacement)
      nil
     {:file (filename-from-ns ns)
      :new-source (ast/ast->string
                   replacement)})))

(defn replace-namespaces [namespaces f]
  "Replaces vars by calling f on each one."
  (remove empty?
          (map #(map-to-alist (build-replacement-map % f)) namespaces)))

(defn replace-callers [v f]
  "Replaces all callers of a var by calling a function on them."
  (replace-namespaces (namespaces-who-refer-to v) f))
