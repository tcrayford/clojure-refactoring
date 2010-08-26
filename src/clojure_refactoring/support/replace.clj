(ns clojure-refactoring.support.replace
  (:use [clojure-refactoring.support
         source core paths])
  (:use [clojure-refactoring.support.parsley
         :only [parsley-to-string]]))

(defn map-to-alist [m]
  "Converts a clojure map to an alist suitable for emacs"
  (map (fn [[k v]] (list k v)) m))

(def line-from-var (comp :line meta))

(defn build-replacement-map [ns f]
  "Builds a replacement map for emacs for a given namespace"
  {:file (filename-from-ns ns)
   :new-source (parsley-to-string
                (f (:parsley (parsley-from-cache ns))))})

(defn replace-namespaces [namespaces f]
  "Replaces vars by calling f on each one."
  (map #(map-to-alist (build-replacement-map % f)) namespaces))

(defn replace-callers [v f]
  "Replaces all callers of a var by calling a function on them."
  (replace-namespaces (namespaces-who-refer-to v) f))
