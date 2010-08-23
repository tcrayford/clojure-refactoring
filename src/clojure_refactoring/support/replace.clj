(ns clojure-refactoring.support.replace
  (:use [clojure-refactoring.support
         source core paths vars])
  (:use [clojure-refactoring.support.parsley
         :only [parsley-to-string]]))

(defn map-to-alist [m]
  "Converts a clojure map to an alist suitable for emacs"
  (map (fn [[k v]] (list k v)) m))

(def line-from-var (comp :line meta))

(defn build-replacement-map [v f]
  "Builds a replacement for a var (to be sent to emacs) by calling f on the parsley tree
from that var."
  {:file (slime-file-from-var v)
   :var-name (.sym v)
   :line (line-from-var v)
   :new-source (parsley-to-string
                (f (:parsley (get-entry-from-cache v))))})

(defn replace-vars [vars f]
  "Replaces vars by calling f on each one."
  (map #(map-to-alist (build-replacement-map % f)) vars))

(defn replace-callers [v f]
  "Replaces all callers of a var by calling a function on them."
  (replace-vars (vars-who-call v) f))
