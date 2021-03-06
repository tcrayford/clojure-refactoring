;; Copyright (c) 2010 Tom Crayford,
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;     Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;
;;     Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials provided
;;     with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

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
