(ns clojure-refactoring.support.vars
  (:use [clojure-refactoring.support namespaces source core])
  (:use [clojure-refactoring.support.namespaces :only [does-ns-refer-to-var? all-ns-that-refer-to]]))

(defn does-var-call-fn? [v fn]
  "Checks if a var calls a function named 'fn"
  (if-let [source (get-source-from-cache v)]
    (tree-contains? source fn)))

(defn all-vars [nses]
  (->> (map ns-interns nses)
       (map vals)
       flatten))

(defn vars-who-call [v]
  {:pre [(not (nil? v))]}
  (let [sym (.sym v)]
    (->> (all-ns-that-refer-to v)
         all-vars
         (filter #(does-var-call-fn? % sym)))))

(defn populate-cache []
  (pmap cache-source (->> (find-ns-in-user-dir)
                          (map require-and-return)
                          all-vars)))
