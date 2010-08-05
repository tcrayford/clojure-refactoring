(ns clojure-refactoring.replace
  (:use clojure-refactoring.source)
  (:use clojure.walk))

(defn renaming-fn [old-var new-sym]
  (fn [node]
    (postwalk-replace
     {(.sym old-var) new-sym}
     node)))

(defn map-to-alist [m]
  (map (fn [[k v]] (list k v)) m))

(def line-from-var (comp :line meta))

(defn build-replacement-map [v f]
  {:file (file-from-var v)
   :var-name (.sym v)
   :line (line-from-var v)
   :new-source (f (read-string (get-source-from-cache v)))})

(defn replace-all-who-call [v f]
  (map #(map-to-alist (build-replacement-map % f))
       (all-vars-who-call v)))
