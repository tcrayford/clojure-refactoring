(ns clojure-refactoring.support.source
  (:use [clojure.contrib.find-namespaces :only [find-namespaces-in-dir]]
        [clojure-refactoring.support core paths])
  (:import java.io.File)
  (require [clojure-refactoring.support.parser :as parser]))

(defn- find-and-load [namespace]
  (when-not (find-ns namespace)
    (require namespace))
  (find-ns namespace))

(defn find-ns-in-user-dir []
  (->> (File. (System/getProperty "user.dir"))
       find-namespaces-in-dir
       (map find-and-load)))

(defn- last-modified [namespace]
  (.lastModified (File. (filename-from-ns namespace))))

(defonce ns-cache (atom {})) ;; a mapping of namespace-symbols to
;; cache entries

(defrecord NameSpaceCacheEntry [time parsley namespace])
;; Time is the time this cache entry was created, parsley is the
;; result of calling parsley after slurping the file

(defn new-ns-entry [namespace]
  (let [f (filename-from-ns namespace)
        parsed (parser/parse (slurp f))]
    (NameSpaceCacheEntry. (last-modified namespace)
                          parsed
                          namespace)))

(defn- in-time? [cached]
  (= (last-modified (:namespace cached)) (:time cached)))

(defmacro with-cached [namespace-name & body]
  `(if-let [~'cached (@ns-cache ~namespace-name)]
     ~@body))

(defn in-time? [namespace-name]
  (with-cached namespace-name
    (in-time? cached)))

(defn- entry-from-cache [namespace-name]
  (with-cached namespace-name
    (if (in-time? cached)
      cached
      (new-ns-entry namespace-name))
    (new-ns-entry namespace-name)))

(def parsley-from-cache (comp :parsley entry-from-cache))

(defn- add-to-ns-cache! [ns]
  (swap! ns-cache assoc (force-ns-name ns) (new-ns-entry ns)))

(defn- reload [ns]
  (add-to-ns-cache! ns)
  ns)

(defn require-and-return [ns]
  (if (in-time? ns) ns
      (reload (ns-name ns))))

(def reload-all-user-ns
     #(pmap require-and-return (find-ns-in-user-dir)))

(defn bound-in? [namespace v]
  (when v (= (ns-resolve namespace (.sym v)) v)))

(defn namespaces-who-refer-to [v]
  (->> (find-ns-in-user-dir)
       (pmap require-and-return)
       (filter #(bound-in? % v))))
