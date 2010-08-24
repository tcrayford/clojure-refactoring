(ns clojure-refactoring.support.namespaces
  (:use [clojure.contrib.find-namespaces :only [find-namespaces-in-dir]])
  (:use [clojure-refactoring.support core paths]))

(defn find-and-load [namespace]
  (if (find-ns namespace)
    (find-ns namespace)
    (do (require namespace)
        (find-ns namespace))))

(defn find-ns-in-user-dir []
  (->> (java.io.File. (System/getProperty "user.dir"))
       find-namespaces-in-dir
       (map find-and-load)
       (remove nil?)))

(defn does-ns-refer-to-var? [namespace v]
  (when v
    (= (ns-resolve namespace (.sym v)) v)))

(defonce ns-cache (atom {})) ;; a mapping of namespace-symbols to last
;; modified times

(defn- extract-filename [namespace]
  (slime-find-file
   (str
    (.replaceAll
     (.replaceAll (name namespace) "-" "_")
     "\\."
     "/")
    ".clj")))

(defn force-ns-name [namespace]
  (if (symbol? namespace)
    namespace
    (ns-name namespace)))

(defn filename-from-ns [namespace]
  (extract-filename (force-ns-name namespace)))

(defn last-modified [namespace]
  (when-let [a (filename-from-ns namespace)]
    (.lastModified (java.io.File. a))))

(defn get-cached-time [namespace]
  (@ns-cache (force-ns-name namespace)))

(defn ns-in-time? [namespace]
  (if-let [cached-time get-cached-time]
    (= cached-time (last-modified namespace))))

(defn reload [ns]
  (do (swap! ns-cache assoc ns (last-modified ns))
      (require ns :reload)
      ns))

(defn require-and-return [ns]
  (do
    (if (not (ns-in-time? ns))
      (reload (ns-name ns)))
    ns))

(defn reload-all-user-ns []
  (pmap #(require-and-return %)
        (find-ns-in-user-dir)))

(defn all-ns-that-refer-to [v]
  (->> (find-ns-in-user-dir)
       (map require-and-return)
       (filter #(does-ns-refer-to-var? % v))))
