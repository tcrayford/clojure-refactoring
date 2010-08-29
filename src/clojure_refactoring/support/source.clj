(ns clojure-refactoring.support.source
  (:use [clojure.contrib.find-namespaces :only [find-namespaces-in-dir]])
  (:use [clojure-refactoring.support core paths parsley]))

(defn find-and-load [namespace]
  (if (find-ns namespace)
    (find-ns namespace)
    (do (require namespace)
        (find-ns namespace))))

(defn find-ns-in-user-dir []
  (->> (java.io.File. (System/getProperty "user.dir"))
       find-namespaces-in-dir
       (map find-and-load)))

(defn last-modified [namespace]
  (when-let [a (filename-from-ns namespace)]
    (.lastModified (java.io.File. a))))

(defonce ns-cache (atom {})) ;; a mapping of namespace-symbols to
;; cache entries

(defrecord NameSpaceCacheEntry [time parsley])
;; Time is the time this cache entry was created, parsley is the
;; result of calling parsley after slurping the file

(defn new-ns-entry [namespace]
  (when-let [f (filename-from-ns namespace)]
    (let [slurped (slurp f)
          parsed (parse slurped)]
      (NameSpaceCacheEntry. (.lastModified (java.io.File. f))
                            parsed))))

(defn cache-entry-in-time? [namespace-name cached]
  (= (last-modified namespace-name) (:time cached)))

(defn ns-in-time? [namespace-name]
  (if-let [cached (@ns-cache namespace-name)]
    (cache-entry-in-time? namespace-name cached)))

(defn entry-from-cache [namespace-name]
  (if-let [cached (@ns-cache namespace-name)]
    (if (cache-entry-in-time? namespace-name cached)
      cached
      (new-ns-entry namespace-name))
    (new-ns-entry namespace-name)))

(def parsley-from-cache
     (comp :parsley entry-from-cache))

(defn add-to-ns-cache! [ns]
  (swap! ns-cache assoc (force-ns-name ns) (new-ns-entry ns)))

(defn reload [ns]
  (do (add-to-ns-cache! ns)
      (require ns :reload)
      ns))

(defn require-and-return [ns]
  (do (when-not (ns-in-time? ns)
        (reload (ns-name ns)))
      ns))

(defn reload-all-user-ns []
  (pmap #(require-and-return %)
        (find-ns-in-user-dir)))

(defn does-ns-refer-to-var? [namespace v]
  (when v
    (= (ns-resolve namespace (.sym v)) v)))

(defn namespaces-who-refer-to [v]
  (->> (find-ns-in-user-dir)
       (pmap require-and-return)
       (filter #(does-ns-refer-to-var? % v))))
