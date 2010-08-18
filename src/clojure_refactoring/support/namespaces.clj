(ns clojure-refactoring.support.namespaces
  (:use [clojure.contrib.find-namespaces :only [find-namespaces-in-dir]])
  (:use [clojure-refactoring.support source core paths]))

(defn find-and-load [ns]
  (if (find-ns ns)
    (find-ns ns)
    (do (require ns)
        (find-ns ns))))

(defn find-ns-in-user-dir []
  (->> (java.io.File. (System/getProperty "user.dir"))
       find-namespaces-in-dir
       (map find-and-load)
       (remove nil?)))

(defn does-ns-refer-to-var? [ns v]
  (when v
    (= (ns-resolve ns (.sym v)) v)))

(defonce ns-cache (atom {})) ;; a mapping of namespace-symbols to last
;; modified times

(defn- extract-filename [ns]
  (slime-find-file
   (str
    (.replaceAll
     (.replaceAll (name ns) "-" "_")
     "\\."
     "/")
    ".clj")))

(defn force-ns-name [ns]
  (if (symbol? ns)
    ns
    (ns-name ns)))

(defn filename-from-ns [ns]
  (extract-filename (force-ns-name ns)))

(defn last-modified [ns]
  (when-let [a (filename-from-ns ns)]
    (.lastModified (java.io.File. a))))

(defn ns-in-time? [ns]
  (if-let [cached-time (@ns-cache (force-ns-name ns))]
    (= cached-time (last-modified ns))))

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
