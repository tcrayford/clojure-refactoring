(ns clojure-refactoring.support.namespaces
  (:use [clojure.contrib.find-namespaces :only [find-namespaces-in-dir]])
  (:use [clojure-refactoring.support source core]))

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

(defn require-and-return [ns]
  (do (require (ns-name ns) :reload)
      ns))

(defn reload-all-user-ns []
  (pmap #(require-and-return %)
        (find-ns-in-user-dir)))

(defn all-ns-that-refer-to [v]
  (->> (find-ns-in-user-dir)
       (map require-and-return)
       (filter #(does-ns-refer-to-var? % v))))
