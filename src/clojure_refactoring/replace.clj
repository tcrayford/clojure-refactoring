(ns clojure-refactoring.replace
  (:use [clojure-refactoring source core])
  (:use clojure.walk)
  (:require clojure.contrib.string)
  (:import (java.io StringReader File)
           (java.util.zip ZipFile)
           (clojure.lang LineNumberingPushbackReader)))


;;the below stolen from slime
(defn- clean-windows-path [#^String path]
  ;; Decode file URI encoding and remove an opening slash from
  ;; /c:/program%20files/... in jar file URLs and file resources.
  (or (and (.startsWith (System/getProperty "os.name") "Windows")
           (second (re-matches #"^/([a-zA-Z]:/.*)$" path)))
      path))

(defn- slime-file-resource [#^java.net.URL resource]
  (clean-windows-path (.getFile resource)))

(defn- slime-find-resource [#^String file]
  (let [resource (.getResource (clojure.lang.RT/baseLoader) file)]
    (slime-file-resource resource)))

(defn- slime-find-file [#^String file]
  (if (.isAbsolute (File. file))
    file
    (slime-find-resource file)))
;; end stealing from slime

(defn renaming-fn [old-var new-sym]
  (fn [node]
    (postwalk-replace
     {(.sym old-var) new-sym}
     node)))

(defn map-to-alist [m]
  (map (fn [[k v]] (list k v)) m))

(def line-from-var (comp :line meta))

(defn slime-file-from-var [v]
  (slime-find-file (file-from-var v)))

(defn build-replacement-map [v f]
  {:file (slime-file-from-var v)
   :var-name (.sym v)
   :line (line-from-var v)
   :new-source (clojure.contrib.string/butlast 1
                (format-code
                 (f (read-string (get-source-from-cache v)))))})

(defn replace-all [vars f]
  (map #(map-to-alist (build-replacement-map % f)) vars))

(defn replace-all-who-call [v f]
  (replace-all (all-vars-who-call v) f))
