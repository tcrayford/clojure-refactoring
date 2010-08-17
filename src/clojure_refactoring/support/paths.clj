(ns clojure-refactoring.support.paths
  (:import java.io.File))

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

(defn slime-find-file [#^String file]
  (if (.isAbsolute (File. file))
    file
    (slime-find-resource file)))

(def file-from-var (comp :file meta))

(defn slime-file-from-var [v]
  (slime-find-file (file-from-var v)))
