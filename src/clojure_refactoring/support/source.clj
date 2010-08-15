(ns clojure-refactoring.support.source
  (:use clojure.contrib.monads)
  (:use [clojure-refactoring.support core parsley])
  (:use [clojure.contrib.find-namespaces :only [find-namespaces-in-dir]])
  (:import (clojure.lang RT)
           (java.io LineNumberReader InputStreamReader PushbackReader))
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

(defn slime-find-file [#^String file]
  (if (.isAbsolute (File. file))
    file
    (slime-find-resource file)))
;; end stealing from slime

(defonce source-cache (atom {}))

(defrecord CachedSource [time source file parsley])

;; Yoinked and modified from clojure.contrib.repl-utils.
;; Now takes a var instead of a sym in the current ns
(defn get-source-from-var
  "Returns a string of the source code for the given symbol, if it can
find it. This requires that the symbol resolve to a Var defined in
a namespace for which the .clj is in the classpath. Returns nil if
it can't find the source.
Example: (get-source-from-var 'filter)"
  [v] (when-let [filepath (:file (meta v))]
        (when-let [strm (.getResourceAsStream (RT/baseLoader) filepath)]
          (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
            (dotimes [_ (dec (:line (meta v)))] (.readLine rdr))
            (let [text (StringBuilder.)
                  pbr (proxy [PushbackReader] [rdr]
                        (read [] (let [i (proxy-super read)]
                                   (.append text (char i))
                                   i)))]
              (read (PushbackReader. pbr))
              (str text))))))

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

(defn new-file [path] ;;extracted so we can stub using binding
  (java.io.File. path))

(def file-from-var (comp :file meta))

(defn absolute-file-from-var [v]
  ((with-monad maybe-m
     (m-chain [file-from-var
               slime-find-file
               new-file])) v))

(defn new-cached-source [v] ;; refactor this using maybe monad?
  (when-let [f (absolute-file-from-var v)]
    (let [source (get-source-from-var v)]
      (CachedSource. (.lastModified f)
                     source
                     (.getCanonicalPath f)
                     (sexp source)))))

(defn in-time? [cached]
  (= (.lastModified (new-file (:file cached)))
     (:time cached)))

(defn cache [v]
  (if-let [x (new-cached-source v)]
    (do (swap! source-cache #(assoc % v x))
        x)))

(defn cache-source [v]
  (:source (cache v)))

(defn get-entry-from-cache [v]
  (if-let [cached (@source-cache v)]
    (if (and (:file cached) (in-time? cached))
      cached
      (cache v))
    (cache v)))

(defn get-source-from-cache [v]
  (if-let [entry (get-entry-from-cache v)]
    (:source entry)))

(defn- does-var-call-fn? [v fn]
  "Checks if a var calls a function named 'fn"
  (if-let [source (get-source-from-cache v)]
    (rec-contains? (read-string source) fn)))

(defn does-ns-refer-to-var? [ns v]
  (when v
    (= (ns-resolve ns (.sym v)) v)))

(defn reload-all-user-ns []
  (map #(require (ns-name %) :reload) (find-ns-in-user-dir)))

(defn require-and-return [ns]
  (do (require (ns-name ns) :reload)
      ns))

(defn all-ns-that-refer-to [v]
  (->> (find-ns-in-user-dir)
       (map require-and-return)
       (filter #(does-ns-refer-to-var? % v))))

(defn all-vars [nses]
  (->> (map ns-interns nses)
       (map vals)
       (flatten)))

(defn populate-cache []
  (doseq [vars (->> (map require-and-return (find-ns-in-user-dir))
                    (all-vars))]
    (cache-source vars)))

(defn empty-cache []
  (reset! source-cache {}))

(defn vars-who-call [v]
  {:pre [(not (nil? v))]}
  (let [sym (.sym v)]
    (->> (all-ns-that-refer-to v)
         (all-vars)
         (filter #(does-var-call-fn? % sym)))))
