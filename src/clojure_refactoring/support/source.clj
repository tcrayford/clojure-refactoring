(ns clojure-refactoring.support.source
  (:use clojure.contrib.monads)
  (:use [clojure-refactoring.support core parsley paths])
  (:import (clojure.lang RT)
           (java.io LineNumberReader InputStreamReader PushbackReader)))

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

(defn new-file [path] ;;extracted so we can stub using binding
  (java.io.File. path))

(defn absolute-file-from-var [v]
  ((with-monad maybe-m
     (m-chain [file-from-var
               slime-find-file
               new-file])) v))

(defn new-cached-source [v]
  (when-let [f (absolute-file-from-var v)]
    (let [source (get-source-from-var v)]
      (CachedSource. (.lastModified f)
                     (read-string source)
                     (.getCanonicalPath f)
                     (parse source)))))

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
