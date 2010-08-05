(ns clojure-refactoring.source
  (:use clojure-refactoring.core)
  (:use [clojure.contrib.find-namespaces :only [find-namespaces-in-dir]])
  (:import (clojure.lang RT)
           (java.io LineNumberReader InputStreamReader PushbackReader)))

(defonce source-cache (atom {}))

(defrecord CachedSource [time source file])

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

(defn find-ns-in-user-dir []
  (->>
   (java.io.File. (System/getProperty "user.dir"))
   (find-namespaces-in-dir)
   (map find-ns)
   (remove nil?)))

(defn new-file [file-path]
  (java.io.File. file-path))

(defn in-time? [cached]
  (= (.lastModified (new-file (:file cached)))
     (:time cached)))

(def file-from-var (comp :file meta))

(defn new-cached-source [v]
  (when-let [file-path (file-from-var v)]
    (when-let [f (new-file file-path)]
      (CachedSource. (.lastModified f)
                     (get-source-from-var v)
                     file-path))))

(defn cache-source [var]
  (if-let [x (new-cached-source var)]
    (do (swap! source-cache #(assoc % var x))
        (:source x))))

(defn get-source-from-cache [var]
  (if-let [cached (@source-cache var)] ;; use anaphoric aand for this
    (if (in-time? cached)
      (:source cached)
      (cache-source var))
    (cache-source var)))

(defn- does-var-call-fn [var fn]
  "Checks if a var calls a function named 'fn"
  (if-let [source (get-source-from-cache var)]
    (let [node (read-string source)]
      (if (rec-contains? node fn)
        var
        false))))

(defn does-ns-refer-to-var? [ns v]
  (= (ns-resolve ns (.sym v)) v))

(defn require-and-return [ns]
  (do (require (ns-name ns) :reload)
      ns))

(defn all-ns-that-refer-to [var]
  (->> (find-ns-in-user-dir)
       (map require-and-return)
       (filter #(does-ns-refer-to-var? % var))))

(defn all-vars [nses]
  (->> (map ns-interns nses)
       (map vals)
       (flatten)))

(defn populate-cache []
  (doseq [v (->> (map require-and-return (find-ns-in-user-dir))
                 (all-vars))]
    (cache-source v)))

(defn empty-cache []
  (reset! source-cache {}))

(defn all-vars-who-call [var]
  (let [sym (.sym var)]
    (->> (all-ns-that-refer-to var)
         (all-vars)
         (map #(does-var-call-fn % sym))
         (filter #(identity %)))))

(defn source-for-vars-who-call [var]
  (map (comp read-string get-source-from-cache) ;; TODO: duplicating this, can we do
       ;; better?
   (all-vars-who-call var)))
