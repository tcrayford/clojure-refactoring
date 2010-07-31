(ns clojure-refactoring.thread-expression
  (:use clojure-refactoring.core
        clojure.walk
        [clojure.contrib str-utils]))

(defn but-second [coll]
  (conj (drop 2 coll) (first coll)))

(defn threading-fns-from-type [type]
  "Returns functions to be used by thread-with-type
based on what type of threading is going to be"
  ({'-> {:position-f second
         :all-but-position-f but-second}
    '->> {:position-f last
          :all-but-position-f butlast}} type))

;; TODO: more robust error checking. If we can't thread a function
;; throw an exception instead of trying it anyway
(defn thread-with-type [thread-type code]
  (loop [node (read-string code) new-node '()]
    (let [{:keys [position-f all-but-position-f]}
          (threading-fns-from-type thread-type)]
      (if (list? (position-f node))
        (recur
          (position-f node)
          (conj new-node (all-but-position-f node)))
        (conj
          (conj new-node (all-but-position-f node))
          (position-f node))))))

(defn construct-threaded [thread-type code]
  (format-code
   (conj
    (thread-with-type thread-type code)
    thread-type)))

(def thread-last
     (partial construct-threaded '->>))

(def thread-first
     (partial construct-threaded '->))

(def expression-threaders '#{->> -> clojure.core/->> clojure.core/->})

(defn threaded? [node]
  (and (seq? node) (expression-threaders (first node))))

(defn extract-threaded [coll]
  (if (threaded? coll)
    (macroexpand-1 coll)
    coll))

(defn thread-unthread [code]
  "Takes an expression starting with ->> or -> and unthreads it"
  (format-code
   (loop [node (read-string code)]
     (if (some #(rec-contains? node %) expression-threaders)
       (recur
        (postwalk
         extract-threaded
         node))
       node))))
