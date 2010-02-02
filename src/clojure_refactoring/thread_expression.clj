(ns clojure_refactoring.thread_expression
  (:use clojure_refactoring.core clojure.walk [clojure.contrib str-utils seq-utils]))

;; TODO: more robust error checking. If we can't thread a function
;; throw an exception instead of trying it anyway

(defn thread-last [code]
  (format-code
   (conj (loop [node (read-string code) new-node '()]
           (if (list? (last node))
             (recur (last node) (conj new-node (butlast node)))
             (conj
              (conj new-node (butlast node))
              (last node))))
         '->>)))

(defn- butsecond [coll]
  (conj (rest (rest coll)) (first coll)))

(defn thread-first [code]
  (format-code
   (conj (loop [node (read-string code) new-node '()]
           (if (list? (second node))
             (recur (second node) (conj new-node (butsecond node)))
             (conj
              (conj new-node (butsecond node))
              (second node))))
         '->)))

(def expression-threaders '#{->> ->})

(defn reverse-seq [coll]
  (if (seq? coll)  (reverse coll) (list coll)))

(defn wrap [inner [outer]]
  "Wraps the inner collection in the outer one."
  (if (not (seq? outer))
    (apply conj inner (reverse-seq outer))
    (list (apply conj inner (reverse-seq outer)))))

(defn- unthread-last [code]
  "Unthread an expression threaded with ->>."
  (loop [[_ & node] (read-string code) new-node '()]
    (if (= (count node) 0)
      (first new-node)
      (recur (rest node)
             (wrap new-node node)))))

(defn- unthread-first [code]
  (loop [[_ & node] (read-string code) new-node '()]
    (if (= (count node) 0)
      new-node
      (recur (rest node)
             (concat (list (first (first node)) new-node) (drop 1 (first node)))))))

(defn thread-unthread [code]
  "Takes an expression starting with ->> or -> and unthreads it"
  (format-code
   (cond (.contains code "->>") (unthread-last code)
         (.contains code "->") (unthread-first code)
         :else (Exception. "No threading expressions found"))))
