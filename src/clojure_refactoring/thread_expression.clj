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

(defn wrap [inner outer]
  "Wraps the inner collection in the outer one."
  (if (not (seq? (first outer)))
    (apply conj inner (reverse-seq (first outer)))
    (list (apply conj inner (reverse-seq (first outer))))))

(defn unthread-last [code]
  "Unthread an expression threaded with ->>."
  (loop [node (read-string code) new-node '()]
    (if (= (first node) '->>)
      (recur (rest node) '())
      (if (= (count node) 0)
        (first new-node)
        (recur (rest node)
               (wrap new-node node))))))

(defn unthread-first [code]
  (loop [node (read-string code) new-node '()]
    (if (= (first node) '->)
      (recur (rest node) '())
      (if (= (count node) 0)
        (first new-node)
        (recur (rest node)
               (wrap new-node node))))))

(eval (unthread-first
       (format-code '(-> 1
                         (/ 32)
                         (* 1.8 2 3)
                         (+ 42)))))

(defn thread-unthread [code]
  "Takes an expression starting with ->> or -> and unthreads it"
  (format-code
   (if (.contains code "->>")
     (unthread-last code)
     (Exception. "No threading expressions found"))))

(thread-unthread
 (format-code '(->> sym
                    (rec-contains? (rest node))
                    (for [sym *binding-forms*])
                    (some #(= % true))
                    (not))))