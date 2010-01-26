(ns clojure_refactoring.extract_method
  (:use clojure_refactoring.core [clojure.contrib str-utils seq-utils]))

(defn fun-call [f-node]
  "Won't work for multiple arity functions"
  (conj (for [arg (fn-args f-node)] arg)
        (nth f-node 1)))

(defn arg-occurences [f-node extracted-node]
  "Finds the occurrences of bindings from f-node in the extracted node.
Works for binding forms in core/*binding-forms*"
  (->>
   extracted-node
   (find-occurences (find-bindings f-node extracted-node))
   (filter #(not= % nil))
   (set)
   (vec)))

(defn new-fn [name args body]
  (conj '() body args (symbol name) 'defn))

(defn escaped-re-pattern [s]
  (re-pattern
   (java.util.regex.Pattern/quote s)))

(defn extract-method [fn-string extract-string new-name]
  "Extracts extract-string out of fn-string and replaces it with a
function call to the extracted method. Only works on single arity root functions"
  (let [function-node (read-string fn-string)
        extract-node (read-string extract-string)
        args (arg-occurences function-node
                             extract-node)
        new-fun (new-fn new-name
                        args
                        extract-node)]
    (str
     (format-code new-fun)
     "\n"
     (re-gsub
      (escaped-re-pattern extract-string)
      (str (fun-call new-fun))
      fn-string))))