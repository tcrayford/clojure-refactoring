(ns clojure_refactoring.extract_method
  (:use clojure_refactoring.core clojure.contrib.str-utils))

(defn fun-call [f-node]
  (conj (for [arg (fn-args f-node)] arg) (nth f-node 1)))

(defn arg-occurences [f-node extracted-node]
  (vec (filter #(not= % nil) (find-occurences (fn-args f-node) extracted-node))))

(defn new-fn [name args body]
  (conj '() body args name 'defn))

(defn extract-method [fn-string extract-string new-name]
  "Extracts node out of root-node and replaces it with a
function call to the extracted method. Only works on single arity root functions"
  (let [args (arg-occurences (read-string fn-string)
                             (read-string extract-string))
        new-fun (new-fn (symbol new-name)
                        args
                        (read-string extract-string))]
    (str
     (with-out-str (pr-code new-fun))
     "\n"
     (re-gsub
      (re-pattern (java.util.regex.Pattern/quote extract-string))
      (str (fun-call new-fun))
      fn-string))))

