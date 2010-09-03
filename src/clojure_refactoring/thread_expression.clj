(ns clojure-refactoring.thread-expression
  (:use [clojure-refactoring.support core parsley]
        [clojure.walk :only [postwalk]]))

(defn threading-fns-from-type [type]
  "Returns functions to be used by thread-with-type
based on what type of threading is going to be"
  ({'-> {:position-f second
         :all-but-position-f but-second}
    '->> {:position-f last
          :all-but-position-f butlast}} type))

(defn finish-threading [node new-node thread-type]
  (let [{:keys [position-f all-but-position-f]}
        (threading-fns-from-type thread-type)]
    `(~(position-f node) ~(all-but-position-f node) ~@new-node)))

(defn- not-last-threading-node? [node position-f]
  (and (list? (position-f node))
       (list? (position-f (position-f node)))))

(defn- thread-with-type [thread-type code]
  (let [{:keys [position-f all-but-position-f]}
        (threading-fns-from-type thread-type)]
    (loop [node (read-string code) new-node '()]
      (if (not-last-threading-node? node position-f)
        (recur (position-f node)
               (conj new-node (all-but-position-f node)))
        (finish-threading node new-node thread-type)))))

(defn- construct-threaded [thread-type code]
  (apply str (butlast (format-code
             `(~thread-type ~@(thread-with-type thread-type code))))))

(def thread-last
     (partial construct-threaded '->>))

(def thread-first
     (partial construct-threaded '->))

(def expression-threaders '#{->> -> clojure.core/->> clojure.core/->})

(def threaded?
     (all-of? seq? (comp expression-threaders first)))

(defn- expand-threaded [coll]
  (if (threaded? coll)
    (macroexpand-1 coll)
    coll))

(defn- expand-all-threaded [node]
  (postwalk expand-threaded node))

(defn- any-threaded? [node]
  (some #(tree-contains? node %) expression-threaders))

(defn thread-unthread [code]
  "Takes an expression starting with ->> or -> and unthreads it"
  (format-code
   (loop [node (read-string code)]
     (if (any-threaded? node)
       (recur
        (expand-all-threaded node))
       node))))
