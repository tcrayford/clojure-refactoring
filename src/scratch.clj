(def chat-s (read-string
             (str "("
                  (slurp  "/Users/tcrayford/Projects/hallway/src/chat.clj") ")")))

(find-first #(= % 'defn) chat-s)

(defn pr-code [ast]
  (with-pprint-dispatch *code-dispatch* (pprint ast)))

(for [node chat-s
      :when  (= (first node) 'defn)]
  node)

(defn fn-args [ast]
  "Returns all the top level vectors in an ast"
  (for [node ast :when (vector? node)] node))

;; as long as the namespace is loaded, we can do (source fn) to get
;; the source for a var
;; Can also do (meta #'fn) to get metadata for a function
;; Can use slime-defun-at-point for getting function text

(pr-code (for [node chat-s
      :when  (= (first node) 'defn)]
           (fn-args node)))