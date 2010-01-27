clojure-refactoring
---

Provides several automated refactorings of clojure code. These are
relatively rigorous, but use with a test suite and some manual
checking (at this stage).

Available refactorings
----------------------

Thread-last - threads the current expression via ->>
Thread-first - threads the current expression via ->
Unthread - unthreads the current expression.
TODO: this only works with ->>

Extract-fn - extracts the current expression and replaces it with a
call to it.
Extract global - defines the current expression as a global var for
this namespace (using def).

Emacs dependancies
---
Slime, a running clojure connection, paredit, and thing at point.

Usage
---

Bind a key to clojure-refactoring-ido, and make sure you select
what you want to operate on first.

Example keybinding:
    (global-set-key (kbd "s-e") 'clojure-refactoring-ido)

Eval the file clojure-refactoring-mode.el inside emacs.
TODO: this should be a minor-mode.
TODO: provide a default binding for clojure-refactoring-ido

NOTE: Very alpha, has some breakages. Report any problems via Github issues.

Installation
---

Add a dev-dependancies to your lein/swank enabled project, then run
`lein deps`

For example (inside project.clj)
    :dev-dependencies [[leiningen/lein-swank "1.0.0-SNAPSHOT"]
                       [clojure_refactoring "0.1.1"]]


License
---
Copyright (C) 2009 Tom Crayford.

