;; Paredit in Clojure mode
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)

;; ClojureScript REPL for inferior lisp mode
(defun use-browser-repl () 
  (setq inferior-lisp-program "lein trampoline cljsbuild repl-listen"))
(add-hook 'clojurescript-mode-hook 'use-browser-repl)

;; Launch `lein run`
(defun make-lein-run-buffer ()
  (interactive)
  (async-shell-command "lein trampoline run") ; save memory
  (with-current-buffer "*Async Shell Command*"
    (rename-buffer "*lein run*")))

;; Visit all relevant buffers in a Clojure web project
(defun open-clojure-web-project ()
  (visit-files-by-types '("html" "css" "clj" "cljs")))

;; Open everything, start server
(defun clojure-web-jack-in ()
  (open-clojure-web-project)
  (make-lein-run-buffer))

(provide 'my-clojure)
