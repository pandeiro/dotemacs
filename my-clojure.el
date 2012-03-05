;; Paredit in Clojure mode
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)

;; ClojureScript REPL for inferior lisp mode
(defun use-browser-repl () 
  (setq inferior-lisp-program "browser-repl"))
(add-hook 'clojurescript-mode-hook 'use-browser-repl)

;; Launch a cljs-watch shell and rename it *watch*
(defun make-cljs-watch-buffer ()
  (interactive)
  (shell "*watch*")
  (comint-send-string "*watch*" (concat "cd " desktop-dirname "&& watch " "\n")))

;; Launch a couchapp autopush shell and name it *push*
(defun make-couchapp-autopush-buffer ()
  (interactive)
  (shell "*push*")
  (comint-send-string "*push*" (concat "cd " desktop-dirname "/app" 
				       "&& couchapp autopush --update-delay 1 " "\n")))

(provide 'my-clojure)