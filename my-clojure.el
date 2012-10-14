(require 'clojure-mode)

;; Better indentation for Compojure macros and Clutch fns
(define-clojure-indent
  (defroutes 'defun)
  (-> 'defun)
  (->> 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2)
  (save-view 1)
  (view-server-fns 1))

;; Enable auto-completion with nREPL
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;; ClojureScript REPL for inferior lisp mode
(setq cljs-inferior-lisp-cmd "lein trampoline cljsbuild repl-listen")

(defun use-browser-repl () 
  (setq inferior-lisp-program cljs-inferior-lisp-cmd))
(add-hook 'clojurescript-mode-hook 'use-browser-repl)

;; Launch `lein run`
(defun make-lein-run-buffer ()
  (interactive)
  (async-shell-command "lein trampoline run") ; save memory
  (with-current-buffer "*Async Shell Command*"
    (rename-buffer "*lein run*")))

;; Launch ClojureScript compilation watcher
(defun make-lein-cljsbuild-buffer ()
  (interactive)
  (async-shell-command "lein cljsbuild auto")
  (with-current-buffer "*Async Shell Command*"
    (rename-buffer "*cljsbuild*")))

;; Visit all relevant buffers in a Clojure web project
(defun open-clojure-web-project ()
  (visit-files-by-types '("html" "css" "clj" "cljs")))

;; Display Clojure web buffers
;; (defun show-web-buffers ()
;;   (delete-other-windows)
;;   (display-buffer "project.clj")
;;   (split-window-vertically)
;;   (display-buffer "*lein run*")
;;   (split-window-vertically)
;;   (display-buffer "routes.clj")
;;   (split-window-vertically)
;;   (display-buffer "templates.clj")
;;   (split-window-vertically)
;;   (display-buffer "main.cljs")
;;   (split-window-vertically)
;;   (display-buffer "style.css")
;;   (balance-windows))

;; (defun show-web-buffers ()
;;   (delete-other-windows)
;;   (let ((project (display-buffer "project.clj")))
;;     (delete-other-windows project)
;;     (split-window-horizontally)
;;     (display-buffer "main.cljs")
;;     (split-window-vertically)
;;     (display-buffer "app.clj")
;;     (split-window-vertically)
;;     (display-buffer "templates.clj")
;;     (split-window-vertically)
;;     (display-buffer "routes.clj")
;;     (split-window-vertically)
;;     (display-buffer "*lein run*")
;;     (split-window-vertically)
;;     (display-buffer "*cljsbuild*")
;;     (split-window-vertically)
;;     (display-buffer "*inferior-lisp*"))
;;   (balance-windows))

(defun only-one-window-p ()
  (= 1 (length (window-list))))

(defun hide-inf-lisp ()
  (delete-window (get-buffer-window "*inferior-lisp*")))

(defun show-web-buffers ()
  (if (only-one-window-p) nil (delete-other-windows))
  (display-buffer "project.clj")
  (hide-inf-lisp)
  (let ((right (split-window-right)))
    (with-selected-window (split-window-below)
      (display-buffer "*lein run*")
      (with-selected-window (split-window-below)
	(display-buffer "*cljsbuild*")
	(with-selected-window (split-window-below)
	  (display-buffer "*inferior-lisp*"))))
    (with-selected-window right
      (display-buffer "routes.clj")
      (with-selected-window (split-window-below)
	(display-buffer "templates.clj")
	(with-selected-window (split-window-below)
	  (display-buffer "main.cljs")))))
  (balance-windows))

;; Open everything, start server, watch cljs files, start cljs repl
(defun clojure-web-jack-in ()
  (open-clojure-web-project)
  (make-lein-run-buffer)
  (make-lein-cljsbuild-buffer)
  (inferior-lisp cljs-inferior-lisp-cmd)
  (show-web-buffers))

;; Treat cljx like clj
(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))

(provide 'my-clojure)
