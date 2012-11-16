;; Package management with Marmalade repo
(add-to-list 'load-path "~/.emacs.d/")
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; More stealing from technomancy
(setq marmalade-server "http://marmalade-repo.org/")
(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
		      clojurescript-mode
		      nrepl
		      auto-complete
		      ac-nrepl
		      refheap
		      magit
		      paredit
		      workgroups
		      lua-mode
		      go-mode
		      markdown-mode
		      htmlize
		      rainbow-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'my-general)
(require 'my-webdev)
(require 'my-util)
(require 'my-keys)
(require 'my-clojure)
(require 'my-org)
(require 'my-go)
(require 'my-erc)
