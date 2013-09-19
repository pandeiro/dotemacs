;; Customizations via GUI don't pollute this file
(setq custom-file "~/.emacs.d/my-custom.el")
(load custom-file)

;; Package management with Marmalade repo
(add-to-list 'load-path "~/.emacs.d/")
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" .  "http://melpa.milkbox.net/packages/"))
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
		      elnode
		      lua-mode
		      js-comint
		      nodejs-repl
		      go-mode
		      markdown-mode
		      rainbow-mode
		      solarized-theme
		      gist
		      skewer-mode
		      s))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'my-general)
(require 'my-ui)
(require 'my-webdev)
(require 'my-util)
(require 'my-keys)
(require 'my-clojure)
(require 'my-org)
(require 'my-go)
(require 'my-erc)
(require 'my-scratch)
(require 'my-c)
