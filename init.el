;; Customizations via GUI don't pollute this file
(setq custom-file "~/.emacs.d/my-custom.el")
(load custom-file)

;; Package management with Marmalade repo
(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" .  "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
		      nrepl
		      auto-complete
		      ac-nrepl
		      refheap
		      magit
		      paredit
		      workgroups
		      elnode
		      lua-mode
		      nodejs-repl
		      go-mode
		      markdown-mode
		      pandoc-mode
		      rainbow-mode
		      solarized-theme
		      gist
		      skewer-mode
		      s
		      dash
		      dash-functional
		      expand-region
		      scratch
		      popwin
		      deferred
		      request
		      livid-mode))

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
(require 'my-c)
(require 'my-elisp)
(require 'my-yas)

