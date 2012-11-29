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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector [solarized-bg red green yellow blue magenta cyan solarized-fg])
 '(custom-safe-themes (quote ("d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" default)))
 '(fci-rule-color "#073642"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
