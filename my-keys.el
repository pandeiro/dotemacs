;; My key-bindings, all in one place

; jump to end of word, then beginning of next, then end, etc
(global-set-key (kbd "C-c f") 'forward-same-syntax)

(global-set-key (kbd "C-x p") 'back-window)

; (global-set-key (kbd "C-c k") 'kill-start-of-line)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c j") 'nrepl-jack-in) ; TODO move to Clojure mode hook!
(global-set-key (kbd "C-c n") 'next-logical-line)
(global-set-key (kbd "C-c p") 'previous-logical-line)
(global-set-key (kbd "C-c r") 'clojure-open-nrepl)
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-c C-d") 'delete-file-of-current-buffer) ;; FIXME sgml mode coopted
(global-set-key (kbd "C-c x") 'scratch)

;; Open Emacs config dir
(global-set-key (kbd "C-c e") 'open-my-config-dir)

;; Open recent files menu
(global-set-key (kbd "C-x C-f") 'my-find-file) ;; defined in my-general.el

;; Load and save desktops
(global-set-key (kbd "C-c d") 'desktop-read)
(global-set-key (kbd "C-c D") 'save-desktop-to-default-location)

; (global-set-key (kbd "C-c r") 'make-lein-run-buffer)
; (global-set-key (kbd "C-c b") 'make-lein-cljsbuild-buffer)

;; As suggested by org-mode itself
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Start an HTTP server (with Elnode) w/ a particular docroot and port
(global-set-key (kbd "C-c h") 'elnode-make-webserver)

; http://inversethought.com/hg/hgwebdir.cgi/dotemacs/file/5f95b63347ec/dotemacs.el
; NOTE: These shortcuts work in xterm but the up/down ones do NOT work in gnome-terminal
(global-set-key (kbd "C-S-<down>") 'windmove-down)
(global-set-key (kbd "C-S-<up>") 'windmove-up)
(global-set-key (kbd "C-S-<left>") 'windmove-left)
(global-set-key (kbd "C-S-<right>") 'windmove-right)

; Dired stuff
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "I") 'dired-insert-subdir-stay-put)))

; Quick keyboard macros with F1 (http://www.emacswiki.org/emacs/KeyboardMacros)
(global-set-key '[(f1)]          'call-last-kbd-macro)
(global-set-key '[(shift f1)]    'toggle-kbd-macro-recording-on)
(provide 'my-keys)

