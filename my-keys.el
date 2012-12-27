;; My key-bindings, all in one place
;;
;; Note: global key-bindings only. Mode-specific bindings are in relevant file

(global-set-key (kbd "C-x p") 'back-window)

; (global-set-key (kbd "C-c k") 'kill-start-of-line)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c j") 'nrepl-jack-in) ; TODO move to Clojure mode hook!
(global-set-key (kbd "C-c l") 'goto-line)

;; Open Emacs config dir
(global-set-key (kbd "C-c e") 'open-my-config-dir)

;; Start a new project
(global-set-key (kbd "C-c n") 'start-new-project)

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

(provide 'my-keys)

