;; My key-bindings, all in one place

(global-set-key (kbd "C-x p") 'back-window)

(global-set-key (kbd "C-c k") 'kill-start-of-line)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c j") 'nrepl-jack-in)
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-c r") 'make-lein-run-buffer)
(global-set-key (kbd "C-c b") 'make-lein-cljsbuild-buffer)

;; As suggested by org-mode itself
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


; http://inversethought.com/hg/hgwebdir.cgi/dotemacs/file/5f95b63347ec/dotemacs.el
; NOTE: These shortcuts work in xterm but the up/down ones do NOT work in gnome-terminal
(global-set-key (kbd "C-S-<down>") 'windmove-down)
(global-set-key (kbd "C-S-<up>") 'windmove-up)
(global-set-key (kbd "C-S-<left>") 'windmove-left)
(global-set-key (kbd "C-S-<right>") 'windmove-right)

(provide 'my-keys)

