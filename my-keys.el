;; My key-bindings, all in one place

(global-set-key (kbd "C-x p") 'back-window)

(global-set-key (kbd "C-c k") 'kill-start-of-line)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c j") 'clojure-jack-in)
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-c r") 'make-lein-run-buffer)

; http://inversethought.com/hg/hgwebdir.cgi/dotemacs/file/5f95b63347ec/dotemacs.el
; NOTE: These shortcuts work in xterm but the up/down ones do NOT work in gnome-terminal
(global-set-key (kbd "C-S-<down>") 'windmove-down)
(global-set-key (kbd "C-S-<up>") 'windmove-up)
(global-set-key (kbd "C-S-<left>") 'windmove-left)
(global-set-key (kbd "C-S-<right>") 'windmove-right)

(provide 'my-keys)

