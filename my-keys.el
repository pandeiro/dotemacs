;; My key-bindings, all in one place

(global-set-key (kbd "C-x p") 'back-window)

(global-set-key (kbd "C-c k") 'kill-start-of-line)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c j") 'clojure-jack-in)
(global-set-key (kbd "C-c l") 'goto-line)

; http://inversethought.com/hg/hgwebdir.cgi/dotemacs/file/5f95b63347ec/dotemacs.el
(global-set-key (kbd "C-S-j") 'windmove-down)
(global-set-key (kbd "C-S-k") 'windmove-up)
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-l") 'windmove-right)

(provide 'my-keys)