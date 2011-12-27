;; My key-bindings, all in one place

(global-set-key (kbd "C-x p") 'back-window)

(global-set-key (kbd "C-c k") 'kill-start-of-line)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c j") 'clojure-jack-in)
(global-set-key (kbd "C-c l") 'goto-line)

(provide 'my-keys)