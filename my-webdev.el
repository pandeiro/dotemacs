;; JavaScript 2-space indent
(setq js-indent-level 2)

;; Turn on rainbow-mode automatically with css-mode
(defun in-rainbows () (rainbow-mode 1))
(add-hook 'css-mode-hook 'in-rainbows)

;;
;; Elnode customizations
;;
;; Don't start webserver automatically
(setq elnode-do-init nil)

;; todo: customize elnode-webserver-index-file-template & elnode-webserver-index-page-template

(provide 'my-webdev)
