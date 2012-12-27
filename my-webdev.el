;; JavaScript 2-space indent
(setq js-indent-level 2)

;; Turn on rainbow-mode automatically with css-mode
(defun in-rainbows () (rainbow-mode 1))
(add-hook 'css-mode-hook 'in-rainbows)

;; Open markdown in markdown-mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;
;; Elnode customizations
;;
;; Don't start webserver automatically
(setq elnode-do-init nil)

;; todo: customize elnode-webserver-index-file-template & elnode-webserver-index-page-template

;; Use nodejs as inferior javascript process
(require 'js-comint)
(setq inferior-js-program-command "node")

;; Fix nodejs prompt
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GJK]" "" output)))))

(provide 'my-webdev)
