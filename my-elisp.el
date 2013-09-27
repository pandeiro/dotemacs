(defun turn-on-eldoc-in-elisp ()
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-in-elisp)

(provide 'my-elisp)
