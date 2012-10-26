;; JavaScript 2-space indent
(setq js-indent-level 2)

;; Turn on rainbow-mode automatically with css-mode
(defun in-rainbows () (rainbow-mode 1))
(add-hook 'css-mode-hook 'in-rainbows)
