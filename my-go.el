(require 'go-mode-load)

; Set Go tab indent to 4 spaces
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode t)
	    (setq tab-width 4)))

(provide 'my-go)
