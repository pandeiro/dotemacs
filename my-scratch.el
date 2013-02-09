;; Open straight to *scratch*, no message, org-mode
(setq initial-major-mode 'org-mode)

(setq initial-scratch-message
      ;; Startup file containing links to current projects, bookmarks, etc
      (let ((startup-file "~/.emacs.d/startup.org"))
	(if (file-exists-p startup-file)
	    (with-temp-buffer
	      (insert-file-contents startup-file)
	      (buffer-string))
	  "* First
   
")))


;; No startup screen
(setq inhibit-startup-screen 1)

(provide 'my-scratch)
