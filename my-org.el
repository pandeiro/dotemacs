;; Optimize indentation for outline-style documents
(setq org-startup-indented t)

;; Set agenda
(setq org-agenda-files '("~/org"))

;; Autohide leading stars
(setq org-hide-leading-stars t)

;; Specify `llpp` to open pdfs and feh to open jpgs and pngs
(add-hook 'org-mode-hook 'set-org-mode-app-defaults)
(defun set-org-mode-app-defaults ()
  (setq org-file-apps
	'(("pdf" . "llpp %s")
	  ("jpg" . "feh %s")
	  ("png" . "feh %s"))))

;; Override C-S-arrows b/c they are used for window movement
(setq org-replace-disputed-keys t)
(add-hook 'org-mode-hook 'set-org-mode-shift-replacements)
(defun set-org-mode-shift-replacements ()
  (setq org-disputed-keys
	'(([control shift left] .  [(meta shift -)])
	  ([control shift right] . [(meta shift +)]))))

(add-hook 'org-mode-hook 'unbind-org-mode-control-shift-up-and-down)
(defun unbind-org-mode-control-shift-up-and-down ()
  (define-key org-mode-map (kbd "<C-S-up>") nil)
  (define-key org-mode-map (kbd "<C-S-down>") nil))

;; Working with source code



(provide 'my-org)
