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

;; Custom man link type (taken from Info)
(require 'org)

(org-add-link-type "man" 'org-man-open)
(add-hook 'org-store-link-functions 'org-man-store-link)

(defcustom org-man-command 'man
  "The Emacs command to be used to display a man page."
  :group 'org-link
  :type '(choice (const man) (const woman)))

(defun org-man-open (path)
  "Visit the manpage on PATH.
   PATH should be a topic that can be thrown at the man command."
  (funcall org-man-command path))

(defun org-man-store-link ()
  "Store a link to a manpage."
  (when (memq major-mode '(Man-mode woman-mode))
    ;; This is a man page, we do make this link
    (let* ((page (org-man-get-page-name))
	   (link (concat "man:" page))
	   (description (format "Manpage for %s" page)))
      (org-store-link-props
       :type "man"
       :link link
       :description description))))

(defun org-man-get-page-name ()
  "Extract the page name from the buffer name."
  ;; This works for both `Man-mode' and `woman-mode'.
  (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
      (match-string 1 (buffer-name))
    (error "Cannot create link to this man page")))

;; Working with source code

;; Evaluate the following other languages besides emacs-lisp in source blocks:
;; - JavaScript
(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)))


(provide 'my-org)
