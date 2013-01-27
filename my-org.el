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
	  ("png" . "feh %s")
	  ("xcf" . "gimp %s"))))

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

;; *scratch* initial msg advertises this binding
(add-hook 'org-mode-hook 'bind-src-fontify)
(defun bind-src-fontify ()
  (define-key org-mode-map (kbd "C-c C-h") (lambda ()
					     (interactive)
					     (progn (org-src-fontify-buffer)
						    (message "Colors!!!11!")))))

;; native colors
(setq org-src-fontify-natively t)

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

;; Tangle on by default
(setq org-babel-default-header-args
      (cons '(:tangle . "yes")
	    (assq-delete-all :tangle org-babel-default-header-args)))

;; Evaluate the following other languages besides emacs-lisp in source blocks:
;; - JavaScript
(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (clojure . t)
   (emacs-lisp . t)
   (python . t)
   (sh . t)
   (C . t)))

;; Eval w/o confirming
(setq org-confirm-babel-evaluate nil)

;; Clojure-specific
;;
;; org-babel Clojure support is built to work with Swank/SLIME, which is no longer
;; being developed, having been replaced by nREPL. This code switches out the former
;; in favor of the latter. Taken from https://github.com/lambdatronic/org-babel-example

;; Patch ob-clojure to work with nrepl
(declare-function nrepl-send-string-sync "ext:nrepl" (code &optional ns))

(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with Babel."
  (require 'nrepl)
  (with-temp-buffer
    (insert (org-babel-expand-body:clojure body params))
    ((lambda (result)
       (let ((result-params (cdr (assoc :result-params params))))
         (if (or (member "scalar" result-params)
                 (member "verbatim" result-params))
             result
           (condition-case nil (org-babel-script-escape result)
             (error result)))))
     (plist-get (nrepl-send-string-sync
                 (buffer-substring-no-properties (point-min) (point-max))
                 (cdr (assoc :package params)))
                :value))))

(provide 'my-org)
