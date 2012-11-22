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
	  ([control shift right] . [(meta shift +)])
	  ([control shift up] .    [(meta p)])
	  ([control shift down] .  [(meta n)]))))
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; from https://github.com/lambdatronic/org-babel-example

;; Add org-babel support
(when (locate-file "ob" load-path load-suffixes)
  (require 'ob)
  (require 'ob-tangle)
  (require 'ob-clojure)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure    . t))))

;; Pull in the htmlize library for pretty source code in HTML output
(require 'htmlize)

;; Fontify source code in org-latex export to PDF
(require 'org-latex)
(setq org-export-latex-listings 'minted)
(add-to-list 'org-export-latex-packages-alist '("" "minted"))
(setq org-export-latex-custom-lang-environments
      '(
        (emacs-lisp "common-lispcode")
        ))
(setq org-export-latex-minted-options
      '(("fontsize" "\\scriptsize")
        ("linenos" "false")))
(setq org-latex-to-pdf-process '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                                 "bibtex %b"
                                 "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                                 "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))

;; Some adjustments to work with org-babel
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
