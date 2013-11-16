;; JavaScript 2-space indent
(setq js-indent-level 2)
(setq js2-basic-offset 2) ; js2-mode equivalent
;(setq js2-bounce-indent-p t)

;; Turn on rainbow-mode automatically with css-mode
(defun in-rainbows () (rainbow-mode 1))
(add-hook 'css-mode-hook 'in-rainbows)

;; Open markdown in markdown-mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; todo: customize elnode-webserver-index-file-template & elnode-webserver-index-page-template

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


;; Use web-mode
;(require 'web-mode)
;(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;; Interactive browser development
(setq httpd-root "~/rt/browser")

(defun browse-default-app ()
  (interactive)
  (shell-command "webapp &"))

(defun start-httpd ()
  "Begins the simple HTTP server and initiates skewer for live
JavaScript (etc) evaluation in the browser."
  (interactive)
  (httpd-start)
  (with-temp-buffer (skewer-mode)))

;; dirty hack for quick HTML "eval" from html-mode buffers
;; via skewer
(defun stick-in-div-wrapper ()
  "Just stick the HTML contents of any buffer in div#wrapper (minus
the linebreaks, which break skewer)"
  (interactive)
  (skewer-eval-synchronously
   (concat "document.querySelector('div#wrapper').innerHTML = '"
	   (s-replace "
"
		      ""
		      (buffer-substring-no-properties (point-min) (point-max))) "'")))


(eval-after-load "sgml-mode"
  '(progn
     (define-key html-mode-map (kbd "C-x C-e") 'browse-default-app)
     (define-key html-mode-map (kbd "C-M-x") 'stick-in-div-wrapper)
     (define-key html-mode-map (kbd "C-c C-d") 'ng-snip-show-docs-at-point)))

;; Use subword-mode in js2-mode
(add-hook 'js2-mode-hook 'subword-mode)

;; Use emmet in html-mode
(add-hook 'html-mode-hook 'emmet-mode)

;; fix yas in js2
(defun js2-tab-properly ()
  (interactive)
  (let ((yas-fallback-behavior 'return-nil))
    (unless (yas-expand)
      (indent-for-tab-command)
      (if (looking-back "^\s*")
          (back-to-indentation)))))

(eval-after-load 'js2-mode
  '(progn
     (define-key js2-mode-map (kbd "TAB") 'js2-tab-properly)))

(defun webdev-save-and-refresh ()
  (interactive)
  (save-buffer (current-buffer))
  (skewer-eval "window.location.href = window.location.href"))

(provide 'my-webdev)
