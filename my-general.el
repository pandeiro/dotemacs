;; Simple Sessions: desktop file + saveplace
;;
;; Don't open desktop files by default, but setup environment
;; for using one single global desktop file in ~/.config/emacs
(setq desktop-path (list "~/.config/emacs"))
(setq desktop-dirname "~/.config/emacs")
(setq desktop-base-file-name "desktop.el")

(defun desktop-load (directory)
  "Like desktop-read, but prompts for a directory"
  (interactive (list (ido-read-directory-name "Desktop directory: ")))
  (desktop-read directory))

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;; Use recentf mode to keep track of what I open
(recentf-mode 1)
(setq recentf-exclude (list ".ido.last"))
(setq recentf-max-saved-items 50)

(defun my-find-file (&optional prefix)
  (interactive "P")
  (if prefix
      (recentf-open-files)
    (call-interactively 'ido-find-file)))

;; No backups and autosave files, please
(setq make-backup-files nil)
(auto-save-mode -1)

;; Find and open things with ido
(require 'ido)
(ido-mode t)

;; Use ido for everything if this is turned on
;; http://emacswiki.org/emacs/InteractivelyDoThings#toc14
(defvar ido-enable-replace-completing-read nil
  "If t, use ido-completing-read instead of completing-read if possible.

  Set it to nil using let in around-advice for functions where the
  original completing-read is required.  For example, if a function
  foo absolutely must use the original completing-read, define some
  advice like this:

  (defadvice foo (around original-completing-read-only activate)
    (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (and (boundp 'ido-cur-list)
               ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                                     allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Emacs adjustments made here, bound to C-c e
(defun open-my-config-dir ()
  (interactive)
  (dired "~/.emacs.d"))

;; Paredit in Clojure and Lisp modes
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)

;; Replace yes/no questions with y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; No Shift selection
(setq-default shift-select-mode nil)

;; C-u - C-x o is too much
(defun back-window ()
  (interactive)
  (other-window -1))

;; SQLite3 for inferior sqlite process
(setq sql-sqlite-program "sqlite3")

;; Bash (shell-mode) indentation tweak
(defun alter-case-indent ()
  "Sets up indentation of case statements in `shell-script-mode'.  Automatically added to
`sh-mode-hook'"
  (setq sh-indent-for-case-label 0
        sh-indent-for-case-alt 2)
  (add-hook 'sh-mode-hook 'alter-case-indent))

;; Kill to start of line
(defun kill-start-of-line ()
  "kill from point to start of line"
  (kill-line 0))

;; helps with colors when using tmux
(defun terminal-init-screen ()
  "Terminal initialization function for screen/tmux"
  ;; Use the xterm color initialization code.
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))

;; Auto-update buffers after a git branch change
;; http://stackoverflow.com/questions/1480572/how-to-have-emacs-auto-refresh-all-buffers-when-files-have-changed-on-disk
(global-auto-revert-mode t)

;; Use 'a' to open files/dirs in current Dired buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Use chromium to open links
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

;; enable lower-casing regions
(put 'downcase-region 'disabled nil)

;; Setup ESS mode - commented while not doing stats work
;(require 'ess-site)

;; To facilitate easier keyboard macro usage (from http://www.emacswiki.org/emacs/KeyboardMacros)
(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

;; Better unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Can't count on "American typists" anymore
(setq sentence-end-double-space nil)

;; Replace selection when typing, like GUI editors do
(delete-selection-mode)

(provide 'my-general)
