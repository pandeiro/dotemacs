;; Simple Sessions: desktop file + saveplace
(require 'saveplace)
(setq-default save-place t)
;(desktop-save-mode 1)

;; Paredit in Clojure and Lisp modes
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)

;; No backups and autosave files, please
(setq make-backup-files nil)
(auto-save-mode -1)

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

;; Setup ESS mode - commented while not doing stats work
;(require 'ess-site)

(provide 'my-general)
