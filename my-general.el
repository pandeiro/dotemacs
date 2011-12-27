;; Simple Sessions: desktop file + saveplace
(require 'saveplace)
(setq-default save-place t)
(desktop-save-mode 1)

;; Line numbers
(global-linum-mode 1)

;; Tchau, Menubar
(menu-bar-mode 0)

;; Parenthesis matching
(show-paren-mode t)

;; No backups and autosave files, please
(setq make-backup-files nil)
(auto-save-mode -1)

;; C-u - C-x o is too much
(defun back-window ()
  (interactive)
  (other-window -1))

;; SQLite3 for inferior sqlite process
(setq sql-sqlite-program "sqlite3")

;; JavaScript 2-space indent
(setq js-indent-level 2)

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

(provide 'my-general)
