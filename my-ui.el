;; Line numbers (deactivated, wastes space)
;(global-linum-mode 1)

;; Tchau, Menubar, Toolbar, big Fringe
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-fringe-mode '(0 . 0))

;; Custom scrollbars now defined in ~/.config/gtk-3.0/gtk.css
;(scroll-bar-mode -1)

;; Don't show startup screen
(setq inhibit-startup-screen 1)

;; Parenthesis matching
(show-paren-mode t)

;; Show column position
(setq column-number-mode t)

;; Default font
(set-face-attribute 'default nil :height 100 :family "Inconsolata")

;; Autofill where appropriate
(defun ui-setup-auto-fill ()
  (auto-fill-mode 1)
  (setq fill-column 80))
(eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'ui-setup-auto-fill))
(eval-after-load 'org-mode
  (add-hook 'org-mode-hook 'ui-setup-auto-fill))
(add-hook 'text-mode-hook 'ui-setup-auto-fill)

;; Blinking bar cursor, invisible in non-selected windows
(setq cursor-in-non-selected-windows nil)
(setq-default cursor-type 'bar)
(blink-cursor-mode t)

;; Highlight line
(global-hl-line-mode t)

;; Use Solarized light and dark with one custom tweak: no fringes
(defun apply-solarized-light ()
  (interactive)
  (scroll-bar-mode)
  (load-theme 'solarized-light 1)
  (custom-theme-set-faces
   'solarized-light
   `(fringe ((((class color) (min-colors 89))
	      (:foreground nil :background nil))))))

(defun apply-solarized-dark ()
  (interactive)
  (scroll-bar-mode -1)
  (load-theme 'solarized-dark 1)
  (custom-theme-set-faces
   'solarized-dark
   `(fringe ((((class color) (min-colors 89))
	      (:foreground nil :background nil))))))

(defun apply-zenburn ()
  (interactive)
  (scroll-bar-mode -1)
  (load-theme 'zenburn 1))

;; Load Solarized light by default
(apply-solarized-light)

;; Turn on ansi-color-for-comint-mode for shell output buffers
(ansi-color-for-comint-mode-on)

;; Load Workgroups and use w/ prefix C-c w
(require 'workgroups)
(setq wg-prefix-key (kbd "C-c w"))
(setq wg-switch-on-load nil)
(setq wg-morph-on nil)
;(setq wg-morph-hsteps 8)
;(setq wg-morph-vsteps 8)
(workgroups-mode 1)
(wg-load "~/.config/emacs/workgroups.el")

;; Use popwin
(require 'popwin)
(popwin-mode 1)

;; Stolen from emacs-live
(setq popwin:special-display-config
      '(("*Help*"  :height 30)
        ("*Completions*" :noselect t)
        ("*Messages*" :noselect t :height 30)
        ("*Apropos*" :noselect t :height 30)
        ("*compilation*" :noselect t)
        ("*Backtrace*" :height 30)
        ("*Messages*" :height 30)
        ("*Occur*" :noselect t)
        ("*Ido Completions*" :noselect t :height 30)
        ("*magit-commit*" :noselect t :height 40 :width 80 :stick t)
        ("*magit-diff*" :noselect t :height 40 :width 80)
        ("*magit-edit-log*" :noselect t :height 15 :width 80)
        ("\\*ansi-term\\*.*" :regexp t :height 30)
;        ("*shell*" :height 30)
;        ("*eshell*" :height 30)
        (".*overtone.log" :regexp t :height 30)
        ("*gists*" :height 30)
        ("*sldb.*":regexp t :height 30)
        ("*nrepl-error*" :height 30 :stick t)
        ("*nrepl-doc*" :height 30 :stick t)
        ("*nrepl-src*" :height 30 :stick t)
        ("*nrepl-result*" :height 30 :stick t)
        ("*nrepl-macroexpansion*" :height 30 :stick t)
        ("*Kill Ring*" :height 30)
        ("*Compile-Log*" :height 30 :stick t)
        ("*git-gutter:diff*" :height 30 :stick t)))

;; Use winner-mode to be able to undo/redo window layouts
(winner-mode 1)

;; Could people please start working on the features that are needed?
(defun line-spacing-toggle ()
  "Toggle line-spacing between single and AMOUNT (defaults to 12)"
  (interactive)
  (setq line-spacing (if line-spacing nil 1.33)))

(defun big-fringe-toggle ()
  "Wordprocessory margins"
  (interactive)
  (set-fringe-mode (if (equal '(0 . 0) fringe-mode) '(30 . 20) '(0 . 0))))

;; Setup auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(provide 'my-ui)
