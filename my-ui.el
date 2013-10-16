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

;; Default font
(set-face-attribute 'default nil :height 100 :family "Inconsolata")

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

(provide 'my-ui)
