;; Line numbers (deactivated, wastes space)
;(global-linum-mode 1)

;; Tchau, Menubar, Toolbar, big Fringe
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-fringe-mode '(1 . 1))

;; Custom scrollbars now defined in ~/.config/gtk-3.0/gtk.css
; (scroll-bar-mode -1)

;; Parenthesis matching
(show-paren-mode t)

;; Default font
(set-face-attribute 'default nil :height 90 :family "Inconsolata")

;; Use Solarized with one custom tweak: no fringes
(load-theme 'solarized-light 1)
(custom-theme-set-faces
 'solarized-light
 `(fringe ((((class color) (min-colors 89))
	    (:foreground nil :background nil)))))

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
