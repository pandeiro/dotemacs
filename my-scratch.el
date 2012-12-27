;; Open straight to *scratch*, no message, org-mode
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message
"You are in *scratch* w/ org-mode:

C-x C-e   eval lisp              anywhere
C-c C-c   eval anything          in src blocks
C-c C-h   highlight code syntax  here
C-c '     edit src               in other buffer

C-c n     new project            prompt

Have fun!

* ")

;; No startup screen
(setq inhibit-startup-screen 1)

(provide 'my-scratch)
