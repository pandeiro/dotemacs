;; Package management with Marmalade repo
(add-to-list 'load-path "~/.emacs.d/")
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'my-general)
(require 'my-keys)
(require 'my-clojure)

