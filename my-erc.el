(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#clojure" "##javascript" "#archlinux"
	 "#bash" "#git" "#html5" "#couchdb")))

(provide 'my-erc)