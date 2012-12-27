(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#org-mode" "#clojure" "##javascript" "#archlinux"
	 "##linux" "#bash" "#git" "#html5" "##twitter-bootstrap" "#couchdb")))

;; http://emacs-fu.blogspot.com/2009/06/erc-emacs-irc-client.html
;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; open IRC buffers in background by default
(setq erc-join-buffer 'bury)

(provide 'my-erc)
