(setq user-mail-address "jflatow@gmail.com"
      user-full-name "Jared Flatow")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq gnus-secondary-select-methods
      '((nntp "news.gwene.org")
        (nnhackernews "")))

(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-summary-line-format "%U%R%z%B %(%[%4L: %-23,23f%]%) %s @ %&user-date;\n")
(setq gnus-sum-thread-tree-root "")
(setq gnus-sum-thread-tree-vertical "│")
(setq gnus-sum-thread-tree-indent " ")
(setq gnus-sum-thread-tree-leaf-with-other "├")
(setq gnus-sum-thread-tree-single-leaf "└")
