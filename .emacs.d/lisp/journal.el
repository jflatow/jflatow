;;; journal.el --- Interact with a timedb journal

;; Copyright (C) 2021 by Jared Flatow

;; Emacs Lisp Archive Entry
;; Filename: journal.el
;; Author: Jared Flatow <jared@convex.io>
;; Version: 1.0
;; Keywords: extensions, journal, tools

;;; Commentary:

;; This file allows you to quickly open the present journal file.

;;; Code:

(defvar journal-root "~/Dropbox/Writing/Journal"
  "Location of the journal root directory.")

(defun journal-open ()
  "Open today's journal file."
  (interactive)
  (find-file (concat (file-name-as-directory journal-root) (format-time-string "%Y/%m/%d"))))

(provide 'journal)

;;; journal.el ends here
