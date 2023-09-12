;;; jflatow.el --- Personal modes, hooks, and settings

;; Copyright (C) 2021 by Jared Flatow

;; Emacs Lisp Archive Entry
;; Filename: jflatow.el
;; Author: Jared Flatow <jared@convex.io>
;; Version: 1.0
;; Keywords: extensions, preferences

;;; Commentary:

;; This file is used in my personal `.emacs' file.

;;; Code:


;;; Local dependencies

;; Easy journaling
(require 'journal)


;;; Global minor modes
(define-globalized-minor-mode global-goto-address-mode goto-address-mode
  (lambda () (goto-address-mode t)) :group 'mine)

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The indicator for the mode line.
  :lighter " Sensitive"
  ;; The minor mode bindings.
  :after-hook
  (if (symbol-value sensitive-mode)
      (progn
	;; disable backups
	(set (make-local-variable 'backup-inhibited) t)
	;; disable auto-save
	(if auto-save-default
	    (auto-save-mode -1)))
    ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;resort to default auto save setting
    (if auto-save-default
	(auto-save-mode 1))))

(defun jflatow-minors ()
  "Set my minor modes."
  (interactive)

  ;; Save my history
  (savehist-mode t)

  ;; Highlight region and overwrite it when I type
  (transient-mark-mode t)
  (delete-selection-mode nil)
  (show-paren-mode t)

  ;; Tab bar history
  (tab-bar-history-mode t)

  ;; Winners record window configuration changes
  (winner-mode t)

  ;; Links should always be default
  ;;  `C-c RET' opens
  ;;   which you can discover via `C-h .'
  (global-goto-address-mode t)

  ;; Don't forget to mark gpg files as sensitive
  (add-to-list 'auto-mode-alist '("\\.gpg$" . sensitive-mode)))
(jflatow-minors)


;;; Random preferences for builtins

(defun jflatow-vars ()
  "Set my preference vars."
  (interactive)

  ;; No splash
  (setq inhibit-splash-screen t)

  ;; Always show that ugly trailing whitespace
  (setq-default show-trailing-whitespace t)

  ;; ABD
  (setq-default debug-on-error t)

  ;; No tabs
  (setq-default indent-tabs-mode nil)

  ;; Show column numbers
  (setq-default column-number-mode t)

  ;; Don't forget how to `widen'
  (put 'narrow-to-region 'disabled nil)

  ;; Only ever confirm with a single key
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Lines is lines
  (setq truncate-partial-width-windows t)
  (setq mode-require-final-newline nil)

  ;; Always show those ugly carriage returns
  (setq inhibit-eol-conversion t)

  ;; Save bookmarks as soon as they are created
  (setq bookmark-save-flag 1)

  ;; Preferred indentation for switch statements in cc-mode
  (c-set-offset 'case-label '+)

  ;; Tab bar preferences
  (setq tab-bar-close-button-show 'selected
        tab-bar-select-tab-modifiers '(control) ;; BUG C-<#> broken on mac
        tab-bar-tab-hints nil)

  ;; Clarify tabs a little
  (set-face-attribute 'tab-bar-tab nil ;; all tabs
                      :background "#" :foreground "black" :box nil)
  (set-face-attribute 'tab-bar-tab-inactive nil ;; inactive tabs
                      :background "gray" :foreground "black" :box nil)

  ;; Filter internet relay chat noise
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))

  ;; Encryption and auth sources
  (setq auth-source-debug t)
  (setq auth-sources
        '((:source "~/.authinfo.gpg")))
  (setq epg-pinentry-mode 'loopback))
(jflatow-vars)


;;; Shortcut keystrokes

;; Nice idea for quickly going back faster than `C-x b RET':
;;  http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun jflatow-keys ()
  "Set my global keys."
  (interactive)
  ;; Re-map key translation for option-arrow keys on Mac
  (define-key input-decode-map (kbd "ESC ESC O C") (kbd "M-<right>"))
  (define-key input-decode-map (kbd "ESC ESC O D") (kbd "M-<left>"))

  ;; Key chords could be interesting to experiment with
  ;;  but use some old fashioned keybinding for now (TODO)

  ;; Multi occur by buffer pattern
  (global-set-key (kbd "C-c m") 'multi-occur-in-matching-buffers)

  ;; Instant `journal-open'
  (global-set-key (kbd "C-c j") 'journal-open)

  ;; Install tab selection helpers
  (global-set-key (kbd "C-<left>") 'tab-bar-switch-to-prev-tab)
  (global-set-key (kbd "C-<right>") 'tab-bar-switch-to-next-tab)

  ;; Move the default `C-d` binding, replace it with a keymap
  (global-set-key (kbd "C-<delete>") 'delete-char)
  (define-key global-map (kbd "C-d") (make-sparse-keymap))

  ;; Install the window movement helpers
  (windmove-default-keybindings)

  ;; Convenient copying
  (global-set-key (kbd "C-c p") 'pbcopy-kill-ring-item))
(jflatow-keys)


;;; Shortcut commands

;; Auth secrets
(defun auth-secret (&rest args)
  "Retrieve the secret from the authinfo using ARGS as the search criteria."
  (let ((secret (plist-get (car (apply 'auth-source-search args)) :secret)))
    (if (functionp secret) (funcall secret) secret)))

;; Bash shell

(defun bash ()
  "Run a simple bash shell in an `ansi-term'."
  (interactive)
  (ansi-term "/bin/bash"))

;; Go

(defun go-run-buffer()
  (interactive)
  (shell-command (concat "go run " (buffer-name))))

;; Python and Python3 shells

(defun py-shell ()
  "Run a python shell using `run-python'

NB: this and `py3-shell' currently share the same buffer,
 could hack something else using `run-python-internal',
  and overriding anything and everything
   including `python-shell-calculate-command'"
  (interactive)
  (run-python "/usr/bin/env python" nil 0))

(defun py3-shell ()
  "Run a python3 shell using `run-python'

NB: shares buffer with `py-shell'"
  (interactive)
  (run-python "/usr/bin/env python3" nil 0))

;; Convenient pbcopy

(defun pbcopy-kill-ring-item (&optional arg)
  "Copy the last kill ring item to the clipboard using `pbcopy`.
With a prefix ARG, copy the nth previous kill ring item."
  (interactive "P")
  (let ((item (nth (or arg 0) kill-ring)))
    (when item
      (with-temp-buffer
        (insert item)
        (call-process-region (point-min) (point-max) "pbcopy"))
      (message "Copied to clipboard: %s" item))))


;;; Help

(defun jflatow-help ()
  "Sometimes you might need a refresher.

These docs are meant to be a self-documenting help
 this file is accessible via a persistent bookmark

Replication is good
 this is fundamental to emacs/lisp/fsf/gnu

Use `M-TAB` for `completion-at-point' in buffer
 in modes/points that support completion

Also, emacs has tabs and `tab-bar-mode'
 they are named (session) persistent window configurations
  `C-x t 2` new tab
  `C-x t o` other tab
  `C-x t 0` close tab

Don't forget about
 `M-A` to cycle `marginalia'
 `M-a`, `M-e` `forward-sentence' and `backward-sentence'
 `M-p`, `M-n`, `M-r` are generally useful for minibuffer history
 `M-w` even better than `copy-region-as-kill'
 `M-DEL` to `backward-kill-word'
 `M--` negate argument to invert command (often backwards)

Understand the logic behind key bindings
 `C-c` commands are part of the `mode-specific-map'
 `C-h b` to peruse all the bindings
 `C-h m` to discover useful commands in current mode
 `C-h w` where-is command shows the bindings for command

Use `xref'
 `M-.` to `xref-find-definitions' a symbol
 `M-,` to `xref-pop-marker-stack' and go back to where search began

Use bookmarks & registers, mark & point
 `C-x r C-h` for a refresher on bookmarks & registers
 `C-x r w <reg>` save window config
 `C-x r j <reg>` load window config
 `C-x C-x` to `exchange-point-and-mark' and visualize them

Use keyboard macros
 https://www.emacswiki.org/emacs/KeyboardMacros
  `\\[kmacro-start-macro-or-insert-counter]` `kmacro-start-macro-or-insert-counter'
  `\\[kmacro-end-or-call-macro]` `kmacro-end-or-call-macro'

For shells, `comint' is infinitely useful
 https://www.masteringemacs.org/article/comint-writing-command-interpreter
 https://www.masteringemacs.org/article/shell-comint-secrets-history-commands
  `C-c C-x` is amazing in shells for repeating seq of commands
  `C-u <N> C-c .` pick an arg
  `C-c C-l` list history
  `C-c M-r` history search backward
  `C-c C-r` beginning of last output
  `C-c C-o` flush last output
  `C-c C-s` save last output (as it is now)

Use `ielm' instead of *scratch* buffer for some stuff
 normal tab completion
  and you can see output inline

For some stuff, `eshell' is pretty awesome
 can just redirect to a buffer `ls >>> #<mybuf>`
  but not a good idea for data pipes
   as everything passes through interpreter
    otherwise use one of:
     `\\[shell-command]` `shell-command'
     `\\[shell-command-on-region]` `shell-command-on-region'
     `\\[shell]` `shell'

Remember comments, info, and docs
 `\\[comment-dwim]` `comment-dwim'
  `C-h r m comments RET m comment commands RET`
 doc tips in the manual:
  `C-h r s emacs lisp RET RET m documentation RET s tips RET RET`
   can use `checkdoc-minor-mode'

Org pays dividends, remember `org-info'
 capture todos
  `\\[org-capture]` `org-capture' with template
 add/remove agenda files on the fly with
  `\\[org-agenda-file-to-front]` `org-agenda-file-to-front'
  `\\[org-remove-file]` org-remove-file'
 jump around with
  `\\[org-cycle-agenda-files]` `org-cycle-agenda-files'
  `\\[org-switchb]` `org-switchb'

You can always go back to the `normal-mode'
 to reset the local variables and major mode of a buffer

To test out big changes you can create a fake user dir and do:
 `HOME=. emacs --debug-init`

There's a `speedbar'
 and appointments (i.e. `appt-activate')

TRAMP is amazing and it works out of the box
 https://www.gnu.org/software/tramp
  especially with selectrum, just start typing `C-x C-f /scp:`

`C-u C-x =` info about char under cursor

TODO:
 experiment with
  tramp over Docker
   https://willschenk.com/articles/2020/tramp_tricks/
    can be chained together with other methods

  start using snippets
   e.g. https://github.com/magnars/.emacs.d/tree/master/snippets
  bunch of cool ideas, like the dude with quick-calc wrapper
   https://www.reddit.com/r/emacs/comments/l51ocx/what_is_the_most_useful_part_of_your_emacs_config/

 learn his window setup:
  http://www.howardism.org/Technical/Emacs/new-window-manager.html

 notes on mail/gnus
 notes for epa for encryption/decryption (builtin, with epg)
  https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
   tl;dr: just edit .gpg files
    and add sensitive mode

 add function(s) to configure/connect erc
  https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
"
  (interactive)
  (describe-function 'jflatow-help))

(provide 'jflatow)

;;; jflatow.el ends here
