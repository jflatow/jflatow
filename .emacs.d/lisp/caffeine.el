(defun caffeine-cleared-credential (&optional username email)
  (or username (setq username "jared"))
  (or email (setq email (concat username "@caffeine.tv")))
  (base64-encode-string
   (json-encode
    (list
     (cons :expiry (truncate (float-time)))
     (cons :session
           (list
            (cons :id (string-trim (shell-command-to-string "uuidgen")))))
     (cons :owner
           (list
            (cons :account
                  (list
                   (cons :user
                         (list
                          (cons :id (shell-command-to-string "uuidgen"))
                          (cons :username username)
                          (cons :email email)
                          (cons :created_at (truncate (float-time)))))))))
     (cons :permissions (list "is_admin")))) t))
;; (base64-decode-string (caffeine-cleared-credential "joe"))

(defun get-json-response (&optional buffer)
  (or buffer (setq buffer (get-buffer restclient-same-buffer-response-name)))
  (if buffer
      (with-current-buffer buffer
        (json-read-from-string (buffer-string)))))

(defun command (a &optional b)
  (or b (setq b default))
  (command-body a b))

;; (get-json-response (get-buffer "*HTTP Response*"))
;; (eval-buffer)
