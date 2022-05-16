;; -*- lexical-binding: t; -*-

(require 'term)

(defun cmd/toggle-terminal ()
  "Toggle between the buffer named *terminal* and the previous
buffer. If *terminal* does not exist, it is created.
"
  (interactive)
  (if (and (eql major-mode 'term-mode)
           (string= (buffer-name) "*terminal*"))
      (switch-to-buffer nil)
    (term "/bin/bash")))

(defun cmd/term (&optional buffer-name)
  "Run a new terminal. If called with universal argument, ask for the new terminal's
buffer name and rename the new terminal buffer to it. Otherwise, simply execute
`(term \"/usr/bin/bash\")'.
"
  (interactive (list
                (when current-prefix-arg
                  (read-string "Terminal buffer name: " nil nil "*terminal*"))))
  (if buffer-name
      (progn
        (let ((old-term-buffer (get-buffer "*terminal*")))
          (and old-term-buffer
               (with-current-buffer old-term-buffer
                 (rename-buffer "__terminal__" t)))
          (term "/usr/bin/bash")
          (rename-buffer buffer-name t)
          (and old-term-buffer
               (with-current-buffer (get-buffer "__terminal__")
                 (rename-buffer "*terminal*")))))
    (term "/usr/bin/bash")))

(defun cmd/make-terminal-frame (&optional buffer-name)
  (interactive (list
                (when current-prefix-arg
                  (read-string "Terminal buffer name: " nil nil "*terminal*"))))
  (select-frame-set-input-focus
   (let ((width (alist-get 'width jsynacek-ui-alist))
         (internal-border-width (alist-get 'internal-border-width jsynacek-ui-alist)))
     ;;default-directory
     (make-frame `((left . ,(/ (+ (* width (frame-char-width))
                                  (* 2 internal-border-width))
                               2))
                   ,(assoc 'width jsynacek-ui-alist)
                   ,(assoc 'height jsynacek-ui-alist)))))
  (cmd/term buffer-name))

(provide 'jsynacek-term)
