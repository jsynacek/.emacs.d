(make-face 'jsynacek-buffer-name-face)
(set-face-attribute 'jsynacek-buffer-name-face nil
		    :inherit 'mode-line-face
		    :weight 'bold)

(set-face-attribute 'mode-line nil
		    :background "#fdf6e3"
		    :box '())

;; (make-face 'jsynacek-directory-face)
;; (set-face-attribute 'jsynacek-directory-face nil
;; 		    :inherit 'mode-line-face
;; 		    :height 75
;; 		    :foreground "#666666")

(make-face 'jsynacek-major-mode-face)
(set-face-attribute 'jsynacek-major-mode-face nil
		    :inherit 'mode-line-face
		    :weight 'bold)
		    ;; :foreground "#586e75")


(make-variable-buffer-local 'jsynacek-old-mode-line-format)
(setq-default jsynacek-old-mode-line-format nil)

(setq jsynacek-mode-line-separator " ● ")

(defun jsynacek-remote-buffer ()
  (if (and (stringp default-directory)
	   (file-remote-p default-directory))
      "⬌"
    " "))

(defun jsynacek-modified-buffer ()
  (if (buffer-modified-p (current-buffer))
      "✎"
    " "))

(defun jsynacek-read-only-buffer ()
  (if buffer-read-only
      ""
    " "))

(defvar jsynacek-git-mode-file
  '(:eval (let* ((file (buffer-file-name))
		 (backend (symbol-name (vc-backend file))))
	    (if (stringp file)
		(if (vc-registered file)
		    (concat " "
			    (substring vc-mode
				       (+ 2 (length backend)))
			    " "))))))
(put 'jsynacek-git-mode-file 'risky-local-variable t)

(defun jsynacek-mode-line-padding ()
  (let ((padding 80))
    (format "%%%d " padding)))

(defun jsynacek-mode-line-toggle ()
  (interactive)
  (if (and (boundp 'jsynacek-old-mode-line-format)
	   jsynacek-old-mode-line-format)
      (progn
	(setq mode-line-format jsynacek-old-mode-line-format)
	(setq jsynacek-old-mode-line-format nil))
    (progn
      (setq jsynacek-old-mode-line-format mode-line-format)
      (setq mode-line-format '(" "
			       (:eval (jsynacek-remote-buffer))
			       " "
			       (vc-mode jsynacek-git-mode-file)
			       (:eval (jsynacek-read-only-buffer))
			       " "
			       ;; (:propertize (:eval (abbreviate-file-name default-directory))
			       ;; 		face jsynacek-directory-face)
			       ;; " "
			       (:propertize "%b"
			       		    face jsynacek-major-mode-face)
			       ;; mode-line-buffer-identification
			       (:eval (jsynacek-modified-buffer))
			       " "
			       (:propertize mode-name
					    face
					    jsynacek-major-mode-face)
			       "  "
			       ;; (:eval (jsynacek-mode-line-padding))
			       "%l"
			       erc-modified-channels-object))))
  (force-mode-line-update))

;; debug only
(global-set-key (kbd "<f12>") 'jsynacek-mode-line-toggle)
