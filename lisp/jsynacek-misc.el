;; -*- lexical-binding: t; -*-

(defun cmd/other-window-or-frame (arg)
  "Switch to other window. If called with universal argument, swith to other frame."
  (interactive "P")
  (if (equal arg '(4))
      (other-frame 1)
    (other-window 1)))

(provide 'jsynacek-misc)
