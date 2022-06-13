;; -*- lexical-binding: t; -*-

(defun cmd/other-window-or-frame (arg)
  "Switch to other window. If called with universal argument, swith to other frame."
  (interactive "P")
  (if (equal arg '(4))
      (other-frame 1)
    (other-window 1)))

;; See 'org-show-notification' in org-clock.el for more details on how org timers are implemented.
(defvar *jsynacek-timer* nil)

(defvar *jsynacek-timer-time* (* 45 60)
  "Timer time in seconds.")

(defun cmd/start-timer ()
  (interactive)
  (if *jsynacek-timer*
      (progn
        ;; The remaining time computation shamelessly stolen from timer-list.el.
        (let* ((time (list (aref *jsynacek-timer* 1)
                           (aref *jsynacek-timer* 2)
                           (aref *jsynacek-timer* 3)))
               (time-str
                (format-seconds "%dd %hh %mm %z%ss"
                                (float-time
                                 (if (aref *jsynacek-timer* 7)
                                     time
                                   (time-subtract time nil))))))
          (message "Timer already set. (%s remaining)" time-str)))
    (progn
      (setq *jsynacek-timer*
            (run-with-timer
             *jsynacek-timer-time* nil
             (lambda ()
               (setq *jsynacek-timer* nil)
               (call-process "notify-send" nil nil nil
                             "-u" "critical"
                             "-i" "emacs"
                             "Stop working now!"
                             "Take a break."))))
      (pcase (decode-time *jsynacek-timer-time*)
        (`(,secs ,mins . ,_) (message "Timer set to %02d:%02d." mins secs))))))

(defun cmd/stop-timer ()
  (interactive)
  (when *jsynacek-timer*
    (cancel-timer *jsynacek-timer*)
    (setq *jsynacek-timer* nil)
    (message "Timer cancelled.")))

(provide 'jsynacek-misc)
