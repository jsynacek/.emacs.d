;; -*- lexical-binding: t; -*-
(load (concat user-emacs-directory "open-color.el"))

;;(setq jsynacek-font "Liberation Mono")

(face-spec-set 'default
               `((t :foreground ,(oc-color 'gray9)
                    ;; :background ,(oc-color 'yellow9))))
                    :background "white")))
(face-spec-set 'fringe
               '((t :background "white")))
(face-spec-set 'region
               `((t :background ,(oc-color 'gray2))))
(face-spec-set 'highlight
               `((t :background ,(oc-color 'green1)))) ;; TODO gray2 to match region maybe?
(face-spec-set 'shadow
               `((t :foreground ,(oc-color 'gray6))))
(face-spec-set 'success
               `((t :foreground ,(oc-color 'green8))))
(face-spec-set 'warning
               `((t :foreground ,(oc-color 'orange7)))) ;; TODO yellow9 maybe?
(face-spec-set 'error
               `((t :foreground ,(oc-color 'red7))))
;; (face-spec-set 'minibuffer-prompt
;;                `((t :foreground ,(oc-color 'blue7)
;;                     :weight bold)))
(face-spec-set 'minibuffer-prompt
               `((t :foreground nil)))

(face-spec-set 'font-lock-comment-face
               `((t :foreground ,(oc-color 'gray6))))
(face-spec-set 'font-lock-doc-face
               '((t :inherit font-lock-comment-face)))
(face-spec-set 'font-lock-string-face
               `((t :foreground ,(oc-color 'red9))))
(face-spec-set 'font-lock-keyword-face
               `((t :foreground ,(oc-color 'blue7))))
(face-spec-set 'font-lock-builtin-face
               '((t :foreground nil)))
(face-spec-set 'font-lock-type-face
               '((t :foreground nil)))
(face-spec-set 'font-lock-constant-face
               '((t :foreground nil)))
(face-spec-set 'font-lock-variable-name-face
               '((t :foreground nil)))
(face-spec-set 'font-lock-function-name-face
               '((t :foreground nil
                    :inherit font-lock-keyword-face)))
(face-spec-set 'mode-line
               '((t :foreground "#212529"    ; gray9
                    :background "#ced4da"    ; gray4
                    :box (:style flat-button))))
(face-spec-set 'mode-line-inactive
               '((t :foreground "#212529"    ; gray9
                    :background "#e9ecef"    ; gray2
                    :box (:style flat-button))))
(face-spec-set 'mode-line-highlight
               '((t :foreground "#212529"    ; gray9
                    :background "#dee2e6"    ; gray3
                    :box (:style flat-button))))

(face-spec-set 'show-paren-match
               `((t :foreground nil
                    :background ,(oc-color 'green2))))
(face-spec-set 'show-paren-mismatch
               `((t :foreground nil
                    :background ,(oc-color 'red2))))

(face-spec-set 'dired-header
               '((t :weight bold)))

;; Comint
(face-spec-set 'comint-highlight-prompt
               '((t :weight normal)))
(face-spec-set 'comint-highlight-input
               '((t :weight normal)))

;; Header and Info
(face-spec-set 'header-line
               '((t :foreground nil
                    :background nil
                    :inherit mode-line-inactive)))
(face-spec-set 'header-line-highlight
               '((t :foreground nil
                    :inherit highlight)))
(face-spec-set 'info-node
               '((t :foreground nil
                    :weight bold
                    :slant normal)))
(face-spec-set 'info-header-xref
               `((t :foreground ,(oc-color 'blue8))))
(face-spec-set 'info-xref
               `((t :foreground ,(oc-color 'blue8))))
(face-spec-set 'info-xref-visited
               `((t :foreground ,(oc-color 'violet8))))
(face-spec-set 'Info-quoted
               `((t :foreground ,(oc-color 'gray7)
                    :font "Liberation Mono 10")))

;; TODO magit
;; (face-spec-set 'magit-diff-removed-highlight
;;                `((t :foreground ,(oc-color 'red9)
;;                     :background ,(oc-color 'red1))))
;; (face-spec-set 'magit-diff-added-highlight
;;                `((t :foreground ,(oc-color 'green9)
;;                     :background ,(oc-color 'green1))))
;; (face-spec-set 'magit-diff-context-highlight
;;                `((t :foreground ,(oc-color 'gray6)
;;                     :background ,(oc-color 'gray0))))
(face-spec-set 'magit-branch-local
               `((t :foreground ,(oc-color 'blue7))))
(face-spec-set 'magit-branch-remote
               `((t :foreground ,(oc-color 'violet7))))
(face-spec-set 'magit-section-heading
               `((t :foreground ,(oc-color 'yellow9))))
(face-spec-set 'magit-tag
               '((t :foreground nil)))
