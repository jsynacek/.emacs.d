;; -*- lexical-binding: t; -*-
(require 'open-color)

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

;; Minibufer
(face-spec-set 'minibuffer-prompt
               `((t :foreground nil)))

;; Font lock
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

;; Mode-line
(face-spec-set 'mode-line
               `((t :foreground ,(oc-color 'gray9)
                    :background ,(oc-color 'gray4)
                    :box (:style flat-button))))
(face-spec-set 'mode-line-inactive
               `((t :foreground ,(oc-color 'gray9)
                    :background ,(oc-color 'gray2)
                    :box (:style flat-button))))
(face-spec-set 'mode-line-highlight
               `((t :foreground ,(oc-color 'gray9)
                    :background ,(oc-color 'gray3)
                    :box (:style flat-button))))

;; Isearch
(face-spec-set 'isearch
               `((t :foreground nil
                    :background ,(oc-color 'yellow3))))
(face-spec-set 'lazy-highlight
               `((t :foreground nil
                    :background ,(oc-color 'yellow1))))

;; Paren matching
(face-spec-set 'show-paren-match
               `((t :foreground nil
                    :background ,(oc-color 'green2))))
(face-spec-set 'show-paren-mismatch
               `((t :foreground nil
                    :background ,(oc-color 'red2))))

;; Dired
(face-spec-set 'dired-header
               '((t :weight bold)))

;; Comint and shell
(face-spec-set 'comint-highlight-prompt
               '((t :weight normal)))
(face-spec-set 'comint-highlight-input
               '((t :weight normal)))
(face-spec-set 'sh-quoted-exec
               `((t :foreground ,(oc-color 'violet7))))
(face-spec-set 'sh-heredoc
               ;; `((t :foreground ,(oc-color 'yellow8))))
               `((t :foreground nil
                    :inherit font-lock-string-face)))

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
               `((t :foreground ,(oc-color 'gray9)
                    :slant italic
                    :font "Liberation Mono 11")))

;; Magit
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

;; Org
(face-spec-set 'org-level-1
               '((t :inherit default)))
(face-spec-set 'org-level-2
               '((t :inherit default)))
(face-spec-set 'org-level-3
               '((t :inherit default)))
(face-spec-set 'org-headline-done
               `((t :foreground nil
                    inherit default)))
(face-spec-set 'org-date
               `((t :foreground ,(oc-color 'violet7)
                    :font "Liberation Mono 11")))
(face-spec-set 'org-todo
               `((t :foreground ,(oc-color 'red7))))
(setq org-todo-keyword-faces
      `(("DOING" . ,(oc-color 'cyan6))
        ("WAITING" . ,(oc-color 'orange7))
        ("DONE" . ,(oc-color 'green8))))

