;;; open-color.el --- Open color scheme  -*- lexical-binding: t; -*-

;; Copywhatever (W) 2022 Jan Synáček

;; Author: Jan Synáček <jan.synacek@gmail.com>
;; Version: 1.0
;; URL:

;;; Commentary

;; This package provides the Open color scheme.
;; See https://yeun.github.io/open-color.

(defconst oc-gray-alist '((gray0 . "#f8f9fa")
                          (gray1 . "#f1f3f5")
                          (gray2 . "#e9ecef")
                          (gray3 . "#dee2e6")
                          (gray4 . "#ced4da")
                          (gray5 . "#adb5bd")
                          (gray6 . "#868e96")
                          (gray7 . "#495057")
                          (gray8 . "#343a40")
                          (gray9 . "#212529")))

(defconst oc-red-alist '((red0 . "#fff5f5")
                         (red1 . "#ffe3e3")
                         (red2 . "#ffc9c9")
                         (red3 . "#ffa8a8")
                         (red4 . "#ff8787")
                         (red5 . "#ff6b6b")
                         (red6 . "#fa5252")
                         (red7 . "#f03e3e")
                         (red8 . "#e03131")
                         (red9 . "#c92a2a")))

(defconst oc-pink-alist '((pink0 . "#fff0f6")
                          (pink1 . "#ffdeeb")
                          (pink2 . "#fcc2d7")
                          (pink3 . "#faa2c1")
                          (pink4 . "#f783ac")
                          (pink5 . "#f06595")
                          (pink6 . "#e64980")
                          (pink7 . "#d6336c")
                          (pink8 . "#c2255c")
                          (pink9 . "#a61e4d")))

(defconst oc-grape-alist '((grape0 . "#f8f0fc")
                           (grape1 . "#f3d9fa")
                           (grape2 . "#eebefa")
                           (grape3 . "#e599f7")
                           (grape4 . "#da77f2")
                           (grape5 . "#cc5de8")
                           (grape6 . "#be4bdb")
                           (grape7 . "#ae3ec9")
                           (grape8 . "#9c36b5")
                           (grape9 . "#862e9c")))

(defconst oc-violet-alist '((violet0 . "#f3f0ff")
                            (violet1 . "#e5dbff")
                            (violet2 . "#d0bfff")
                            (violet3 . "#b197fc")
                            (violet4 . "#9775fa")
                            (violet5 . "#845ef7")
                            (violet6 . "#7950f2")
                            (violet7 . "#7048e8")
                            (violet8 . "#6741d9")
                            (violet9 . "#5f3dc4")))

(defconst oc-indigo-alist '((indigo0 . "#edf2ff")
                            (indigo1 . "#dbe4ff")
                            (indigo2 . "#bac8ff")
                            (indigo3 . "#91a7ff")
                            (indigo4 . "#748ffc")
                            (indigo5 . "#5c7cfa")
                            (indigo6 . "#4c6ef5")
                            (indigo7 . "#4263eb")
                            (indigo8 . "#3b5bdb")
                            (indigo9 . "#364fc7")))

(defconst oc-blue-alist '((blue0 . "#e7f5ff")
                          (blue1 . "#d0ebff")
                          (blue2 . "#a5d8ff")
                          (blue3 . "#74c0fc")
                          (blue4 . "#4dabf7")
                          (blue5 . "#339af0")
                          (blue6 . "#228be6")
                          (blue7 . "#1c7ed6")
                          (blue8 . "#1971c2")
                          (blue9 . "#1864ab")))

(defconst oc-cyan-alist '((cyan0 . "#e3fafc")
                          (cyan1 . "#c5f6fa")
                          (cyan2 . "#99e9f2")
                          (cyan3 . "#66d9e8")
                          (cyan4 . "#3bc9db")
                          (cyan5 . "#22b8cf")
                          (cyan6 . "#15aabf")
                          (cyan7 . "#1098ad")
                          (cyan8 . "#0c8599")
                          (cyan9 . "#0b7285")))

(defconst oc-teal-alist '((teal0 . "#e6fcf5")
                          (teal1 . "#c3fae8")
                          (teal2 . "#96f2d7")
                          (teal3 . "#63e6be")
                          (teal4 . "#38d9a9")
                          (teal5 . "#20c997")
                          (teal6 . "#12b886")
                          (teal7 . "#0ca678")
                          (teal8 . "#099268")
                          (teal9 . "#087f5b")))

(defconst oc-green-alist '((green0 . "#ebfbee")
                           (green1 . "#d3f9d8")
                           (green2 . "#b2f2bb")
                           (green3 . "#8ce99a")
                           (green4 . "#69db7c")
                           (green5 . "#51cf66")
                           (green6 . "#40c057")
                           (green7 . "#37b24d")
                           (green8 . "#2f9e44")
                           (green9 . "#2b8a3e")))

(defconst oc-lime-alist '((lime0 . "#f4fce3")
                          (lime1 . "#e9fac8")
                          (lime2 . "#d8f5a2")
                          (lime3 . "#c0eb75")
                          (lime4 . "#a9e34b")
                          (lime5 . "#94d82d")
                          (lime6 . "#82c91e")
                          (lime7 . "#74b816")
                          (lime8 . "#66a80f")
                          (lime9 . "#5c940d")))

(defconst oc-yellow-alist '((yellow0 . "#fff9db")
                            (yellow1 . "#fff3bf")
                            (yellow2 . "#ffec99")
                            (yellow3 . "#ffe066")
                            (yellow4 . "#ffd43b")
                            (yellow5 . "#fcc419")
                            (yellow6 . "#fab005")
                            (yellow7 . "#f59f00")
                            (yellow8 . "#f08c00")
                            (yellow9 . "#e67700")))

(defconst oc-orange-alist '((orange0 . "#fff4e6")
                            (orange1 . "#ffe8cc")
                            (orange2 . "#ffd8a8")
                            (orange3 . "#ffc078")
                            (orange4 . "#ffa94d")
                            (orange5 . "#ff922b")
                            (orange6 . "#fd7e14")
                            (orange7 . "#f76707")
                            (orange8 . "#e8590c")
                            (orange9 . "#d9480f")))

(defun oc-color (color)
  "Return the #RRGGBB defined for COLOR by the Open Color definition.
COLOR is a symbol named one of `gray', `red', `pink', `grape', `violet',
`indigo', `blue', `cyan', `teal', `green', `lime', `yellow' or `orange'
followed by a digit.

Example:

  (oc-color 'blue7) => \"#1c7ed6\"
"
  (let* ((key-str (symbol-name color))
         (alist (symbol-value
                 (intern
                  (concat "oc-" (substring key-str 0 -1) "-alist")))))
    (alist-get color alist)))

(when (native-comp-available-p)
  (native-compile 'oc-color))

(provide 'open-color)
