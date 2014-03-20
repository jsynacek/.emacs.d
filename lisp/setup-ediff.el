(require 'ediff)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (if (> (frame-width) 140)
                                      'split-window-horizontally
                                    'split-window-vertically))

(provide 'setup-ediff)
