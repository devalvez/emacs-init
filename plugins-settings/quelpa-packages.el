(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

(use-package blamer
  :quelpa (blamer :fetcher github :repo "artawower/blamer.el")
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 98
                   :italic t)))
  :config
  (global-blamer-mode 1))
