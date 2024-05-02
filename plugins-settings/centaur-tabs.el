(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-style "slant") ; alternate | bar | box | chamfre | rounded | slant | wave | zigzag
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
                                        ; To make icons plain (same color as tabs’ text):
  (setq centaur-tabs-plain-icons t)
  ;; To gray out icons for the unselected tabs:
  (setq centaur-tabs-gray-out-icons 'buffer)
                                        ; To display a colored bar at the left of the selected tab
  (setq centaur-tabs-set-bar 'over) ; left | over | under
  ;; Note: If you're not using Spacmeacs, in order for the underline to display
  ;; correctly you must add the following line:
  ;; (setq x-underline-at-descent-line t)

                                        ; To disable the close button
                                        ; (setq centaur-tabs-set-close-button nil)
                                        ; To change the displayed string for the close button
  (setq centaur-tabs-close-button "×")
                                        ;To display a marker indicating that a buffer has been modified (atom-style)
  (setq centaur-tabs-set-modified-marker t)
                                        ; To change the displayed string for the modified-marker
  (setq centaur-tabs-modified-marker "⚬")
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

;; INFO: more settings in https://github.com/ema2159/centaur-tabs
