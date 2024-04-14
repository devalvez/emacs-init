(use-package spaceline
  :ensure t)

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config
  (spaceline-all-the-icons-theme)
  ;; (all-the-icons-install-fonts)
  (setq spaceline-all-the-icons-auto-update-p nil)
  (setq spaceline-all-the-icons-show-editor nil)
  (setq spaceline-all-the-icons-buffer-size-limit 1000000)
  ;; Enable anzu searching
  (spaceline-all-the-icons--setup-anzu)
  ;; Enable package update indicator
  (spaceline-all-the-icons--setup-package-updates)
  ;; Enable # of commits ahead of upstream in git
  (spaceline-all-the-icons--setup-git-ahead)
  ;; Enable Paradox mode line
  (spaceline-all-the-icons--setup-paradox)
  ;; Enable Neotree mode line
  (spaceline-all-the-icons--setup-neotree)
  ;; Change Icons
  (setq spaceline-all-the-icons-icon-set-modified 'circle)
  (setq spaceline-all-the-icons-icon-set-bookmark 'bookmark)
  (setq spaceline-all-the-icons-icon-set-dedicated 'pin)
  (setq spaceline-all-the-icons-icon-set-window-numbering 'circle)
  (setq spaceline-all-the-icons-icon-set-window-eyebrowse-workspace 'circle)
  (setq spaceline-all-the-icons-icon-set-multiple-cursors 'caret)
  (setq spaceline-all-the-icons-icon-set-git-stats 'git-stats)
  (setq spaceline-all-the-icons-icon-set-flycheck-slim 'git-stats)
  (setq spaceline-all-the-icons-icon-set-sun-time 'sun/moon)
  (setq spaceline-all-the-icons-separator-type 'slant))
