(use-package projectile
  :ensure t
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-banner-logo-title "Welcome to Emacs Devalvez")
  (setq dashboard-startup-banner "~/.emacs.d/thumb.png")
  (setq dashboard-footer-icon (all-the-icons-octicon "flame"
                                                     :height 0.3
                                                     :v-adjust 2
                                                     :face 'font-lock-keyword-face))

  (setq dashboard-items '((recents  . 5)
                          (projects . 5)))
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (defun dashboard-insert-custom (list-size)
    (insert "The End"))
  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
  (add-to-list 'dashboard-items '(custom) t)
  (setq dashboard-set-navigator t)
  
  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-faicon "github" :height 1.1 :v-adjust 0.0)
            "Github"
            "github.com/devalvez"
            (lambda (&rest _) (browse-url "https://github.com/devalvez")))

           (,(all-the-icons-faicon "gitlab" :height 1.1 :v-adjust 0.0)
            "GitLab"
            "gitlab.com/WesleyAntonioAlves"
            (lambda (&rest _) (browse-url "https://gitlab.com/WesleyAntonioAlves")))

           (,(all-the-icons-faicon "hand-spock-o" :height 1.1 :v-adjust 0.0)
            "Devalvez Blog"
            "devalvez.online"
            (lambda (&rest _) (browse-url "https://devalvez.online")))

           ))))
