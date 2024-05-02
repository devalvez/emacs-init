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
  (setq dashboard-vertically-center-content t)
  ;;(setq dashboard-show-shortcuts nil)
  (setq dashboard-banner-logo-title "Welcome to Emacs DEVALVEZ")
  (setq dashboard-startup-banner "~/.emacs.d/astronaut.png")

  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package

  (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                     :height 1.5
                                                     :v-adjust 0
                                                     :face 'font-lock-keyword-face))

  (setq dashboard-items '((recents   . 10)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)
                          (registers . 5)))
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

           (,(all-the-icons-faicon "rebel" :height 1.1 :v-adjust 0.0)
            "Devalvez Blog"
            "https://devalvez.com"
            (lambda (&rest _) (browse-url "https://devalvez.com")))

           )))
  
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer))
  (setq dashboard-navigation-cycle t)
  (setq dashboard-heading-shorcut-format " [%s]")
  (setq dashboard-item-shortcuts '((recents   . "r")
                                   (bookmarks . "m")
                                   (projects . "p")
                                   (agenda    . "a")
                                   (registers . "e")))
  (setq dashboard-item-names '(("Recent Files:"               . "Arquivos recentes:")
                               ("Agenda for today:"           . "Agenda de hoje:")
                               ("Agenda for the coming week:" . "Agenda:")))
  (setq dashboard-icon-type 'all-the-icons) )
(setq dashboard-set-footer nil)

