;;; init --- Settings file

;;; Commentary:

;;; code:

(xterm-mouse-mode 1)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default python-indent-offset 2)
(setq indent-line-function 'insert-tab)

;; Emacs background color.
;; (set-face-background 'default "#001734")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "JB" :slant normal :weight regular :height 120 :width normal))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))


;;======================================================

(prefer-coding-system 'utf-8)

(setq auto-save-default nil)
(setq backup-inhibited t)
(setq-default truncate-lines t)
(global-hl-line-mode)
(global-display-line-numbers-mode)
;; (setq-default cursor-type 'box)
(setq-default cursor-type '(bar . 2))

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq-default fill-column 80)
(global-display-fill-column-indicator-mode)

(setq-default indicate-empty-lines t)
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
;; (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
;; (set-fringe-bitmap-face 'tilde 'font-lock-function-name-face)


;; Find files FFAP custom function
(defun find-file-in-project-at-point (&optional open-another-window)
  "Find file whose name is guessed around point.
If OPEN-ANOTHER-WINDOW is not nil, the file will be opened in new window."
  (interactive "P")
  (let* ((filename (or (ffap-file-at-point)
                       (thing-at-point 'filename)
                       (thing-at-point 'symbol)
                       (read-string "No file name at point. Please provide file name:")))
         ;; filename could be a path
         )
    (cond
     (filename
      ;; strip prefix "../../" or "././" from file name
      (setq filename (replace-regexp-in-string "^\\(\\.\\.*/\\)*" "" filename))
      ;; Set ffip-match-path-instead-of-filename globally
      (setq ffip-match-path-instead-of-filename t)
      (ffip-find-files filename open-another-window))
     (t
      (message "No file name is provided.")))))

(global-set-key (kbd "C-c p") 'find-file-in-project-at-point)


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Straight package management
(load-file "~/.emacs.d/plugins-settings/straight.el")

(use-package flycheck-aspell
  :ensure t
  :config
  ;;  (setq ispell-dictionary "your_default_dictionary")
  (setq ispell-program-name "aspell")
  (setq ispell-silently-savep t))

;; (use-package kaolin-themes
;;   :ensure t
;;   :config (load-theme 'kaolin-dark t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package highlight-defined
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

(use-package avy
  :ensure t)

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :config
  (require 'tree-sitter-langs)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode. tree-sitter-typescript)))

(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (
         (tsx-ts-mode typescript-mode js-mode js2-mode emacs-lisp-mode web-mode). indent-bars-mode)
  :config
  (setq indent-bars-depth-update-delay 0.2)
  (setq indent-bars-no-descend-string t)
  (setq indent-bars-no-descend-list t)
  (setq indent-bars-no-stipple-char-font-weight nil)
  (defun my/tab-bar-mode-setup ()
    (setq tab-bar-colors
          '(("active". "#73C6B6")
            ("inactive". "#212F3D")))
    (tab-bar-mode 1))
  
  (add-hook 'tab-bar-mode-hook #'my/tab-bar-mode-setup))



(use-package smartparens
  :ensure t
  :config (smartparens-global-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package helm-xref
  :ensure t
  :config
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini))

(use-package eglot
  :ensure t)

(use-package company
  :ensure t
  :after eglot
  :hook (eglot-managed-mode . company-mode)
  :config
  (setq company-show-quick-access t)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  ;; (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package company-quickhelp
  :ensure t
  :after company
  :init
  :config
  (setq company-quickhelp-idle-delay 0.1)
  (company-quickhelp-mode 1))


(use-package posframe
  :ensure t)

;; (use-package browse-at-remote
;;   :ensure t)

(add-to-list 'load-path "~/.emacs.d/plugins/posframe")
(require 'posframe)

(use-package pos-tip
  :ensure t)

(use-package try
  :ensure t)

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  (set-face-attribute 'flycheck-posframe-info-face nil :inherit 'info)
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error)
  (setq flycheck-posframe-warning-prefix "\u26a0 ")
  (setq flycheck-posframe-border-width 10))

(use-package selectrum
  :ensure t
  :config
  (selectrum-mode 1))

(use-package projectile
  :ensure t
  :config (projectile-mode 1)
  (setq projectile-sort-order 'recentf)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/Documents/projects/works/" "~/Documents/projects/personal/" ("~/works" . 1))))

(use-package org-projectile
  :ensure t)

(use-package org-project-capture
  :bind (("C-c n p" . org-project-capture-project-todo-completing-read))
  :ensure t
  :config
  (progn
    (setq org-project-capture-backend
          (make-instance 'org-project-capture-projectile-backend))  ; Replace with your backend of choice
    (setq org-project-capture-projects-file "~/.emacs.d/org/projects.org")
    (org-project-capture-single-file)))

(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-mode)
    ;; (which-key-setup-side-window-right-bottom)
    (which-key-setup-minibuffer)))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1)
  :bind (
         ("C-z" . 'undo)
         ("C-S-z" . 'redo)))

(defun my-undo-tree-config ()
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))
(add-hook 'after-init-hook 'my-undo-tree-config)

(use-package neotree
  :ensure t
  :bind (("C-\\" . 'neotree-toggle)))

(use-package multiple-cursors
  :ensure t
  :bind (
	       ("C-S-<down>" . 'mc/mark-next-like-this)
	       ("C-S-<up>" . 'mc/mark-previous-like-this)
	       ("C-+" . 'mc/mark-all-like-this)
	       ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)
	       ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)))

(use-package switch-window
  :ensure t
  :bind (
         ("C-M-z" . 'switch-window)))

(use-package fix-word
  :ensure t
  :bind (
         ("M-u" . 'fix-word-upcase)
         ("M-l" . 'fix-word-downcase)
         ("M-c" . 'fix-word-capitalize)))

(use-package beacon
  :ensure t
  :config (beacon-mode -1))

(use-package minimap
  :ensure t
  :config (minimap-mode 0))

(use-package hydra
  :ensure t)

(use-package file-info
  :straight (:host github :repo "artawower/file-info.el")
  :bind (("C-c d" . 'file-info-show))
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
                                                 :internal-border-width 2
                                                 :internal-border-color "#61AFEF"
                                                 :left-fringe 16
                                                 :right-fringe 16)))

(use-package dap-mode
  :ensure t
  :init (add-hook 'dap-stopped-hook
		              (lambda (arg) (call-interactively #'dap-hydra))))

(use-package google-translate
  :ensure t
  :bind (
	       ("\C-ct" . 'google-translate-at-point)
	       ("\C-cT" . 'google-translate-query-translate))
  :config (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(use-package rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'foo-mode-hook #'rainbow-delimiters-mode))

(use-package wakatime-mode
  :ensure t
  :config (global-wakatime-mode))

(use-package screenshot
  :straight (:type git :host github :repo "tecosaur/screenshot")
  :config 
  (setq screenshot-line-numbers-p nil)
  
  (setq screenshot-min-width 80)
  (setq screenshot-max-width 90)
  (setq screenshot-truncate-lines-p nil)
  ;;
  (setq screenshot-text-only-p nil)
  ;;
  (setq screenshot-font-family "JetBrains Mono")
  (setq screenshot-font-size 10)
  ;;
  (setq screenshot-border-width 16)
  (setq screenshot-radius 10)
  ;;
  (setq screenshot-shadow-intensity 90)
  (setq screenshot-shadow-radius 8)
  (setq screenshot-shadow-offset-horizontal 1)
  (setq screenshot-shadow-offset-vertical 4)
  :hook((screenshot-buffer-creation-hook . g-screenshot-on-buffer-creation)))

(defun g-screenshot-on-buffer-creation ()
  (setq display-fill-column-indicator-column nil)
  (setq line-spacing nil))

;; Dired custom
(load-file "~/.emacs.d/plugins-settings/dired-config.el")

;; GitBlame Config
(load-file "~/.emacs.d/plugins-settings/quelpa-packages.el")

;; Dashboard Config
(load-file "~/.emacs.d/plugins-settings/dashboard-settigs.el")

;; IDEs settings
(load-file "~/.emacs.d/plugins-settings/ides-settings.el")

;; HideShow
(load-file "~/.emacs.d/plugins-settings/hideshowvis.el")

;; Spaceline Buffer Bar
;; (load-file "~/.emacs.d/plugins-settings/spaceline-bar.el")

;; Doom modeline
(load-file "~/.emacs.d/plugins-settings/doom-modeline-manual.el")

;; Centaur Tabs
(load-file "~/.emacs.d/plugins-settings/centaur-tabs.el")

;; Curtom function
(load-file "~/.emacs.d/plugins-settings/custom-function.el")

;; Font ligatures
(load-file "~/.emacs.d/plugins-settings/font-ligatures.el")

;; Nofify
(load-file "~/.emacs.d/plugins/notify.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d77d6ba33442dd3121b44e20af28f1fae8eeda413b2c3d3b9f1315fbda021992" default))
 '(global-display-line-numbers-mode t)
 '(indent-bars-no-stipple-char-font-weight 'thin)
 '(indent-bars-prefer-character t)
 '(minimap-minimum-width 20)
 '(minimap-window-location 'right)
 '(neo-autorefresh nil)
 '(neo-theme 'nerd)
 '(neo-window-width 30)
 '(tool-bar-mode nil))
