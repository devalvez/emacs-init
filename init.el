;; init.el -- Emacs initialisation -- lexical-binding: t --:
;;
;;; Commentary:
;;
;;; Code:
;; lsp-lang

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq-default truncate-lines 1)
(setq-default cursor-type 'box)
;; (setq-default cursor-type '(bar . 2))
(defvar blink-cursor-interval 0.1)
(defvar blink-cursor-blinks 25)
(global-hl-line-mode)

(global-display-line-numbers-mode 1)
;; Alternatively, to use it only in programming modes:
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Smooth Scroll
(pixel-scroll-precision-mode 1)
(setq ring-bell-function 'ignore)

(setq-default indicate-empty-lines t)
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
;; (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
;; (set-fringe-bitmap-face 'tilde 'font-lock-function-name-face)
;; (setq-default indincate-empty-lines t)

(when(>= emacs-major-version 27)
  (setq-default fill-column 80)
  (global-display-fill-column-indicator-mode))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(setq byte-compile-warnings '(cl-functions))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package nlinum
  :ensure t
  :config (nlinum-mode 1))

(use-package page-break-lines
  :ensure t
  :config (page-break-lines-mode))

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
                                                     :height 1
                                                     :v-adjust -0.05
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

(use-package try
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-show-quick-access t)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  ;; (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :ensure t))

(use-package google-translate
  :ensure t
  :bind (
	       ("\C-ct" . 'google-translate-at-point)
	       ("\C-cT" . 'google-translate-query-translate))
  :config (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

(use-package js-import
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

(use-package kaolin-themes
  :ensure t
  :config (load-theme 'kaolin-aurora t))

(use-package spaceline-all-the-icons
  :ensure t)

(use-package spaceline
  :ensure t)

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config (spaceline-all-the-icons-theme 'your-segment-symbol "Devalvez" 'etc)
  ;;(all-the-icons-install-fonts)
  (setq spaceline-all-the-icons-auto-update-p nil)
  (setq spaceline-all-the-icons-show-editor nil)
  (setq spaceline-all-the-icons-buffer-size-limit 1000000)
  (spaceline-all-the-icons-theme)
  
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

;; (use-package doom-modeline
;;   :ensure t
;;   :config (doom-modeline t)
;;   :init (doom-modeline-mode 1)
;;   ;; If non-nil, cause imenu to see `doom-modeline' declarations.
;;   ;; This is done by adjusting `lisp-imenu-generic-expression' to
;;   ;; include support for finding `doom-modeline-def-*' forms.
;;   ;; Must be set before loading doom-modeline.
;;   (setq doom-modeline-support-imenu t)
;;   ;; How tall the mode-line should be. It's only respected in GUI.
;;   ;; If the actual char height is larger, it respects the actual height.
;;   (setq doom-modeline-height 30)
;;   ;; How wide the mode-line bar should be. It's only respected in GUI.
;;   (setq doom-modeline-bar-width 4)
;;   ;; Whether to use hud instead of default bar. It's only respected in GUI.
;;   (setq doom-modeline-hud nil)
;;   ;; The limit of the window width.
;;   ;; If `window-width' is smaller than the limit, some information won't be
;;   ;; displayed. It can be an integer or a float number. `nil' means no limit."
;;   (setq doom-modeline-window-width-limit 85)
;;   ;; How to detect the project root.
;;   ;; nil means to use `default-directory'.
;;   ;; The project management packages have some issues on detecting project root.
;;   ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;;   ;; to hanle sub-projects.
;;   ;; You can specify one if you encounter the issue.
;;   (setq doom-modeline-project-detection 'auto)
;;   ;; Determines the style used by `doom-modeline-buffer-file-name'.
;;   ;;
;;   ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   ;;   auto => emacs/l/comint.el (in a project) or comint.el
;;   ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   ;;   truncate-with-project => emacs/l/comint.el
;;   ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   ;;   truncate-all => ~/P/F/e/l/comint.el
;;   ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
;;   ;;   relative-from-project => emacs/lisp/comint.el
;;   ;;   relative-to-project => lisp/comint.el
;;   ;;   file-name => comint.el
;;   ;;   buffer-name => comint.el<2> (uniquify buffer name)
;;   ;;
;;   ;; If you are experiencing the laggy issue, especially while editing remote files
;;   ;; with tramp, please try `file-name' style.
;;   ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
;;   (setq doom-modeline-buffer-file-name-style 'relative-from-project)
;;   ;; Whether display icons in the mode-line.
;;   ;; While using the server mode in GUI, should set the value explicitly.
;;   (setq doom-modeline-icon nil)
;;   ;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
;;   (setq doom-modeline-major-mode-icon nil)
  
;;   ;; Whether display the colorful icon for `major-mode'.
;;   ;; It respects `nerd-icons-color-icons'.
;;   (setq doom-modeline-major-mode-color-icon t)
;;   ;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
;;   (setq doom-modeline-buffer-state-icon t)

;;   ;; Whether display the modification icon for the buffer.
;;   ;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
;;   (setq doom-modeline-buffer-modification-icon t)


;;   ;; Whether display the time icon. It respects variable `doom-modeline-icon'.
;;   (setq doom-modeline-time-icon t)

;;   ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
;;   (setq doom-modeline-unicode-fallback nil)

;;   ;; Whether display the buffer name.
;;   (setq doom-modeline-buffer-name t)

;;   ;; Whether highlight the modified buffer name.
;;   (setq doom-modeline-highlight-modified-buffer-name t)

;;   ;; When non-nil, mode line displays column numbers zero-based.
;;   ;; See `column-number-indicator-zero-based'.
;;   (setq doom-modeline-column-zero-based t)

;;   ;; Specification of \"percentage offset\" of window through buffer.
;;   ;; See `mode-line-percent-position'.
;;   (setq doom-modeline-percent-position '(-3 "%p"))

;;   ;; Format used to display line numbers in the mode line.
;;   ;; See `mode-line-position-line-format'.
;;   (setq doom-modeline-position-line-format '("L%l"))

;;   ;; Format used to display column numbers in the mode line.
;;   ;; See `mode-line-position-column-format'.
;;   (setq doom-modeline-position-column-format '("C%c"))

;;   ;; Format used to display combined line/column numbers in the mode line. See `mode-line-position-column-line-format'.
;;   (setq doom-modeline-position-column-line-format '("%l:%c"))

;;   ;; Whether display the minor modes in the mode-line.
;;   (setq doom-modeline-minor-modes nil)

;;   ;; If non-nil, a word count will be added to the selection-info modeline segment.
;;   (setq doom-modeline-enable-word-count nil)

;;   ;; Major modes in which to display word count continuously.
;;   ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
;;   ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
;;   ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
;;   (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

;;   ;; Whether display the buffer encoding.
;;   (setq doom-modeline-buffer-encoding t)

;;   ;; Whether display the indentation information.
;;   (setq doom-modeline-indent-info nil)

;;   ;; Whether display the total line number。
;;   (setq doom-modeline-total-line-number nil)

;;   ;; If non-nil, only display one number for checker information if applicable.
;;   (setq doom-modeline-checker-simple-format t)

;;   ;; The maximum number displayed for notifications.
;;   (setq doom-modeline-number-limit 99)

;;   ;; The maximum displayed length of the branch name of version control.
;;   (setq doom-modeline-vcs-max-length 12)

;;   ;; Whether display the workspace name. Non-nil to display in the mode-line.
;;   (setq doom-modeline-workspace-name t)

;;   ;; Whether display the perspective name. Non-nil to display in the mode-line.
;;   (setq doom-modeline-persp-name t)

;;   ;; If non nil the default perspective name is displayed in the mode-line.
;;   (setq doom-modeline-display-default-persp-name nil)

;;   ;; If non nil the perspective name is displayed alongside a folder icon.
;;   (setq doom-modeline-persp-icon t)

;;   ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
;;   (setq doom-modeline-lsp t)

;;   ;; Whether display the GitHub notifications. It requires `ghub' package.
;;   (setq doom-modeline-github nil)

;;   ;; The interval of checking GitHub.
;;   (setq doom-modeline-github-interval (* 30 60))

;;   ;; Whether display the modal state.
;;   ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
;;   (setq doom-modeline-modal t)

;;   ;; Whether display the modal state icon.
;;   ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
;;   (setq doom-modeline-modal-icon t)

;;   ;; Whether display the modern icons for modals.
;;   (setq doom-modeline-modal-modern-icon t)

;;   ;; When non-nil, always show the register name when recording an evil macro.
;;   (setq doom-modeline-always-show-macro-register nil)

;;   ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
;;   ;;(setq doom-modeline-mu4e nil)
;;   ;; also enable the start of mu4e-alert
;;   ;;(mu4e-alert-enable-mode-line-display)

;;   ;; Whether display the gnus notifications.
;;   (setq doom-modeline-gnus t)

;;   ;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
;;   (setq doom-modeline-gnus-timer 2)

;;   ;; Wheter groups should be excludede when gnus automatically being updated.
;;   (setq doom-modeline-gnus-excluded-groups '("dummy.group"))

;;   ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
;;   (setq doom-modeline-irc t)

;;   ;; Function to stylize the irc buffer names.
;;   (setq doom-modeline-irc-stylize 'identity)

;;   ;; Whether display the battery status. It respects `display-battery-mode'.
;;   (setq doom-modeline-battery t)

;;   ;; Whether display the time. It respects `display-time-mode'.
;;   (setq doom-modeline-time t)

;;   ;; Whether display the misc segment on all mode lines.
;;   ;; If nil, display only if the mode line is active.
;;   (setq doom-modeline-display-misc-in-all-mode-lines t)

;;   ;; Whether display the environment version.
;;   (setq doom-modeline-env-version t)
;;   ;; Or for individual languages
;;   (setq doom-modeline-env-enable-python t)
;;   (setq doom-modeline-env-enable-ruby t)
;;   (setq doom-modeline-env-enable-perl t)
;;   (setq doom-modeline-env-enable-go t)
;;   (setq doom-modeline-env-enable-elixir t)
;;   (setq doom-modeline-env-enable-rust t)

;;   ;; Change the executables to use for the language version string
;;   (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
;;   (setq doom-modeline-env-ruby-executable "ruby")
;;   (setq doom-modeline-env-perl-executable "perl")
;;   (setq doom-modeline-env-go-executable "go")
;;   (setq doom-modeline-env-elixir-executable "iex")
;;   (setq doom-modeline-env-rust-executable "rustc")

;;   ;; What to display as the version while a new one is being loaded
;;   (setq doom-modeline-env-load-string "...")

;;   ;; By default, almost all segments are displayed only in the active window. To
;;   ;; display such segments in all windows, specify e.g.
;;   ;;(setq doom-modeline-always-visible-segments '(mu4e irc))

;;   ;; Hooks that run before/after the modeline version string is updated
;;   (setq doom-modeline-before-update-env-hook nil)
;;   (setq doom-modeline-after-update-env-hook nil))


;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-tokyo-night t)
  
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   ;; (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

(add-to-list 'load-path "~/.emacs.d/plugins/prisma-mode")


(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package highlight-defined
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

(use-package avy
  :ensure t)

(use-package smartparens
  :ensure t
  :config (smartparens-global-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind (("M-x" . 'smex)))

(use-package ido-vertical-mode
  :ensure t
  :config
  (progn
    (ido-mode 1)))
(setq ido-show-count t)

(use-package anzu
  :ensure t
  :config (anzu-mode 1))

(use-package helm
  :ensure t
  :config (helm-mode 1))7

(use-package helm-lsp
  :ensure t)

(use-package helm-xref
  :ensure t
  :config
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini))

(use-package mysql-to-org
  :ensure t)

(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode))

(add-to-list 'load-path "~/.emacs.d/plugins/company-lsp/")
(require 'company-lsp)
(push 'company-lsp company-backends)

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

(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-right-bottom)))

(use-package rainbow-mode
  :ensure t)
;;:mode "\\.css\\'")

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1)
  :bind (
         ("C-z" . 'undo)
         ("C-S-z" . 'red)))
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(use-package winum
  :ensure t
  :config (winum-mode))

(use-package neotree
  :ensure t
  :bind (("C-\\" . 'neotree-toggle)))

(use-package multiple-cursors
  :ensure t
  :bind (
	       ("C->" . 'mc/mark-next-like-this)
	       ("C-<" . 'mc/mark-previous-like-this)
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

;; (use-package beacon
;;   :ensure t
;;   :config (beacon-mode 1))

;; (use-package minimap
;;   :ensure t
;;   :config (minimap-mode 1)) 

;; (use-package highlight-indent-guides
;;    :ensure t
;;    :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  ;; enable Emmet's css abbreviation.
  (add-hook 'css-mode-hook  'emmet-mode)
  (setq emmet-expand-jsx-className? t) ;; default nil
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil))))

(use-package web-beautify
  :ensure t
  :config
  (eval-after-load 'js2-mode
    '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'js3-mode
    '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
  ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
  (eval-after-load 'js
    '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'json-mode
    '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'sgml-mode
    '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'web-mode
    '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'css-mode
    '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css)))

(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1)
  (setq yas-snippet-dirs
	      ;; personal snippets
	      '("~/.emacs.d/snippets"
	        ;; foo-mode and bar-mode snippet collection
          "~/.emacs.d/yasnippet-snippets-20210408.1234/snippets")))


(use-package hydra
  :ensure t)

(use-package dap-mode
  :ensure t
  :init (add-hook 'dap-stopped-hook
		  (lambda (arg) (call-interactively #'dap-hydra))))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.blade\\.php'" . web-mode)
         ("\\.tsx\\'" . web-mode)
	       ("\\.jsx\\'" . web-mode))
  
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        ;;
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
	      web-mode-enable-auto-indentation nil
        lsp-language-id-configuration'((web-mode . "blade.php")))
  
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode)
                             '("\\.tsx?\\'" . prettier-js-mode))))


      

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package js2-mode
  :ensure t)
(use-package js3-mode
  :ensure t)
(use-package js-import
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package auto-rename-tag
  :ensure t
  :config (auto-rename-tag-mode t))

(use-package jest
  :ensure t)

(use-package flycheck-jest
  :ensure t
  :after jest)

(use-package jest-test-mode
  :ensure t
  :after flycheck-jest)

;; (use-package lsp-mode
;;   :ensure t
;;   :custom
;;   (lsp-enable-file-watcher t)     ; Habilita o monitoramento de arquivos
;;   (lsp-file-watch-threshold 5000) ; Define o limite para o número de arquivos monitorados
;;   :config (add-hook 'lsp-mode-hook #'lsp-enable-file-watchers))

(use-package lsp-treemacs
  :ensure t
  :config (lsp-treemacs-sync-mode 1))

(use-package php-mode
  :ensure t
  :config (with-eval-after-load 'php-mode
	          (define-key php-mode-map (kbd "C-c C--") 'php-current-class)
	          (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace)))

(setq lsp-language-id-configuration
      '((web-mode . "php")))

(use-package ac-php
  :ensure t)

(use-package dotenv-mode
  :ensure t
  :config (setq lsp-language-id-configuration
      '((dotenv-mode . "dotenv"))))

(use-package markdown-mode
  :ensure t)

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package wakatime-mode
  :ensure t
  :config (global-wakatime-mode))

(load-file "~/.emacs.d/plugins/prisma-mode/prisma-mode.el")
(autoload 'prisma-mode "prisma-mode" nil t)
(setq prisma-format-on-save t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;; ----------------- SHOWVIS --------------------
;;Hideshow
(add-to-list 'load-path "~/.emacs.d/elpa/hideshowvis/")
(defun hideshowvis-enable ()
  "Will enable hideshowvis minor mode"
  (interactive)
  (hideshowvis-minor-mode 1))

;; HedeShow interaction
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'hideshowvis-hideable-marker [0 0 0 126 126 0 0 0]))

(defconst hideshowvis-version "v0.5" "Version of hideshowvis minor mode")

(defface hideshowvis-hidable-face
  '((t (:foreground "#ccc" :box t)))
  "Face to highlight foldable regions"
  :group 'hideshow)

(defcustom hideshowvis-ignore-same-line t
  "Do not display foldable regions in the fringe if the matching
  closing parenthesis is on the same line. Set this to nil if
  enabling the minor mode is slow on your machine"
  :group 'hideshow)

(defun hideshowvis-highlight-hs-regions-in-fringe (&optional start end old-text-length)
  (when hs-minor-mode
    (save-excursion
      (save-restriction
        (when (and start end)
          (narrow-to-region start end))
        (goto-char (point-min))
        (remove-overlays (point-min) (point-max) 'hideshowvis-hs t)
        (while (search-forward-regexp hs-block-start-regexp nil t)
          (let* ((ovl (make-overlay (match-beginning 0) (match-end 0)))
                 (marker-string "*hideshowvis*")
                 (doit
                  (if hideshowvis-ignore-same-line
                      (let (begin-line)
                        (setq begin-line
                              (save-excursion
                                (goto-char (match-beginning 0))
                                (line-number-at-pos (point))))
                        (save-excursion
                          (goto-char (match-beginning 0))
                          (ignore-errors
                            (progn
                              (funcall hs-forward-sexp-func 1)
                              (> (line-number-at-pos (point)) begin-line)))))
                    t)))
            (when doit
              (put-text-property 0
                                 (length marker-string)
                                 'display
                                 (list 'left-fringe
                                       'hideshowvis-hideable-marker
                                       'hideshowvis-hidable-face)
                                 marker-string)
              (overlay-put ovl 'before-string marker-string)
              (overlay-put ovl 'hideshowvis-hs t))))))))

;;;###autoload
(defun hideshowvis-click-fringe (event)
  (interactive "e")
  (mouse-set-point event)
  (end-of-line)
  (if (save-excursion
        (end-of-line 1)
        (or (hs-already-hidden-p)
            (progn
              (forward-char 1)
              (hs-already-hidden-p))))
      (hs-show-block)
    (hs-hide-block)
    (beginning-of-line)))

(defvar hideshowvis-mode-map
  (let ((hideshowvis-mode-map (make-sparse-keymap)))
    (define-key hideshowvis-mode-map [left-fringe mouse-1]
      'hideshowvis-click-fringe)
    hideshowvis-mode-map)
  "Keymap for hideshowvis mode")

;;;###autoload
(define-minor-mode hideshowvis-minor-mode ()
  "Will indicate regions foldable with hideshow in the fringe."
  :init-value nil
  :require 'hideshow
  :group 'hideshow
  :keymap hideshowvis-mode-map
  (condition-case nil
      (if hideshowvis-minor-mode
          (progn
            (hs-minor-mode 1)
            (hideshowvis-highlight-hs-regions-in-fringe (point-min) (point-max) 0)
            (add-to-list 'after-change-functions
                         'hideshowvis-highlight-hs-regions-in-fringe))
        (remove-overlays (point-min) (point-max) 'hideshowvis-hs t)
        (setq after-change-functions
              (remove 'hideshowvis-highlight-hs-regions-in-fringe
                      after-change-functions)))
    (error
     (message "Failed to toggle hideshowvis-minor-mode")
     )))

;;;###autoload
(defun hideshowvis-enable ()
  "Will enable hideshowvis minor mode"
  (interactive)
  (hideshowvis-minor-mode 1))

;;;###autoload
(defun hideshowvis-symbols ()
  "Defines the things necessary to get a + symbol in the fringe
and a yellow marker indicating the number of hidden lines at
the end of the line for hidden regions."
  (interactive)
  
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0]))
  
  (defcustom hs-fringe-face 'hs-fringe-face
    "*Specify face used to highlight the fringe on hidden regions."
    :type 'face
    :group 'hideshow)
  
  (defface hs-fringe-face
    '((t (:foreground "#888" :box (:line-width 2 :color "grey75" :style released-button))))
    "Face used to highlight the fringe on folded regions"
    :group 'hideshow)
  
  (defcustom hs-face 'hs-face
    "*Specify the face to to use for the hidden region indicator"
    :type 'face
    :group 'hideshow)
  
  (defface hs-face
    '((t (:background "#ff8" :box t)))
    "Face to hightlight the ... area of hidden regions"
    :group 'hideshow)
  
  (defun display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((marker-string "*fringe-dummy*")
             (marker-length (length marker-string))
             (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov))))
             )
        (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
        (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
        (overlay-put ov 'before-string marker-string)
        (put-text-property 0 (length display-string) 'face 'hs-face display-string)
        (overlay-put ov 'display display-string)
        )))
  
  (setq hs-set-up-overlay 'display-code-line-counts))
(provide 'hideshowvis)
;; ---------------------- ENDHIDESHOW -------------------------


;; Duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "C-d") 'duplicate-line)

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (t'ransient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))
(global-set-key [\M-up] 'move-text-up)
(global-set-key [\M-down] 'move-text-down)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)
(which-key-mode)
(add-hook 'prog-mode-hook #'lsp)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      create-lockfiles nil) ;; lock files will kill `npm start'
(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))

  
(defun g-screenshot-on-buffer-creation ()
  (setq display-fill-column-indicator-column nil)
  (setq line-spacing nil))

(use-package screenshot
  ;; :straight (:type git :host github :repo "tecosaur/screenshot")
  :config
  (setq screenshot-line-numbers-p nil)
  
  (setq screenshot-min-width 80)
  (setq screenshot-max-width 90)
  (setq screenshot-truncate-lines-p nil)
  ;;
  (setq screenshot-text-only-p nil)
  ;;
  (setq screenshot-font-family "Cascadia Code")
  (setq screenshot-font-size 10)
  ;;
  (setq screenshot-border-width 16)
  (setq screenshot-radius 10)
  ;;
  (setq screenshot-shadow-intensity 90)
  (setq screenshot-shadow-radius 8)
  (setq screenshot-shadow-offset-horizontal 1)
  (setq screenshot-shadow-offset-vertical 4)

  :hook
  ((screenshot-buffer-creation-hook . g-screenshot-on-buffer-creation)))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("788121c96b7a9b99a6f35e53b7c154991f4880bb0046a80330bb904c1a85e275" default))
 '(global-display-line-numbers-mode t)
 '(highlight-indent-guides-method 'character)
 '(minimap-minimum-width 15)
 '(minimap-window-location 'right)
 '(neo-theme 'nerd)
 '(package-selected-packages
   '(ac-geiser lsp-install-server init-dir jest-test-mode flycheck-jest jest highlight-indent-guides minimap beacon nlinum tide typescript-mode web-mode company-quickhelp dotenv-mode ac-php php-mode js3-mode js2-mode auto-rename-tag json-mode dap-mode hydra yasnippet-snippets web-beautify emmet-mode fix-word switch-window multiple-cursors neotree winum undo-tree rainbow-mode which-key all-the-icons page-break-lines try dashboard))
 '(screenshot-line-numbers-p t)
 '(screenshot-radius 20)
 '(screenshot-relative-line-numbers-p t)
 '(tool-bar-mode nil)
 '(wakatime-api-key "waka_c19164a7-3121-4b52-b5d5-d9ae994b7c46"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Cascadia Code PL" :foundry "SAJA" :slant normal :weight regular :height 120 :width normal))))
 '(highlight-indent-guides-character-face ((t (:foreground "#252525"))))
 '(minimap-active-region-background ((t (:extend t :background "#131315"))))
 '(minimap-current-line-face ((t (:background "#17202A"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))
