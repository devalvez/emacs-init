;; init.el -- Emacs initialisation -- lexical-binding: t --:
;;
;;; Commentary:
;;
;;; Code:
;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq-default truncate-lines 2)
(setq-default cursor-type 'box)
;;(setq-default cursor-type '(bar . 1))
(global-linum-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq-default smooth-scroll-margin 0)
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 100000)
(show-paren-mode)
(which-function-mode 1)
(global-hl-line-mode)
(defvar blink-cursor-interval-visible 0.1)

(setq-default indicate-empty-lines t)
;; (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
;; (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
;; (set-fringe-bitmap-face 'tilde 'font-lock-function-name-face)
;; (setq-default indincate-empty-lines t)

;; (setq-default
;;  whitespace-line-column 80
;;  whitespace-style       '(face lines-tail))
;; (add-hook 'prog-mode-hook #'whitespace-mode)

;;condicional
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

(use-package paradox
  :ensure t
  :config (paradox-enable))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner "~/.emacs.d/emacs.png")
  (setq dashboard-footer-icon (all-the-icons-octicon "flame"
                                                    :height 1.1
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

           (,(all-the-icons-faicon "bitbucket" :height 1.1 :v-adjust 0.0)
            "Bitbucket"
            "bitbucket.org/WesleyAntonioAlves/"
            (lambda (&rest _) (browse-url "https://bitbucket.org/WesleyAntonioAlves/")))
           
           ))))

(use-package try
  :ensure t)

(use-package wakatime-mode
  :ensure t)
(global-wakatime-mode)

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

(when (>= emacs-major-version 27)
(use-package blamer
  :quelpa (blamer :fetcher github :repo "artawower/blamer.el")
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :config
  (setq blamer-author-formatter " ✎ %s ")
  (setq blamer-commit-formatter "● %s")
  (setq blamer-type 'both)
  (setq blamer-prettify-time-p t)
  (global-blamer-mode 1)))

;;Theme
(use-package kaolin-themes
  :ensure t
  :config (load-theme 'kaolin-aurora t))

(use-package spaceline
  :ensure t)

(use-package magit
  :ensure t)

(use-package minimap
  :ensure t)

;; (use-package spaceline-all-the-icons
;;   :ensure t
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme 'your-segment-symbol "Wesley A. Alves" 'etc)
;;   ;; Enable anzu searching
;;   (spaceline-all-the-icons--setup-anzu)
;;   ;; Enable package update indicator
;;   (spaceline-all-the-icons--setup-package-updates)
;;   ;; Enable # of commits ahead of upstream in git
;;   (spaceline-all-the-icons--setup-git-ahead)
;;   ;; Enable Paradox mode line
;;   (spaceline-all-the-icons--setup-paradox)
;;   ;; Enable Neotree mode line
;;   (spaceline-all-the-icons--setup-neotree)
;;   ;; Change Icons
;;   (setq spaceline-all-the-icons-icon-set-modified 'circle)
;;   (setq spaceline-all-the-icons-icon-set-bookmark 'bookmark)
;;   (setq spaceline-all-the-icons-icon-set-dedicated 'pin)
;;   (setq spaceline-all-the-icons-icon-set-window-numbering 'circle)
;;   (setq spaceline-all-the-icons-icon-set-window-eyebrowse-workspace 'circle)
;;   (setq spaceline-all-the-icons-icon-set-multiple-cursors 'caret)
;;   (setq spaceline-all-the-icons-icon-set-git-stats 'git-stats)
;;   (setq spaceline-all-the-icons-icon-set-flycheck-slim 'git-stats)
;;   (setq spaceline-all-the-icons-icon-set-sun-time 'sun/moon)
;;   (setq spaceline-all-the-icons-separator-type 'slant))

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

(use-package helm
  :ensure t
  :config (helm-mode 1))

(use-package helm-xref
  :ensure t
  :config
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini))

(use-package indent-guide
  :ensure t
  :init (indent-guide-global-mode))

(use-package mysql-to-org
  :ensure t)

(use-package helm-lsp
  :ensure t)

(use-package ido-vertical-mode
  :ensure t
  :config
  (progn
    (ido-mode 1)))
(setq ido-show-count t)

(use-package anzu
  :ensure t
  :config (anzu-mode 1))

(use-package flymake
  :ensure t
  :config
  (setq flymake-number-of-errors-to-display 8)
  (setq flymake-max-parallel-syntax-checks 8)
  (setq flymake-run-in-place t))

(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode))

(add-to-list 'load-path "~/.emacs.d/libs/flycheck")
(require 'flycheck)
(global-flycheck-mode)


;; (use-package flycheck
;;   :init (global-flycheck-mode)
;;   :ensure t)

;; (use-package flycheck-posframe
;;   :ensure t
;;   :after flycheck
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
;;   (set-face-attribute 'flycheck-posframe-info-face nil :inherit 'info)
;;   (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
;;   (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error)
;;   (setq flycheck-posframe-warning-prefix "\u26a0 ")
;;   (setq flycheck-posframe-border-width 10))

(add-to-list 'load-path "~/.emacs.d/libs/flycheck-posframe")
(require 'flycheck-posframe)
(global-flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
(set-face-attribute 'flycheck-posframe-info-face nil :inherit 'info)
(set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
(set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error)
(setq flycheck-posframe-warning-prefix "\u26a0 ")
(setq flycheck-posframe-border-width 10)

(use-package rainbow-mode
  :ensure t
  :mode "\\.css\\'")

(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-right-bottom)))

(use-package evil-nerd-commenter
  :ensure t
  :bind (("M-;" . 'evilnc-comment-or-uncomment-lines)))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1)
  :bind (
         ("C-z" . 'undo)
         ("C-S-z" . 'red)))

(use-package winum
  :ensure t
  :config (winum-mode))

(use-package all-the-icons
  :ensure t)

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
          "~/.emacs.d/yasnippet-snippets-20210408.1234/snippets"
          )))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package hydra
  :ensure t)

(use-package dap-mode
  :ensure t
  :init (add-hook 'dap-stopped-hook
		  (lambda (arg) (call-interactively #'dap-hydra))))

(use-package js-import
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package auto-rename-tag
  :ensure t
  :config (auto-rename-tag-mode t))

(use-package lsp-mode
  :ensure t)

(use-package lsp-treemacs
  :ensure t
  :config (lsp-treemacs-sync-mode 1))

(use-package web-mode
  :ensure t)
(use-package js2-mode
  :ensure t)
(use-package js3-mode
  :ensure t)
(use-package php-mode
  :ensure t
  :config (with-eval-after-load 'php-mode
	    (define-key php-mode-map (kbd "C-c C--") 'php-current-class)
	    (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace)))

(use-package ac-php
  :ensure t)

(use-package dotenv-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)


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

(use-package company
  :ensure t
  :config
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))
 
(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :ensure t))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
 
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
	web-mode-enable-auto-indentation nil
        )
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  ;; enable typescript-tslint checker
  ;;(flycheck-add-mode 'typescript-tslint 'web-mode)
  )
 
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
 
(use-package css-mode
  :config
(setq css-indent-offset 2))
;; End To Work TypeScript

;; bookmarks stuff--
(define-key global-map [f9] 'bookmark-jump)
(define-key global-map [f11] 'bookmark-set)
(setq bookmark-default-file "~/.emacs.d/bookmarks")  ;;define file to use.
(setq bookmark-save-flag 1)  ;save bookmarks to .emacs.bmk after each entry

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
;;EndHideshow

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" default))
 '(minimap-minimum-width 20)
 '(minimap-mode t)
 '(minimap-recreate-window nil)
 '(minimap-window-location 'right)
 '(neo-theme 'nerd)
 '(package-selected-packages
   '(wakatime-mode minimap blamer a quelpa-use-package quelpa indent-guide mysql-to-org flycheck-posframe magit spaceline json-mode dap-mode typescript-mode company google-translate js-import projectile try helm-lsp web-beautify fix-word switch-window ac-php dotenv-mode lsp-treemacs yasnippet-snippets lsp-mode helm-xref auto-rename-tag winum multiple-cursors neotree emmet-mode evil-nerd-commenter undo-tree all-the-icons which-key evil rainbow-delimiters ace-jump-mode ace-jump quick-peek flycheck-inline flycheck smex helm anzu smartparens kaolin-themes use-package))
 '(paradox-github-token t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(wakatime-api-key "47441236-aaab-4920-8a43-3dea8608abb6"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 105 :width normal))))
 '(minimap-active-region-background ((t (:extend t :background "#252525")))))
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End
;;; init.el ends here
