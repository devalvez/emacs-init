;; Instalar o LSP Mode
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (setq lsp-keymap-prefix "C-c l") ;; Define um prefixo de teclas para os comandos do LSP
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "ts-ls")
                    :major-modes '(typescript-mode)
                    :priority -1
                    :server-id 'ts-ls))
  :hook ((typescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         ))

(add-hook 'js-mode-hook #'lsp)
(add-hook 'jsx-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'typescript-tsx-mode-hook #'lsp)

(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode json-mode))
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))
(helm-mode)
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

;; Configuração do Tree-sitter para TypeScript e React
;; (use-package typescript-mode
;;   :after tree-sitter
;;   :config
;;   (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX")
;;   (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; Configuração adicional para TypeScript
(use-package typescript-mode
  :after tree-sitter
  :config
  (setq typescript-indent-level 2)
  (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; Usar tsx-ts-mode para arquivos .tsx
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; Configuração do Eglot para LSP
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((typescript-mode . ("typescript-language-server" "--stdio")))))

;; Configuração do Apheleia para autoformatação
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package web-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'auto-rename-tag-mode)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-comment-face t)
  (setq web-mode-enable-whitespaces-face t)
  (setq web-mode-enable-inlays t)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expand t)
  
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'typescript-mode))
  (web-mode-use-tabs)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook (lambda ()
                             (set (make-local-variable 'company-backends) '((company-web-html company-css company-dabbrev-code company-dabbrev)))
                             (flycheck-mode))))

(use-package jest
  :ensure t)

(use-package jest-test-mode
  :ensure t
  :after flycheck-jest)
