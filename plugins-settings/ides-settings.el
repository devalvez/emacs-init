(use-package reformatter
  :ensure t)

(use-package go-mode
  :ensure t)

(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


(use-package js-import
  :ensure t)

(use-package company-web
  :ensure t)

(use-package react-snippets
  :ensure t)

(use-package auto-rename-tag
  :ensure t)

(use-package css-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  ;; enable Emmet's css abbreviation.
  (add-hook 'css-mode-hook  'emmet-mode)
  (setq emmet-expand-jsx-className? t) ;; default nil
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil))))

(use-package yasnippet-snippets
  :ensure t
  :defer t)

(defun tidy-buffer ()
  "Run Tidy HTML parser on current buffer."
  (interactive)
  (if (get-buffer "tidy-errs") (kill-buffer "tidy-errs"))
  (shell-command-on-region (point-min) (point-max)
                           "tidy -f /tmp/tidy-errs -q -i -wrap 72 -c" t)
  (find-file-other-window "/tmp/tidy-errs")
  (other-window 1)
  (delete-file "/tmp/tidy-errs")
  (message "buffer tidy'ed"))
(global-set-key (kbd "C-x t") 'tidy-buffer)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1)
  (setq yas-snippet-dirs
	      ;; personal snippets
	      '("~/.emacs.d/snippets"
	        ;; foo-mode and bar-mode snippet collection
          "~/.emacs.d/yasnippet-snippets-20210408.1234/snippets")))

(use-package markdown-mode
  :ensure t)

(use-package dotenv-mode
  :ensure t
  :config (setq lsp-language-id-configuration
		            '((dotenv-mode . "dotenv"))))

;; TypeScript Config
(load-file "~/.emacs.d/plugins-settings/typescript-settings.el")

;; Prisma ORM Syntax
(add-to-list 'load-path "~/.emacs.d/plugins/prisma-mode")
(autoload 'prisma-mode "prisma-mode" nil t)
(setq lsp-language-id-configuration '((prisma-mode . "prisma")))
(add-to-list 'auto-mode-alist '("\\.prisma\\'". prisma-mode))
(setq prisma-format-on-save t)

(setq lsp-eslint-enable t)
(setq lsp-eslint-run 'onSave)
(setq flycheck-disabled-checkers '(javascript-jshint))
(setq flycheck-checkers '(javascript-eslint))
(add-hook 'js-mode-hook
          (lambda ()
            (flycheck-mode t)
            (tern-mode t)))

(use-package lsp-tailwindcss
  :ensure t
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (setq lsp-tailwindcss-major-modes '(web-mode html-mode sgml-mode css-mode rjsx-mode)))
