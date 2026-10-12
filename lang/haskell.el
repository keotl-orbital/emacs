;;; haskell.el --- -*- lexical-binding: t; -*-

(use-package haskell-mode
  ;; :ensure t
  ;; :hook (
  ;;        ;; (haskell-mode . lsp-deferred)
  ;;        ;; (haskell-mode . smartparens-mode)
  ;;        )
    :bind (:map haskell-mode-map ("C-c C-f" . lsp-format-buffer))
  )
