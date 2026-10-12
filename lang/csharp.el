;;; csharp.el --- -*- lexical-binding: t; -*-

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(csharp-ts-mode . ("omnisharp" "-lsp")))
  )

;; (define-auto-insert '("\\.scala\\'" . "scala skeleton")
;;      '(
;;        (java-namespace (buffer-file-name))
;;        "package " str \n \n
;;        ))


(add-hook 'csharp-ts-mode-hook 'eglot-ensure)
(setq treesit-font-lock-level 4)

(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-ts-mode))
