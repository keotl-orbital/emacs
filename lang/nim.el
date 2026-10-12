;;; nim.el --- -*- lexical-binding: t; -*-

(use-package nim-mode
  :ensure t
  :hook (
         (nim-mode . eglot-ensure)
         )
  )

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(nim-mode . ("nimlangserver" "--stdio")))
  )
