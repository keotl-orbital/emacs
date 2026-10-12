;;; rust.el --- -*- lexical-binding: t; -*-

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyser")))
  )


(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'eglot-ensure)
  )

(use-package rust-mode
  :ensure t
  :init
  ;; (setq rust-mode-treesitter-derive t)
  :config
  (evil-define-key 'normal rust-mode-map
    (kbd "SPC m a c") 'rust-compile
    (kbd "SPC m a k") 'rust-check
    (kbd "SPC m a t") 'rust-test
    (kbd "SPC m a r") 'rust-run
    (kbd "SPC m a l") 'rust-run-clippy
    (kbd "SPC m a m") 'rust-toggle-mutability)
  )
