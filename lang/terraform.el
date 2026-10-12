;;; terraform.el --- -*- lexical-binding: t; -*-

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve")))
  )
(with-eval-after-load 'terraform-mode
  (add-hook 'terraform-mode-hook 'eglot-ensure)
  )

(load  (concat (file-name-directory load-file-name) "../vendor/hcl-mode.el"))
(load  (concat (file-name-directory load-file-name) "../vendor/terraform-mode.el"))
