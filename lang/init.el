;;; init.el --- -*- lexical-binding: t; -*-

(use-package eglot
  :bind
  (:map evil-normal-state-map
	("à" . eglot-code-actions))
  :config
  (electric-pair-mode 1)
  (company-mode 1)
  (setq eldoc-echo-area-use-multiline-p nil)
  )

(load  (concat (file-name-directory load-file-name) "eldoc-box.el"))

(use-package autoinsert
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)

  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  ;; (add-hook 'find-file-hook 'auto-insert)
  ;; (auto-insert-mode 1)

  :config
  ;; (define-auto-insert "\\.html?$" "default-html.html")
  )

(setq-default indent-tabs-mode nil)
(load  (concat (file-name-directory load-file-name) "scala.el"))
(load  (concat (file-name-directory load-file-name) "typescript.el"))
(load  (concat (file-name-directory load-file-name) "yaml.el"))
(load  (concat (file-name-directory load-file-name) "python.el"))
(load  (concat (file-name-directory load-file-name) "terraform.el"))
(load  (concat (file-name-directory load-file-name) "csharp.el"))
(load  (concat (file-name-directory load-file-name) "rust.el"))
(load  (concat (file-name-directory load-file-name) "clang.el"))
(load  (concat (file-name-directory load-file-name) "nim.el"))
;; (load  (concat (file-name-directory load-file-name) "haskell.el"))
