;;; yaml.el --- -*- lexical-binding: t; -*-

(use-package yaml-mode
  :ensure t
  :hook (yaml-mode . (lambda ()
		       (highlight-indentation-mode 1)
		       (auto-fill-mode -1)
		       )))
