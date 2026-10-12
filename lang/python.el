;;; python.el --- -*- lexical-binding: t; -*-

(load  (concat (file-name-directory load-file-name) "../vendor/py-yapf.el"))
(load  (concat (file-name-directory load-file-name) "../vendor/python-isort.el"))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("~/.emacs.d/.cache/lsp/npm/node_modules/.bin/pyright-langserver" "--stdio")))
  )

(add-hook 'python-mode-hook 'eglot-ensure)

(defun python-mode-keybindings ()
  (evil-local-set-key 'normal (kbd "SPC m f") 'py-yapf-buffer))

(add-hook 'python-mode-hook 'python-mode-keybindings)
