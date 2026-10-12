;;; typescript.el --- -*- lexical-binding: t; -*-

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(typescript-ts-mode . ("~/.emacs.d/.cache/lsp/npm/node_modules/.bin/typescript-language-server" "--stdio")))
(add-to-list 'eglot-server-programs
               '(tsx-ts-mode . ("~/.emacs.d/.cache/lsp/npm/node_modules/.bin/typescript-language-server" "--stdio")))
  )

(with-eval-after-load 'typescript-ts-mode
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  )

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(load  (concat (file-name-directory load-file-name) "../vendor/prettier-js.el"))

(defun tsx-ts-mode-keybindings ()
  "Set up tsx-ts-mode specific keybindings."
  (evil-local-set-key 'normal (kbd "SPC m f") 'prettier-js))

(add-hook 'tsx-ts-mode-hook 'tsx-ts-mode-keybindings)
(add-hook 'typescript-ts-mode-hook 'tsx-ts-mode-keybindings)
