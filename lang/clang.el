;;; clang.el --- -*- lexical-binding: t; -*-

;; apt install clangd bear

;; C
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(c-mode . ("clangd")))
  )

(define-auto-insert '("\\.h\\'" . "C header skeleton")
  '(
    (upcase (concat (file-name-base (buffer-file-name)) "_H_"))
    "#ifndef " str \n
    "#define " str \n \n
    _ \n \n
    "#endif // " str \n
    ))

(define-auto-insert '("\\.c\\'" . "C source skeleton")
  '(
    (concat (file-name-base (buffer-file-name)) ".h")
    "#include \"" str "\"" \n
    ))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'auto-insert-mode)

(evil-define-key 'normal c-mode-map (kbd "SPC m g o") 'ff-find-other-file)
(evil-define-key 'normal c-mode-map (kbd "SPC m g O") 'ff-find-other-file-other-window)

;; C++
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(c++-mode . ("clangd")))
  )

(define-auto-insert '("\\.cpp\\'" . "C++ source skeleton")
  '(
    (concat (file-name-base (buffer-file-name)) ".h")
    "#include \"" str "\"" \n
    ))

(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'auto-insert-mode)

(evil-define-key 'normal c++-mode-map (kbd "SPC m g o") 'ff-find-other-file)
(evil-define-key 'normal c++-mode-map (kbd "SPC m g O") 'ff-find-other-file-other-window)
