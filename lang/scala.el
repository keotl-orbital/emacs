;;; scala.el --- -*- lexical-binding: t; -*-

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(scala-ts-mode . ("metals")))
;; TODO change language-server path
  )

(define-auto-insert '("\\.scala\\'" . "scala skeleton")
     '(
       (java-namespace (buffer-file-name))
       "package " str \n \n
       ))


(with-eval-after-load 'scala-ts-mode
  (add-hook 'scala-ts-mode-hook 'eglot-ensure)
  (add-hook 'scala-ts-mode-hook 'auto-insert-mode)
  )
(load  (concat (file-name-directory load-file-name) "../vendor/scala-ts-mode.el"))
(setq treesit-font-lock-level 4)
