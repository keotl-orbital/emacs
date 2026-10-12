;;; spelling.el --- -*- lexical-binding: t; -*-

(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--camel-case" "--sug-mode=ultra" "--run-together-limit=2"))
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mode-map (kbd "C-.") nil)
  ))
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
