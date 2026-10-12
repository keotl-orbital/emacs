;;; toggle-trailing-whitespace.el --- -*- lexical-binding: t; -*-

(defun toggle-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  )
