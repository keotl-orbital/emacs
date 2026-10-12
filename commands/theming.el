;;; theming.el --- -*- lexical-binding: t; -*-

(defun load-light-theme ()
  "Load the Savoy Truffle Light theme."
  (interactive)
  (load-theme 'savoy-truffle-light t))
(defun load-dark-theme ()
  "Load the Savoy Truffle theme."
  (interactive)
  (load-theme 'savoy-truffle t))
