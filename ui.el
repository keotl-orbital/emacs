;;; ui.el --- -*- lexical-binding: t; -*-


;; (global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(setq-default show-trailing-whitespace t)

;; Modeline
(use-package all-the-icons
  :ensure t)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


;; Treemacs
(use-package treemacs
  :ensure t
  :config
  (treemacs-resize-icons 16)
  ;; :bind (("M-m g t" . treemacs-select-window)
  ;; 	 ("M-m g T" . treemacs)
  ;; 	 )
  )

(load  (concat (file-name-directory load-file-name) "./vendor/breadcrumb.el"))
(breadcrumb-mode)
(setq breadcrumb-project-crumb-separator " / ")
(setq breadcrumb-project-max-length 100)
(setq breadcrumb-imenu-max-length 100)
