;;; vcs.el --- -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :config
  ;; (add-hook 'magit-status-mode-hook 'evil-normal-state)
  ;; (+layout-bepo-rotate-ts-bare-keymap '(magit-section-mode-map))
  ;; (+layout-bepo-rotate-keymaps
  ;;    '(magit-log-read-revs-map
  ;;      magit-log-mode-map
  ;;      ;; NOTE: magit-cherry-mode could be moved of magit-log anyday, be
  ;;      ;; careful
  ;;      magit-cherry-mode-map))
  ;; (+layout-bepo-rotate-keymaps '(magit-reflog-mode-map))
  ;; (+layout-bepo-rotate-keymaps
  ;;    '(magit-status-mode-map
  ;;      magit-staged-section-map
  ;;      magit-unstaged-section-map
  ;;      magit-untracked-section-map))
  ;; (+layout-bepo-rotate-keymaps
  ;;    '(magit-diff-mode-map
  ;;      magit-diff-section-base-map))
  ;; (+layout-bepo-rotate-keymaps
  ;;  '(magit-process-mode-map))
  ;; (+layout-bepo-rotate-keymaps
  ;;  '(magit-refs-mode-map))
  ;; (+layout-bepo-rotate-keymaps
  ;;  '(magit-blob-mode-map))
  ;; (+layout-bepo-rotate-ts-bare-keymap
  ;;  '(magit-mode-map))
  (setq magit-diff-visit-prefer-worktree t)
  :bind
  ;; magit-mode
  ;; (:map magit-mode-map ("<escape>" . ignore))
  (:map magit-mode-map ("<SPC>" . nil))
  (:map magit-mode-map ("w" . nil))
  (:map magit-mode-map ("w w" . 'evil-window-next))
  (:map magit-mode-map ("w c" . 'evil-window-left))
  (:map magit-mode-map ("w r" . 'evil-window-right))
  (:map magit-mode-map ("w s" . 'evil-window-up))
  (:map magit-mode-map ("w t" . 'evil-window-down))
  (:map magit-mode-map ("w o" . 'delete-other-windows))
  (:map magit-mode-map ("w l" . 'evil-window-delete))
  (:map magit-mode-map ("w k" . 'evil-window-split))
  (:map magit-mode-map ("w v" . 'evil-window-vsplit))
  ;; magit-log-mode
  (:map magit-log-mode-map ("<SPC>" . nil))
  (:map magit-log-mode-map ("w" . nil))
  (:map magit-log-mode-map ("w w" . 'evil-window-next))
  (:map magit-log-mode-map ("w c" . 'evil-window-left))
  (:map magit-log-mode-map ("w r" . 'evil-window-right))
  (:map magit-log-mode-map ("w s" . 'evil-window-up))
  (:map magit-log-mode-map ("w t" . 'evil-window-down))
  (:map magit-log-mode-map ("w o" . 'delete-other-windows))
  (:map magit-log-mode-map ("w l" . 'evil-window-delete))
  (:map magit-log-mode-map ("w k" . 'evil-window-split))
  (:map magit-log-mode-map ("w v" . 'evil-window-vsplit))
  ;; magit-process-mode
  (:map magit-process-mode-map ("<SPC>" . nil))
  (:map magit-process-mode-map ("w" . nil))
  (:map magit-process-mode-map ("w w" . 'evil-window-next))
  (:map magit-process-mode-map ("w c" . 'evil-window-left))
  (:map magit-process-mode-map ("w r" . 'evil-window-right))
  (:map magit-process-mode-map ("w s" . 'evil-window-up))
  (:map magit-process-mode-map ("w t" . 'evil-window-down))
  (:map magit-process-mode-map ("w o" . 'delete-other-windows))
  (:map magit-process-mode-map ("w l" . 'evil-window-delete))
  (:map magit-process-mode-map ("w k" . 'evil-window-split))
  (:map magit-process-mode-map ("w v" . 'evil-window-vsplit))

  ;; (:map magit-mode-map ("s" . nil))
  ;; (:map magit-mode-map ("t" . nil))
  )
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun +magit-status-if-git-root-no-args ()
  (when (and (not (seq-some #'buffer-file-name (buffer-list)))
             (fboundp 'magit-toplevel)
             (let ((top (magit-toplevel)))
               (and top (file-equal-p default-directory top))))
    (magit-status)))

(add-hook 'emacs-startup-hook #'+magit-status-if-git-root-no-args)

;; VC Annotate mode bindings
(with-eval-after-load 'vc-annotate
  (with-eval-after-load 'evil
    (evil-define-key 'normal vc-annotate-mode-map
      (kbd "RET") 'magit-show-commit)))
