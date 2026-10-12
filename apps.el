;;; apps.el --- -*- lexical-binding: t; -*-

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("M-q" . vterm-send-next-key)
              ("M-v" . vterm-yank)
              )
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'insert)
    (evil-define-key 'insert vterm-mode-map
      (kbd "C-e") 'vterm--self-insert
      (kbd "C-w") 'vterm--self-insert
      (kbd "C-r") 'vterm--self-insert
      (kbd "C-a") 'vterm--self-insert
      (kbd "C-k") 'vterm--self-insert
      (kbd "C-p") 'vterm--self-insert
      (kbd "C-n") 'vterm--self-insert
      (kbd "C-d") 'vterm--self-insert
      ))
  (when (eq system-type 'darwin)
    (setq vterm-shell "fish")
    )

  :init
  (add-hook 'vterm-mode-hook
            (lambda () (setq-local show-trailing-whitespace nil)))
  )

(load  (concat (file-name-directory load-file-name) "./vendor/vterm-anti-flicker-filter.el"))

;; (use-package aidermacs
;;   :ensure t
;;   :bind(("C-c a" . aidermacs-transient-menu))
;;   ;; :config
;;   ;;(setenv "ANTHROPIC_API_KEY" "sk-...")
;;   :custom
;;   (aidermacs-default-chat-mode 'architect)
;;   (aidermacs-default-model "sonnet")
;;  )

;; Minor mode for vterm wrapped apps
(define-minor-mode vterm-app-mode
  "Minor mode for vterm wrapped applications with q to close window."
  :lighter " VApp"
  :keymap (let ((map (make-sparse-keymap)))
            (with-eval-after-load 'evil
              (evil-define-key 'normal map (kbd "q") 'delete-window))
            map))

(defun start-vterm-wrapper-app (executable)
  "Start a new vterm instance, run EXECUTABLE command, and rename buffer to *EXECUTABLE*.
If *EXECUTABLE* buffer already exists, switch to it instead.
Starts in the project root directory if available."
  (let* ((buffer-name (format "*%s*" executable))
         (existing-buffer (get-buffer buffer-name))
         (project (project-current))
         (project-root (when project (project-root project))))
    (if existing-buffer
        (switch-to-buffer existing-buffer)
      (let ((default-directory (or project-root default-directory)))
        (let ((vterm-buffer (vterm t))) ; Create new vterm instance
          (with-current-buffer vterm-buffer
            (vterm-send-string executable)
            (vterm-send-return)
            (rename-buffer buffer-name t)
            (vterm-app-mode 1)))))))

(defun start-claude-vterm ()
  "Start a new vterm instance with claude."
  (interactive)
  (start-vterm-wrapper-app "claude"))

(defun start-aider-vterm ()
  "Start a new vterm instance with aider."
  (interactive)
  (start-vterm-wrapper-app "aider"))

(defun start-codex-vterm ()
  "Start a new vterm instance with codex."
  (interactive)
  (start-vterm-wrapper-app "codex"))

(defun start-opencode-vterm ()
  "Start a new vterm instance with opencode."
  (interactive)
  (start-vterm-wrapper-app "opencode"))

(defun start-pi-vterm ()
  "Start a new vterm instance with pi."
  (interactive)
  (start-vterm-wrapper-app "pi"))

(defun start-vterm-default ()
  "Start a new vterm instance with default profile."
  (interactive)
  (start-vterm-wrapper-app "fish"))

(defun start-claude-vterm-other-window ()
  "Start a new vterm instance with claude in another window."
  (interactive)
  (if (one-window-p)
      (split-window-right))
  (other-window 1)
  (start-claude-vterm))

(defun start-aider-vterm-other-window ()
  "Start a new vterm instance with aider in another window."
  (interactive)
  (if (one-window-p)
      (split-window-right))
  (other-window 1)
  (start-aider-vterm))

(defun start-codex-vterm-other-window ()
  "Start a new vterm instance with codex in another window."
  (interactive)
  (if (one-window-p)
      (split-window-right))
  (other-window 1)
  (start-codex-vterm))

(defun start-opencode-vterm-other-window ()
  "Start a new vterm instance with opencode in another window."
  (interactive)
  (if (one-window-p)
      (split-window-right))
  (other-window 1)
  (start-opencode-vterm))

(defun start-pi-vterm-other-window ()
  "Start a new vterm instance with pi in another window."
  (interactive)
  (if (one-window-p)
      (split-window-right))
  (other-window 1)
  (start-pi-vterm))

(use-package agent-shell
      :ensure t
      :config
      ;; (add-to-list 'exec-path "~/.opencode/bin/")
      )
