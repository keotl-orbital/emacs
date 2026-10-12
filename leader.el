;; Files
(define-key evil-normal-state-map (kbd "SPC f") '("Files 🔍" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC f f") 'find-file)
(define-key magit-mode-map (kbd "SPC f f") 'find-file)
(define-key magit-mode-map (kbd "SPC f r") 'recentf)

;; Project
(defun project-find-regexp-region (beg end)
  "Search the project for the text selected between BEG and END."
  (interactive "r")
  (project-find-regexp (regexp-quote (buffer-substring-no-properties beg end))))

(define-key evil-normal-state-map (kbd "SPC p f") 'project-find-file)
(define-key evil-normal-state-map (kbd "SPC p d") 'project-find-dir)
(define-key evil-normal-state-map (kbd "SPC p D") 'project-dired)
(define-key evil-normal-state-map (kbd "SPC p e") 'flymake-show-project-diagnostics)
(define-key evil-normal-state-map (kbd "SPC p t") 'find-test-file)
(define-key evil-normal-state-map (kbd "SPC p T") 'find-test-file-other-window)
(define-key evil-normal-state-map (kbd "SPC p x") 'project-execute-extended-command)
(define-key evil-normal-state-map (kbd "SPC p !") 'project-shell-command)
(define-key evil-normal-state-map (kbd "SPC p s") '("Search/Replace 🔍" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC p s g") 'project-find-regexp)
(define-key evil-visual-state-map (kbd "SPC p s g") 'project-find-regexp-region)
(define-key evil-normal-state-map (kbd "SPC p s r") 'project-query-replace-regexp)
(define-key evil-normal-state-map (kbd "SPC p b") '("Project buffers 🪟" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC p b b") 'project-switch-to-buffer)
(define-key evil-normal-state-map (kbd "SPC p b k") 'project-kill-buffers)
(define-key evil-normal-state-map (kbd "SPC p b C-b") 'project-list-buffers)

;; VCS
(define-key evil-normal-state-map (kbd "SPC g") '("VCS/Tree 📐" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC g s") 'magit-status)
(define-key evil-normal-state-map (kbd "SPC g t") 'treemacs)
(define-key evil-normal-state-map (kbd "SPC g a") 'vc-annotate)

;; Eglot
(define-key evil-normal-state-map (kbd "SPC m") '("Language ✨" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC m a") '("Actions" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC m a a") 'eglot-code-actions)
(define-key evil-normal-state-map (kbd "SPC m r") '("Refactor" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC m r r") 'eglot-rename)
(define-key evil-normal-state-map (kbd "SPC m r o") 'eglot-code-action-organize-imports)
(define-key evil-normal-state-map (kbd "SPC m r x") 'eglot-code-action-extract)
(define-key evil-normal-state-map (kbd "SPC m r w") 'eglot-code-action-rewrite)
(define-key evil-normal-state-map (kbd "SPC m w") '("Workspace" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC m w r") 'eglot-reconnect)
(define-key evil-normal-state-map (kbd "SPC m w q") 'eglot-shutdown)
(define-key evil-normal-state-map (kbd "SPC m f") 'eglot-format)
(define-key evil-normal-state-map (kbd "SPC m d") 'eldoc)
(define-key evil-normal-state-map (kbd "SPC m g g") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "SPC m g G") 'xref-find-definitions-other-window)
(define-key evil-normal-state-map (kbd "SPC m g r") 'xref-find-references)

;; Toggles
(define-key evil-normal-state-map (kbd "SPC t") '("Toggles 🎛️️" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC t c") 'global-subword-mode)
(define-key evil-normal-state-map (kbd "SPC t f") 'flyspell-mode)
(define-key evil-normal-state-map (kbd "SPC t w") 'toggle-trailing-whitespace)
(define-key evil-normal-state-map (kbd "SPC t l") 'toggle-truncate-lines)
(define-key evil-normal-state-map (kbd "SPC t s") '("Text Size 🅰" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC t s t") 'text-scale-decrease)
(define-key evil-normal-state-map (kbd "SPC t s s") 'text-scale-increase)
(define-key evil-normal-state-map (kbd "SPC t d") 'eldoc-box-hover-at-point-mode)
(define-key evil-normal-state-map (kbd "SPC t t") '("Theming 💅" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC t t t") 'toggle-transparency)
(define-key evil-normal-state-map (kbd "SPC t t l") 'load-light-theme)
(define-key evil-normal-state-map (kbd "SPC t t d") 'load-dark-theme)

;; Text/Spelling
(define-key evil-normal-state-map (kbd "SPC s") '("Text/Spelling 📚" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC s s") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC s S") 'save-some-buffers)
(define-key evil-normal-state-map (kbd "SPC s c") 'flyspell-correct-word-before-point)
(define-key evil-visual-state-map (kbd "SPC s ,") 'comment-dwim)
(define-key evil-normal-state-map (kbd "SPC s ,") 'comment-line)

;; Interaction
(define-key evil-normal-state-map (kbd "SPC i") '("Interaction 🕹️" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC i i") 'yas-insert-snippet)
(define-key evil-normal-state-map (kbd "SPC i u") 'insert-char)

;; Buffers
(define-key evil-normal-state-map (kbd "SPC b") '("Buffers 🪟" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC b b") 'switch-to-buffer)
(define-key evil-normal-state-map (kbd "SPC b B") 'switch-to-buffer-other-window)
(define-key evil-normal-state-map (kbd "SPC b l") 'kill-current-buffer)

;; Help
(define-key evil-normal-state-map (kbd "SPC h") '("Help ❓" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC h t") 'describe-text-properties)
(define-key evil-normal-state-map (kbd "SPC h v") 'describe-variable)
(define-key evil-normal-state-map (kbd "SPC h f") 'describe-function)
(define-key evil-normal-state-map (kbd "SPC h m") 'describe-mode)
(define-key evil-normal-state-map (kbd "SPC h k") 'describe-key)
(define-key evil-normal-state-map (kbd "SPC h K") 'describe-keymap)
(define-key evil-normal-state-map (kbd "SPC h .") 'eldoc-doc-buffer)

;; Apps
(define-key evil-normal-state-map (kbd "SPC a") '("Apps 📱" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC a t") 'vterm)
(define-key evil-normal-state-map (kbd "SPC a c") 'start-claude-vterm)
(define-key evil-normal-state-map (kbd "SPC a C") 'start-claude-vterm-other-window)
(define-key evil-normal-state-map (kbd "SPC a x") 'start-codex-vterm)
(define-key evil-normal-state-map (kbd "SPC a X") 'start-codex-vterm-other-window)
(define-key evil-normal-state-map (kbd "SPC a o") 'start-opencode-vterm)
(define-key evil-normal-state-map (kbd "SPC a O") 'start-opencode-vterm-other-window)
(define-key evil-normal-state-map (kbd "SPC a p") 'start-pi-vterm)
(define-key evil-normal-state-map (kbd "SPC a P") 'start-pi-vterm-other-window)
(define-key evil-normal-state-map (kbd "SPC a j") 'elmux-jump-to-waiting)
(define-key evil-normal-state-map (kbd "SPC a a") '("Elmux" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC a a n") 'elmux-workspace-next)
(define-key evil-normal-state-map (kbd "SPC a a k") 'elmux-workspace-prev)
(define-key evil-normal-state-map (kbd "SPC a a w") 'elmux-workspace-switch)
(define-key evil-normal-state-map (kbd "SPC a a s") 'elmux-sidebar-toggle)
(define-key evil-normal-state-map (kbd "SPC a a N") 'elmux-workspace-create)
(define-key evil-normal-state-map (kbd "SPC a a c") 'elmux-workspace-claude)
(define-key evil-normal-state-map (kbd "SPC a a t") 'elmux-workspace-vterm)
(define-key evil-normal-state-map (kbd "SPC a a x") 'elmux-workspace-codex)
(define-key evil-normal-state-map (kbd "SPC a a o") 'elmux-workspace-opencode)
(define-key evil-normal-state-map (kbd "SPC a a p") 'elmux-workspace-pi)
(define-key evil-normal-state-map (kbd "SPC a a d") 'elmux-dashboard)
(define-key evil-normal-state-map (kbd "SPC a a l") 'tab-line-switch-to-next-tab)
(define-key evil-normal-state-map (kbd "SPC a a h") 'tab-line-switch-to-prev-tab)
(define-key evil-normal-state-map (kbd "SPC a d") 'dictionary-search)

;; Quit
(define-key evil-normal-state-map (kbd "SPC q") '("Quit 🚪" . (keymap)))
(define-key evil-normal-state-map (kbd "SPC q q") 'save-buffers-kill-terminal)
(define-key evil-normal-state-map (kbd "SPC q r") 'reload-config)

;; copy all keys in evil's "SPC" prefix to magit's "SPC"
(dolist (map '(magit-mode-map
               magit-log-mode-map
               magit-process-mode-map
               magit-diff-mode-map
               magit-revision-mode-map
               magit-stash-mode-map
               magit-stashes-mode-map
               magit-refs-mode-map
               magit-reflog-mode-map))
  (when (boundp map)
    (define-key (symbol-value map) (kbd "SPC")
                (lookup-key evil-normal-state-map (kbd "SPC")))))
