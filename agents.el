;;; agents.el --- Overview of vterm agent sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; Workspaces are created intentionally with `agents-workspace-create',
;; which registers a NAME -> root DIRECTORY pair.  They appear in the
;; sidebar (`agents-sidebar-toggle') next to `main'; switching to one
;; creates its tab lazily, landing in a dired of the root.  A
;; workspace's sidebar icon reflects the most urgent Claude hook state
;; among vterm sessions whose `default-directory' is under its root.
;;
;; Build variants of the create command by computing a directory and
;; calling `agents-workspace-create' from Lisp.  Example: create a git
;; worktree and register it, with the worktree location configured
;; per-project through .dir-locals.el:
;;
;;   (defvar my-worktree-directory ".worktrees/"
;;     "Where to create worktrees, relative to the project root.")
;;   (put 'my-worktree-directory 'safe-local-variable #'stringp)
;;
;;   (defun my/workspace-create-worktree (branch)
;;     "Create a git worktree for BRANCH and register it as a workspace."
;;     (interactive "sBranch: ")
;;     (let* ((root (project-root (project-current t)))
;;            (dir (expand-file-name
;;                  (concat my-worktree-directory branch) root)))
;;       (unless (file-directory-p dir)
;;         (let ((default-directory root))
;;           (shell-command
;;            (format "git worktree add %s -b %s"
;;                    (shell-quote-argument dir)
;;                    (shell-quote-argument branch)))))
;;       (agents-workspace-create branch dir)))
;;
;;   ;; ~/Documents/project100x/.dir-locals.el
;;   ((nil . ((my-worktree-directory . ".worktrees/"))))
;;
;; The dir-local takes effect when the command is invoked from a buffer
;; inside that project (the defvar provides the default elsewhere).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'filenotify)
(require 'json)
(require 'project)
(require 'desktop)

(defvar vterm-environment)

(defvar agents-state-directory (expand-file-name "~/.cache/emacs-agents/")
  "Directory where Claude Code hooks write per-session state files.")

(defvar agents--states (make-hash-table :test 'equal)
  "Map of session-id to plist (:state SYMBOL :since FLOAT).")

(defvar-local agents--session-id nil
  "Unique id of this vterm session, also its state file name.")

(defun agents--parse-state (content)
  "Return state symbol from state-file CONTENT, or nil if unrecognized."
  (let ((s (string-trim (or content ""))))
    (when (member s '("working" "waiting" "done"))
      (intern s))))

(defun agents--state-sort-key (state)
  (or (cl-position state '(waiting done working running exited)) 5))

(defun agents--tool-name (buffer-name fallback)
  "Derive a tool label from BUFFER-NAME (e.g. \"*claude:proj*<2>\" -> \"claude\").
Use FALLBACK (typically the process name) for generic \"*vterm*\" buffers."
  (let ((base (and (string-match "\\`\\*\\([^:*]+\\)" buffer-name)
                   (match-string 1 buffer-name))))
    (if (or (null base) (string= base "vterm"))
        (or fallback base "?")
      base)))

(defun agents--format-since (seconds)
  (cond ((null seconds) "—")
        ((< seconds 60) (format "%ds" (floor seconds)))
        ((< seconds 3600) (format "%dm" (floor seconds 60)))
        (t (format "%dh" (floor seconds 3600)))))

(defun agents--badge-text (n)
  (when (> n 0) (format "●%d" n)))

(defvar agents--session-counter 0)
(defvar agents--watch nil)
(defvar agents--poll-timer nil)

(defun agents--new-session-id ()
  (format "%d-%d-%d" (emacs-pid) (floor (float-time))
          (cl-incf agents--session-counter)))

(defun agents--vterm-buffers ()
  (cl-remove-if-not
   (lambda (b) (provided-mode-derived-p
                (buffer-local-value 'major-mode b) 'vterm-mode))
   (buffer-list)))

(defun agents--handle-state-file (file)
  "Read state FILE into `agents--states'; return the new state symbol or nil."
  (let ((id (file-name-nondirectory file)))
    (when (file-exists-p file)
      (let ((state (agents--parse-state
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))))
        (cond
         ((null state) (remhash id agents--states) nil)
         ((eq state (plist-get (gethash id agents--states) :state)) state)
         (t (puthash id (list :state state :since (float-time)) agents--states)
            state))))))

(defun agents--sweep-stale-files ()
  (let ((live (delq nil (mapcar (lambda (b)
                                  (buffer-local-value 'agents--session-id b))
                                (agents--vterm-buffers)))))
    (dolist (f (directory-files agents-state-directory t "\\`[^.]"))
      (unless (member (file-name-nondirectory f) live)
        (ignore-errors (delete-file f))))))

(defun agents--after-state-change ()
  "Refresh badge, sidebar, and dashboard after the state store changed."
  (agents--update-badge)
  (agents--sidebar-render)
  (let ((buf (get-buffer "*agents*")))
    (when (and buf (get-buffer-window buf t))
      (with-current-buffer buf (revert-buffer)))))

(defun agents--notify-callback (event)
  (pcase-let ((`(,_desc ,action ,file . ,_) event))
    (when (memq action '(created changed))
      (agents--handle-state-file file)
      (agents--after-state-change))))

(defun agents--poll-state-directory ()
  (dolist (f (directory-files agents-state-directory t "\\`[^.]"))
    (agents--handle-state-file f))
  (agents--after-state-change))

(defun agents--cleanup-killed-buffer ()
  (when (and agents--session-id (derived-mode-p 'vterm-mode))
    (ignore-errors
      (delete-file (expand-file-name agents--session-id agents-state-directory)))
    (remhash agents--session-id agents--states)
    (run-at-time 0 nil #'agents--after-state-change)))

(defun agents-setup ()
  "Initialize the agents state directory, watcher, and hooks."
  (when agents--watch
    (ignore-errors (file-notify-rm-watch agents--watch))
    (setq agents--watch nil))
  (make-directory agents-state-directory t)
  (agents--sweep-stale-files)
  (add-hook 'kill-buffer-hook #'agents--cleanup-killed-buffer)
  (unless (memq 'agents--badge global-mode-string)
    (setq global-mode-string
          (append (if (listp global-mode-string)
                      global-mode-string
                    (list global-mode-string))
                  '(agents--badge))))
  (condition-case nil
      (setq agents--watch
            (file-notify-add-watch agents-state-directory '(change)
                                   #'agents--notify-callback))
    (error
     (setq agents--poll-timer
           (run-with-timer 3 3 #'agents--poll-state-directory))))
  (setq tab-bar-show nil)
  (tab-bar-mode 1)
  (agents--ensure-main-tab)
  (add-to-list 'desktop-globals-to-save 'agents--workspaces)
  (unless (memq 'agents--workspace-indicator global-mode-string)
    (setq global-mode-string
          (append global-mode-string '(agents--workspace-indicator))))
  (if (boundp 'tab-bar-tab-post-select-functions)
      (add-hook 'tab-bar-tab-post-select-functions #'agents--sidebar-sync)
    (advice-add 'tab-bar-select-tab :after #'agents--sidebar-sync))
  (agents--update-badge))

(defun agents--buffer-state (buf)
  (with-current-buffer buf
    (if (not (process-live-p (get-buffer-process buf)))
        'exited
      (or (and agents--session-id
               (plist-get (gethash agents--session-id agents--states) :state))
          'running))))

(defun agents--state-label (state)
  (pcase state
    ('waiting (propertize (format "● %s" state) 'face 'warning))
    ('done (propertize (format "✓ %s" state) 'face 'success))
    ('working (format "▶ %s" state))
    (_ (format "  %s" state))))

(defun agents--project-label (dir)
  (if-let* ((pr (ignore-errors (project-current nil dir))))
      (project-name pr)
    (abbreviate-file-name (directory-file-name dir))))

(defun agents--entry (buf)
  (with-current-buffer buf
    (let* ((state (agents--buffer-state buf))
           (info (and agents--session-id
                      (gethash agents--session-id agents--states)))
           (since (plist-get info :since))
           (proc (get-buffer-process buf)))
      (list buf
            (vector
             (agents--state-label state)
             (if info "claude"
               (agents--tool-name
                (buffer-name)
                (and proc (file-name-nondirectory
                           (car (process-command proc))))))
             (agents--project-label default-directory)
             (buffer-name)
             (if (memq state '(waiting done exited))
                 (agents--format-since (and since (- (float-time) since)))
               "—"))))))

(defun agents--dashboard-entries ()
  (mapcar #'agents--entry
          (sort (agents--vterm-buffers)
                (lambda (a b)
                  (< (agents--state-sort-key (agents--buffer-state a))
                     (agents--state-sort-key (agents--buffer-state b)))))))

(defun agents--vterm-mode-advice (orig-fun &rest args)
  (let* ((id (agents--new-session-id))
         (vterm-environment
          (cons (concat "EMACS_AGENT_SESSION=" id)
                (bound-and-true-p vterm-environment))))
    (apply orig-fun args)
    (setq-local agents--session-id id)))

(with-eval-after-load 'vterm
  (advice-add 'vterm-mode :around #'agents--vterm-mode-advice))

(defvar agents--refresh-timer nil)

(define-derived-mode agents-dashboard-mode tabulated-list-mode "Agents"
  "Overview of running vterm agent sessions."
  (setq tabulated-list-format
        [("STATE" 10 nil) ("TOOL" 8 t) ("PROJECT" 24 t)
         ("BUFFER" 28 t) ("SINCE" 6 nil)])
  (setq tabulated-list-entries #'agents--dashboard-entries)
  (tabulated-list-init-header))

(defun agents--target-buffer ()
  (let ((buf (tabulated-list-get-id)))
    (unless (buffer-live-p buf)
      (user-error "No session on this line"))
    buf))

(defun agents-open ()
  (interactive)
  (pop-to-buffer-same-window (agents--target-buffer)))

(defun agents-open-other-window ()
  (interactive)
  (pop-to-buffer (agents--target-buffer)))

(defun agents-new-session ()
  (interactive)
  (let ((starter (pcase (completing-read
                         "Tool: " '("claude" "codex" "aider" "fish") nil t)
                   ("claude" 'start-claude-vterm)
                   ("codex" 'start-codex-vterm)
                   ("aider" 'start-aider-vterm)
                   ("fish" 'start-vterm-default))))
    (if (fboundp starter)
        (call-interactively starter)
      (user-error "%s is not defined (is apps.el loaded?)" starter))))

(defun agents-kill-session ()
  (interactive)
  (let ((buf (agents--target-buffer)))
    (when (yes-or-no-p (format "Kill %s? " (buffer-name buf)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))
      (revert-buffer))))

(defun agents-rename-session ()
  (interactive)
  (let ((buf (agents--target-buffer)))
    (with-current-buffer buf
      (rename-buffer (read-string "Rename to: " (buffer-name)) t)))
  (revert-buffer))

(define-key agents-dashboard-mode-map (kbd "RET") #'agents-open)
(define-key agents-dashboard-mode-map "o" #'agents-open-other-window)
(define-key agents-dashboard-mode-map "n" #'agents-new-session)
(define-key agents-dashboard-mode-map "x" #'agents-kill-session)
(define-key agents-dashboard-mode-map "r" #'agents-rename-session)

(with-eval-after-load 'evil
  (evil-set-initial-state 'agents-dashboard-mode 'normal)
  (evil-define-key 'normal agents-dashboard-mode-map
    (kbd "RET") #'agents-open
    "o" #'agents-open-other-window
    "n" #'agents-new-session
    "x" #'agents-kill-session
    "r" #'agents-rename-session
    "gr" #'revert-buffer
    "q" #'quit-window))

(defun agents--refresh-tick ()
  (let ((buf (get-buffer "*agents*")))
    (if (and buf (get-buffer-window buf t))
        (with-current-buffer buf (revert-buffer))
      (when agents--refresh-timer
        (cancel-timer agents--refresh-timer)
        (setq agents--refresh-timer nil)))))

(defun agents-dashboard ()
  "Show an overview of all running vterm sessions."
  (interactive)
  (with-current-buffer (get-buffer-create "*agents*")
    (unless (derived-mode-p 'agents-dashboard-mode)
      (agents-dashboard-mode))
    (revert-buffer)
    (pop-to-buffer (current-buffer)))
  (unless agents--refresh-timer
    (setq agents--refresh-timer
          (run-with-timer 5 5 #'agents--refresh-tick))))

(defvar agents--badge "")
(put 'agents--badge 'risky-local-variable t)

(defun agents--waiting-buffers ()
  (cl-remove-if-not (lambda (b) (eq (agents--buffer-state b) 'waiting))
                    (agents--vterm-buffers)))

(defun agents--update-badge ()
  (let ((text (agents--badge-text (length (agents--waiting-buffers)))))
    (setq agents--badge
          (if (null text) ""
            (concat " " (propertize
                         text
                         'face 'warning
                         'mouse-face 'mode-line-highlight
                         'help-echo "Agents waiting — mouse-1: jump"
                         'local-map (let ((m (make-sparse-keymap)))
                                      (define-key m [mode-line mouse-1]
                                                  #'agents-jump-to-waiting)
                                      m)))))
    (force-mode-line-update t)))

(defun agents-jump-to-waiting ()
  "Cycle through vterm sessions waiting for input."
  (interactive)
  (let ((bufs (agents--waiting-buffers)))
    (if (null bufs)
        (message "No agents waiting")
      (let ((pos (cl-position (current-buffer) bufs)))
        (switch-to-buffer
         (nth (mod (1+ (or pos -1)) (length bufs)) bufs))))))

(defconst agents--claude-hook-events
  '(("UserPromptSubmit" . "working")
    ("PostToolUse" . "working")
    ("Notification" . "waiting")
    ("Stop" . "done")))

(defconst agents--claude-hook-command-format
  (concat "[ -n \"$EMACS_AGENT_SESSION\" ] && "
          "echo %s > \"$HOME/.cache/emacs-agents/$EMACS_AGENT_SESSION\" "
          "|| true"))

(defun agents--merged-claude-hooks (json)
  "Return settings alist JSON with the agents hook entries merged in.
Arrays are vectors, as produced by `json-parse-buffer'.  Idempotent."
  (let ((hooks (alist-get 'hooks json)))
    (dolist (pair agents--claude-hook-events)
      (let* ((event (intern (car pair)))
             (cmd (format agents--claude-hook-command-format (cdr pair)))
             (entries (alist-get event hooks))
             (present
              (cl-some
               (lambda (entry)
                 (cl-some (lambda (h)
                            (string-match-p "emacs-agents"
                                            (or (alist-get 'command h) "")))
                          (append (alist-get 'hooks entry) nil)))
               (append entries nil))))
        (unless present
          (setf (alist-get event hooks)
                (vconcat entries
                         (vector
                          `((hooks . ,(vector `((type . "command")
                                                (command . ,cmd)))))))))))
    (setf (alist-get 'hooks json) hooks)
    json))

(defun agents-install-claude-hooks ()
  "Merge the agents state hooks into ~/.claude/settings.json, with confirmation."
  (interactive)
  (let* ((file (expand-file-name "~/.claude/settings.json"))
         (json (when (file-exists-p file)
                 (with-temp-buffer
                   (insert-file-contents file)
                   (condition-case nil
                       (json-parse-buffer :object-type 'alist
                                          :array-type 'array)
                     (error (user-error
                             "%s does not parse; merge hooks manually" file))))))
         (merged (agents--merged-claude-hooks (copy-tree json t))))
    (if (equal json merged)
        (message "Claude hooks already installed")
      (with-current-buffer (get-buffer-create "*agents-claude-hooks*")
        (erase-buffer)
        (insert (json-serialize merged))
        (json-pretty-print-buffer)
        (display-buffer (current-buffer)))
      (when (yes-or-no-p (format "Write this to %s? " file))
        (when (file-exists-p file)
          (copy-file file (concat file ".bak-"
                                  (format-time-string "%Y%m%d%H%M%S"))))
        (with-temp-file file
          (insert (json-serialize merged))
          (json-pretty-print-buffer))
        (message "Hooks installed; restart running claude sessions to apply")))))

(require 'tab-bar)

(defvar agents-workspace-main-name "main")
(defvar agents-sidebar-width 28)
(defvar agents--sidebar-visible nil)

(defvar agents--workspace-indicator "")
(put 'agents--workspace-indicator 'risky-local-variable t)

(defvar agents--workspaces nil
  "Alist of workspace NAME to root DIRECTORY, in creation order.")

(defun agents--workspace-tab-name (name)
  (concat "ws:" name))

(defun agents--workspace-name (tab-name)
  (and (string-prefix-p "ws:" tab-name)
       (substring tab-name (length "ws:"))))

(defun agents--plain-tab-p (tab-name)
  (not (or (agents--workspace-name tab-name)
           (string-prefix-p "agent:" tab-name))))

(defun agents--tab-names ()
  (mapcar (lambda (tab) (alist-get 'name tab))
          (funcall tab-bar-tabs-function)))

(defun agents--workspace-root (name)
  (alist-get name agents--workspaces nil nil #'equal))

(defun agents--workspace-state (root)
  "Most urgent hook state among sessions rooted under ROOT, or nil."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (states
          (delq nil
                (mapcar
                 (lambda (b)
                   (let ((state (agents--buffer-state b))
                         (dir (expand-file-name
                               (buffer-local-value 'default-directory b))))
                     (and (string-prefix-p root (file-name-as-directory dir))
                          (memq state '(waiting done working))
                          state)))
                 (agents--vterm-buffers)))))
    (car (sort states (lambda (a b) (< (agents--state-sort-key a)
                                       (agents--state-sort-key b)))))))

(defun agents--ensure-main-tab ()
  "Name the first auto-named plain tab main, unless a main tab exists."
  (when (bound-and-true-p tab-bar-mode)
    (let ((tabs (funcall tab-bar-tabs-function)))
      (unless (member agents-workspace-main-name
                      (mapcar (lambda (tab) (alist-get 'name tab)) tabs))
        (when-let* ((tab (cl-find-if
                          (lambda (tab)
                            (and (agents--plain-tab-p (alist-get 'name tab))
                                 (not (alist-get 'explicit-name tab))))
                          tabs)))
          (tab-bar-rename-tab-by-name (alist-get 'name tab)
                                      agents-workspace-main-name))))))

(defun agents--ensure-tab-bar ()
  (unless tab-bar-mode
    (setq tab-bar-show nil)
    (tab-bar-mode 1))
  (agents--ensure-main-tab))

(defun agents--update-workspace-indicator ()
  (let* ((name (and (bound-and-true-p tab-bar-mode)
                    (alist-get 'name (tab-bar--current-tab))))
         (label (and name (agents--workspace-name name))))
    (setq agents--workspace-indicator (if label (format " [%s]" label) ""))
    (force-mode-line-update t)))

(defun agents--workspace-goto (name)
  "Switch to workspace NAME, creating its tab (landing in dired) if needed."
  (agents--ensure-tab-bar)
  (let ((tab-name (agents--workspace-tab-name name)))
    (if (member tab-name (agents--tab-names))
        (tab-bar-switch-to-tab tab-name)
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (delete-other-windows)
      (dired (or (agents--workspace-root name) default-directory))))
  (agents--sidebar-sync))

(defun agents-workspace-create (name directory)
  "Register workspace NAME rooted at DIRECTORY.
It appears in the sidebar; switch to it from there or with
`agents-workspace-switch'.  Callable from Lisp to build variant
commands that compute DIRECTORY first (see Commentary)."
  (interactive
   (list (read-string "Workspace name: ")
         (read-directory-name
          "Root directory: "
          (or (when-let* ((pr (project-current))) (project-root pr))
              default-directory))))
  (when (string-empty-p name)
    (user-error "Workspace name cannot be empty"))
  (when (assoc name agents--workspaces)
    (user-error "Workspace %s already exists" name))
  (setq agents--workspaces
        (append agents--workspaces
                (list (cons name (file-name-as-directory
                                  (expand-file-name directory))))))
  (agents--sidebar-sync)
  (message "Workspace %s created; switch to it from the sidebar" name)
  name)

(defun agents-workspace-next ()
  (interactive)
  (agents--ensure-tab-bar)
  (tab-bar-switch-to-next-tab)
  (agents--sidebar-sync))

(defun agents-workspace-prev ()
  (interactive)
  (agents--ensure-tab-bar)
  (tab-bar-switch-to-prev-tab)
  (agents--sidebar-sync))

(defun agents--current-workspace-name ()
  "Name of the active workspace: registered name, tab name, or main."
  (let ((tab (and (bound-and-true-p tab-bar-mode)
                  (alist-get 'name (tab-bar--current-tab)))))
    (or (and tab (agents--workspace-name tab))
        tab
        agents-workspace-main-name)))

(defun agents--workspace-claude-buffer-name (workspace)
  (format "*%s:claude*" workspace))

(defun agents-workspace-claude ()
  "Start or switch to the claude instance of the active workspace.
Like `start-claude-vterm', but the buffer name is prefixed with the
workspace name, so each workspace gets its own claude instance.
Starts in the workspace root when the workspace is registered."
  (interactive)
  (let* ((workspace (agents--current-workspace-name))
         (buffer-name (agents--workspace-claude-buffer-name workspace))
         (existing (get-buffer buffer-name)))
    (if existing
        (switch-to-buffer existing)
      (let ((default-directory
             (or (agents--workspace-root workspace)
                 (when-let* ((pr (project-current))) (project-root pr))
                 default-directory)))
        (with-current-buffer (vterm t)
          (vterm-send-string "claude")
          (vterm-send-return)
          (rename-buffer buffer-name t)
          (when (fboundp 'vterm-app-mode)
            (vterm-app-mode 1)))))))

(defun agents-workspace-switch ()
  "Pick a workspace: plain tabs (main etc.) or a registered workspace."
  (interactive)
  (agents--ensure-tab-bar)
  (let* ((plain (cl-remove-if-not #'agents--plain-tab-p (agents--tab-names)))
         (candidates (append plain (mapcar #'car agents--workspaces)))
         (choice (completing-read "Workspace: " candidates nil t)))
    (if (assoc choice agents--workspaces)
        (agents--workspace-goto choice)
      (tab-bar-switch-to-tab choice)
      (agents--sidebar-sync))))

;; Sidebar

(define-derived-mode agents-sidebar-mode special-mode "AgentsSide"
  "Sidebar listing agent workspaces."
  (setq truncate-lines t)
  (setq cursor-in-non-selected-windows nil))

(defun agents--sidebar-rows ()
  "Return (LABEL STATE TARGET) rows: plain tabs first, then workspaces."
  (let* ((tabs (and (bound-and-true-p tab-bar-mode) (agents--tab-names)))
         (plain (or (cl-remove-if-not #'agents--plain-tab-p tabs)
                    (list agents-workspace-main-name))))
    (append
     (mapcar (lambda (name) (list name nil (cons 'tab name))) plain)
     (mapcar (lambda (ws)
               (list (car ws) (agents--workspace-state (cdr ws))
                     (cons 'ws (car ws))))
             agents--workspaces))))

(defvar-local agents--sidebar-marks nil
  "Marked sidebar targets, in the same form as the agents-target property.")

(defun agents--sidebar-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun agents--sidebar-render ()
  (when-let* ((buf (get-buffer "*agents-sidebar*")))
    (with-current-buffer buf
      (setq agents--sidebar-marks
            (cl-remove-if (lambda (target)
                            (pcase target
                              (`(ws . ,name)
                               (not (assoc name agents--workspaces)))
                              (_ nil)))
                          agents--sidebar-marks))
      (let* ((inhibit-read-only t)
             (line (line-number-at-pos))
             (window-lines
              (mapcar (lambda (w)
                        (cons w (line-number-at-pos (window-point w))))
                      (get-buffer-window-list buf nil t)))
             (current (and (bound-and-true-p tab-bar-mode)
                           (alist-get 'name (tab-bar--current-tab)))))
        (erase-buffer)
        (insert (propertize "workspaces\n" 'face 'mode-line-emphasis))
        (dolist (row (agents--sidebar-rows))
          (pcase-let ((`(,label ,state ,target) row))
            (let* ((currentp (pcase target
                               (`(tab . ,name) (equal name current))
                               (`(ws . ,name)
                                (equal (agents--workspace-tab-name name)
                                       current))))
                   (markedp (member target agents--sidebar-marks))
                   (icon (pcase state
                           ('waiting (propertize "●" 'face 'warning))
                           ('done (propertize "✓" 'face 'success))
                           ('working "▶")
                           ('exited "✗")
                           (_ " "))))
              (insert (propertize
                       (format "%s%s %s %s\n"
                               (if markedp
                                   (propertize "*" 'face 'warning)
                                 " ")
                               (if currentp ">" " ") icon label)
                       'agents-target target
                       'face (and currentp 'highlight)
                       'mouse-face 'highlight)))))
        (agents--sidebar-goto-line line)
        (dolist (wl window-lines)
          (set-window-point (car wl)
                            (save-excursion
                              (agents--sidebar-goto-line (cdr wl))
                              (point))))))))

(defun agents--sidebar-show ()
  (let ((buf (get-buffer-create "*agents-sidebar*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'agents-sidebar-mode)
        (agents-sidebar-mode)))
    (display-buffer-in-side-window
     buf `((side . left) (slot . 0)
           (window-width . ,agents-sidebar-width)
           (window-parameters . ((no-delete-other-windows . t)))))
    (agents--sidebar-render)))

(defun agents--sidebar-sync (&rest _)
  (agents--ensure-main-tab)
  (agents--update-workspace-indicator)
  (if agents--sidebar-visible
      (agents--sidebar-show)
    (when-let* ((win (get-buffer-window "*agents-sidebar*")))
      (delete-window win)))
  (agents--sidebar-render))

(defun agents-sidebar-toggle ()
  "Toggle the workspace sidebar."
  (interactive)
  (setq agents--sidebar-visible (not agents--sidebar-visible))
  (agents--sidebar-sync))

(defun agents--select-non-side-window ()
  (when-let* ((win (cl-find-if-not
                    (lambda (w) (window-parameter w 'window-side))
                    (window-list))))
    (select-window win)))

(defun agents--sidebar-act ()
  "Switch to the workspace on the current sidebar line; return non-nil if acted."
  (when-let* ((target (get-text-property (point) 'agents-target)))
    (pcase target
      (`(tab . ,name)
       (agents--ensure-tab-bar)
       (unless (equal name (alist-get 'name (tab-bar--current-tab)))
         (tab-bar-switch-to-tab name))
       (agents--sidebar-sync))
      (`(ws . ,name) (agents--workspace-goto name)))
    t))

(defun agents-sidebar-select ()
  "Switch to the workspace on the current sidebar line."
  (interactive)
  (when (agents--sidebar-act)
    (agents--select-non-side-window)))

(defun agents-sidebar-open ()
  "Open the workspace at point, keeping focus in the sidebar."
  (interactive)
  (let ((line (line-number-at-pos)))
    (when (agents--sidebar-act)
      (when-let* ((win (get-buffer-window "*agents-sidebar*")))
        (select-window win)
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun agents-sidebar-mouse-select (event)
  (interactive "e")
  (mouse-set-point event)
  (agents-sidebar-select))

(defun agents--sidebar-target-at-point ()
  (get-text-property (point) 'agents-target))

(defun agents-sidebar-mark ()
  "Mark the workspace at point and move to the next line."
  (interactive)
  (when-let* ((target (agents--sidebar-target-at-point)))
    (cl-pushnew target agents--sidebar-marks :test #'equal)
    (agents--sidebar-render))
  (forward-line 1))

(defun agents-sidebar-unmark ()
  "Unmark the workspace at point and move to the next line."
  (interactive)
  (when-let* ((target (agents--sidebar-target-at-point)))
    (setq agents--sidebar-marks
          (cl-remove target agents--sidebar-marks :test #'equal))
    (agents--sidebar-render))
  (forward-line 1))

(defun agents-sidebar-unmark-all ()
  "Remove all sidebar marks."
  (interactive)
  (setq agents--sidebar-marks nil)
  (agents--sidebar-render))

(defun agents-sidebar-rename ()
  "Rename the workspace at point."
  (interactive)
  (pcase (agents--sidebar-target-at-point)
    (`(ws . ,name)
     (let ((new (read-string (format "Rename %s to: " name) name)))
       (when (assoc new agents--workspaces)
         (user-error "Workspace %s already exists" new))
       (setq agents--workspaces
             (mapcar (lambda (ws)
                       (if (equal (car ws) name) (cons new (cdr ws)) ws))
                     agents--workspaces))
       (when (member (agents--workspace-tab-name name) (agents--tab-names))
         (tab-bar-rename-tab-by-name (agents--workspace-tab-name name)
                                     (agents--workspace-tab-name new))))
     (agents--sidebar-sync))
    (`(tab . ,name)
     (tab-bar-rename-tab-by-name
      name (read-string (format "Rename %s to: " name) name))
     (agents--sidebar-sync))
    (_ (user-error "No workspace on this line"))))

(defun agents-sidebar-delete ()
  "Delete marked workspaces, or the one at point when none are marked.
Session workspaces are killed; plain tabs are closed."
  (interactive)
  (let* ((targets (or agents--sidebar-marks
                      (when-let* ((target (agents--sidebar-target-at-point)))
                        (list target))))
         (labels (mapcar (lambda (target)
                           (pcase target
                             (`(ws . ,name) name)
                             (`(tab . ,name) name)))
                         targets)))
    (unless targets (user-error "No workspace on this line"))
    (when (yes-or-no-p (format "Delete %d workspace%s (%s)? "
                               (length targets)
                               (if (cdr targets) "s" "")
                               (string-join labels ", ")))
      (dolist (target targets)
        (pcase target
          (`(ws . ,name)
           (setq agents--workspaces (assoc-delete-all name agents--workspaces))
           (let ((tab-name (agents--workspace-tab-name name))
                 (names (agents--tab-names)))
             (when (and (member tab-name names) (> (length names) 1))
               (ignore-errors (tab-bar-close-tab-by-name tab-name)))))
          (`(tab . ,name)
           (let ((names (agents--tab-names)))
             (when (and (member name names) (> (length names) 1))
               (ignore-errors (tab-bar-close-tab-by-name name)))))))
      (setq agents--sidebar-marks nil)
      (agents--sidebar-sync))))

(define-key agents-sidebar-mode-map (kbd "RET") #'agents-sidebar-select)
(define-key agents-sidebar-mode-map [mouse-1] #'agents-sidebar-mouse-select)
(define-key agents-sidebar-mode-map "o" #'agents-sidebar-open)
(define-key agents-sidebar-mode-map "q" #'agents-sidebar-toggle)
(define-key agents-sidebar-mode-map "R" #'agents-sidebar-rename)
(define-key agents-sidebar-mode-map "m" #'agents-sidebar-mark)
(define-key agents-sidebar-mode-map "u" #'agents-sidebar-unmark)
(define-key agents-sidebar-mode-map "U" #'agents-sidebar-unmark-all)
(define-key agents-sidebar-mode-map "D" #'agents-sidebar-delete)
(with-eval-after-load 'evil
  (evil-set-initial-state 'agents-sidebar-mode 'normal)
  (evil-define-key 'normal agents-sidebar-mode-map
    (kbd "RET") #'agents-sidebar-select
    "o" #'agents-sidebar-open
    "q" #'agents-sidebar-toggle
    "R" #'agents-sidebar-rename
    "m" #'agents-sidebar-mark
    "u" #'agents-sidebar-unmark
    "U" #'agents-sidebar-unmark-all
    "D" #'agents-sidebar-delete))

(unless noninteractive (agents-setup))

(provide 'agents)
;;; agents.el ends here
