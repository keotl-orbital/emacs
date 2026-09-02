;;; elmux.el --- Overview of vterm agent sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; Workspaces are created intentionally with `elmux-workspace-create',
;; which registers a NAME -> root DIRECTORY pair.  They appear in the
;; sidebar (`elmux-sidebar-toggle') next to `main'; switching to one
;; creates its tab lazily, landing in a dired of the root.  A
;; workspace's sidebar icon reflects the most urgent Claude hook state
;; among vterm sessions whose `default-directory' is under its root.
;;
;; Build variants of the create command by computing a directory and
;; calling `elmux-workspace-create' from Lisp.  Example: create a git
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
;;       (elmux-workspace-create branch dir)))
;;
;;   ;; ~/Documents/project100x/.dir-locals.el
;;   ((nil . ((my-worktree-directory . ".worktrees/"))))
;;
;; The dir-local takes effect when the command is invoked from a buffer
;; inside that project (the defvar provides the default elsewhere).
;;
;; Workspaces persist through `desktop-save-mode': the registry rides in
;; `desktop-globals-to-save', tabs and layouts in desktop's frameset,
;; and workspace app vterms (*WORKSPACE:APP*) through a desktop buffer
;; handler that recreates them in their root and reruns the command from
;; `elmux-workspace-restore-commands' (claude resumes via --continue;
;; set `elmux-desktop-relaunch-agents' to nil for dormant shells).
;; desktop-save-mode must be enabled for any of this to happen.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'filenotify)
(require 'json)
(require 'project)
(require 'desktop)

(defvar vterm-environment)

(defvar elmux-state-directory (expand-file-name "~/.cache/elmux/")
  "Directory where Claude Code hooks write per-session state files.")

(defvar elmux--states (make-hash-table :test 'equal)
  "Map of session-id to plist (:state SYMBOL :since FLOAT).")

(defvar-local elmux--session-id nil
  "Unique id of this vterm session, also its state file name.")

(defun elmux--parse-state (content)
  "Return state symbol from state-file CONTENT, or nil if unrecognized."
  (let ((s (string-trim (or content ""))))
    (when (member s '("working" "waiting" "done"))
      (intern s))))

(defun elmux--state-sort-key (state)
  (or (cl-position state '(waiting done working running exited)) 5))

(defun elmux--tool-name (buffer-name fallback)
  "Derive a tool label from BUFFER-NAME (e.g. \"*claude:proj*<2>\" -> \"claude\").
Use FALLBACK (typically the process name) for generic \"*vterm*\" buffers."
  (let ((base (and (string-match "\\`\\*\\([^:*]+\\)" buffer-name)
                   (match-string 1 buffer-name))))
    (if (or (null base) (string= base "vterm"))
        (or fallback base "?")
      base)))

(defun elmux--format-since (seconds)
  (cond ((null seconds) "—")
        ((< seconds 60) (format "%ds" (floor seconds)))
        ((< seconds 3600) (format "%dm" (floor seconds 60)))
        (t (format "%dh" (floor seconds 3600)))))

(defun elmux--badge-text (n)
  (when (> n 0) (format "●%d" n)))

(defvar elmux--session-counter 0)
(defvar elmux--watch nil)
(defvar elmux--poll-timer nil)

(defun elmux--new-session-id ()
  (format "%d-%d-%d" (emacs-pid) (floor (float-time))
          (cl-incf elmux--session-counter)))

(defun elmux--vterm-buffers ()
  (cl-remove-if-not
   (lambda (b) (provided-mode-derived-p
                (buffer-local-value 'major-mode b) 'vterm-mode))
   (buffer-list)))

(defun elmux--handle-state-file (file)
  "Read state FILE into `elmux--states'; return the new state symbol or nil."
  (let ((id (file-name-nondirectory file)))
    (when (file-exists-p file)
      (let ((state (elmux--parse-state
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))))
        (cond
         ((null state) (remhash id elmux--states) nil)
         ((eq state (plist-get (gethash id elmux--states) :state)) state)
         (t (puthash id (list :state state :since (float-time)) elmux--states)
            state))))))

(defun elmux--sweep-stale-files ()
  (let ((live (delq nil (mapcar (lambda (b)
                                  (buffer-local-value 'elmux--session-id b))
                                (elmux--vterm-buffers)))))
    (dolist (f (directory-files elmux-state-directory t "\\`[^.]"))
      (unless (member (file-name-nondirectory f) live)
        (ignore-errors (delete-file f))))))

(defun elmux--after-state-change ()
  "Refresh badge, sidebar, and dashboard after the state store changed."
  (elmux--update-badge)
  (elmux--sidebar-render)
  (let ((buf (get-buffer "*elmux*")))
    (when (and buf (get-buffer-window buf t))
      (with-current-buffer buf (revert-buffer)))))

(defvar elmux--file-watches (make-hash-table :test 'equal)
  "Map of state-file path to its file-notify descriptor.
kqueue only reports directory-entry changes for the directory watch, so
each state file gets its own watch to catch content overwrites.")

(defun elmux--watch-state-file (file)
  (when (and (not (gethash file elmux--file-watches))
             (file-exists-p file))
    (ignore-errors
      (puthash file
               (file-notify-add-watch file '(change)
                                      #'elmux--file-notify-callback)
               elmux--file-watches))))

(defun elmux--unwatch-state-file (file)
  (when-let* ((desc (gethash file elmux--file-watches)))
    (ignore-errors (file-notify-rm-watch desc))
    (remhash file elmux--file-watches)))

(defun elmux--file-notify-callback (event)
  (pcase-let ((`(,_desc ,action ,file . ,_) event))
    (pcase action
      ('changed
       (elmux--handle-state-file file)
       (elmux--after-state-change))
      ((or 'deleted 'renamed 'stopped)
       (elmux--unwatch-state-file file)))))

(defun elmux--notify-callback (event)
  (pcase-let ((`(,_desc ,action ,file . ,_) event))
    (when (memq action '(created changed))
      (elmux--handle-state-file file)
      (elmux--watch-state-file file)
      (elmux--after-state-change))))

(defun elmux--poll-state-directory ()
  (dolist (f (directory-files elmux-state-directory t "\\`[^.]"))
    (elmux--handle-state-file f))
  (elmux--after-state-change))

(defun elmux--cleanup-killed-buffer ()
  (when (and elmux--session-id (derived-mode-p 'vterm-mode))
    (let ((file (expand-file-name elmux--session-id elmux-state-directory)))
      (elmux--unwatch-state-file file)
      (ignore-errors (delete-file file)))
    (remhash elmux--session-id elmux--states)
    (run-at-time 0 nil #'elmux--after-state-change)))

(defun elmux-setup ()
  "Initialize the elmux state directory, watcher, and hooks."
  (when elmux--watch
    (ignore-errors (file-notify-rm-watch elmux--watch))
    (setq elmux--watch nil))
  (maphash (lambda (_file desc) (ignore-errors (file-notify-rm-watch desc)))
           elmux--file-watches)
  (clrhash elmux--file-watches)
  (make-directory elmux-state-directory t)
  (elmux--sweep-stale-files)
  (add-hook 'kill-buffer-hook #'elmux--cleanup-killed-buffer)
  (add-hook 'vterm-mode-hook #'elmux--desktop-mark-buffer)
  (add-to-list 'desktop-buffer-mode-handlers
               (cons 'vterm-mode #'elmux--desktop-restore-buffer))
  (unless (memq 'elmux--badge global-mode-string)
    (setq global-mode-string
          (append (if (listp global-mode-string)
                      global-mode-string
                    (list global-mode-string))
                  '(elmux--badge))))
  (condition-case nil
      (progn
        (setq elmux--watch
              (file-notify-add-watch elmux-state-directory '(change)
                                     #'elmux--notify-callback))
        (dolist (f (directory-files elmux-state-directory t "\\`[^.]"))
          (elmux--watch-state-file f)))
    (error
     (setq elmux--poll-timer
           (run-with-timer 3 3 #'elmux--poll-state-directory))))
  (setq tab-bar-show nil)
  (tab-bar-mode 1)
  (elmux--ensure-main-tab)
  (add-to-list 'desktop-globals-to-save 'elmux--workspaces)
  (unless (memq 'elmux--workspace-indicator global-mode-string)
    (setq global-mode-string
          (append global-mode-string '(elmux--workspace-indicator))))
  (if (boundp 'tab-bar-tab-post-select-functions)
      (add-hook 'tab-bar-tab-post-select-functions #'elmux--sidebar-sync)
    (advice-add 'tab-bar-select-tab :after #'elmux--sidebar-sync))
  (elmux--update-badge))

(defun elmux--buffer-state (buf)
  (with-current-buffer buf
    (if (not (process-live-p (get-buffer-process buf)))
        'exited
      (or (and elmux--session-id
               (plist-get (gethash elmux--session-id elmux--states) :state))
          'running))))

(defun elmux--state-label (state)
  (pcase state
    ('waiting (propertize (format "● %s" state) 'face 'warning))
    ('done (propertize (format "✓ %s" state) 'face 'success))
    ('working (format "▶ %s" state))
    (_ (format "  %s" state))))

(defun elmux--project-label (dir)
  (if-let* ((pr (ignore-errors (project-current nil dir))))
      (project-name pr)
    (abbreviate-file-name (directory-file-name dir))))

(defun elmux--entry (buf)
  (with-current-buffer buf
    (let* ((state (elmux--buffer-state buf))
           (info (and elmux--session-id
                      (gethash elmux--session-id elmux--states)))
           (since (plist-get info :since))
           (proc (get-buffer-process buf)))
      (list buf
            (vector
             (elmux--state-label state)
             (if info "claude"
               (elmux--tool-name
                (buffer-name)
                (and proc (file-name-nondirectory
                           (car (process-command proc))))))
             (elmux--project-label default-directory)
             (buffer-name)
             (if (memq state '(waiting done exited))
                 (elmux--format-since (and since (- (float-time) since)))
               "—"))))))

(defun elmux--dashboard-entries ()
  (mapcar #'elmux--entry
          (sort (elmux--vterm-buffers)
                (lambda (a b)
                  (< (elmux--state-sort-key (elmux--buffer-state a))
                     (elmux--state-sort-key (elmux--buffer-state b)))))))

(defun elmux--vterm-mode-advice (orig-fun &rest args)
  (let* ((id (elmux--new-session-id))
         (vterm-environment
          (cons (concat "ELMUX_SESSION=" id)
                (bound-and-true-p vterm-environment))))
    (apply orig-fun args)
    (setq-local elmux--session-id id)))

(with-eval-after-load 'vterm
  (advice-add 'vterm-mode :around #'elmux--vterm-mode-advice))

(defvar elmux--refresh-timer nil)

(define-derived-mode elmux-dashboard-mode tabulated-list-mode "Elmux"
  "Overview of running vterm agent sessions."
  (setq tabulated-list-format
        [("STATE" 10 nil) ("TOOL" 8 t) ("PROJECT" 24 t)
         ("BUFFER" 28 t) ("SINCE" 6 nil)])
  (setq tabulated-list-entries #'elmux--dashboard-entries)
  (tabulated-list-init-header))

(defun elmux--target-buffer ()
  (let ((buf (tabulated-list-get-id)))
    (unless (buffer-live-p buf)
      (user-error "No session on this line"))
    buf))

(defun elmux-open ()
  (interactive)
  (pop-to-buffer-same-window (elmux--target-buffer)))

(defun elmux-open-other-window ()
  (interactive)
  (pop-to-buffer (elmux--target-buffer)))

(defun elmux-new-session ()
  (interactive)
  (let ((starter (pcase (completing-read
                         "Tool: " '("claude" "codex" "opencode" "aider" "fish") nil t)
                   ("claude" 'start-claude-vterm)
                   ("codex" 'start-codex-vterm)
                   ("opencode" 'start-opencode-vterm)
                   ("aider" 'start-aider-vterm)
                   ("fish" 'start-vterm-default))))
    (if (fboundp starter)
        (call-interactively starter)
      (user-error "%s is not defined (is apps.el loaded?)" starter))))

(defun elmux-kill-session ()
  (interactive)
  (let ((buf (elmux--target-buffer)))
    (when (yes-or-no-p (format "Kill %s? " (buffer-name buf)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))
      (revert-buffer))))

(defun elmux-rename-session ()
  (interactive)
  (let ((buf (elmux--target-buffer)))
    (with-current-buffer buf
      (rename-buffer (read-string "Rename to: " (buffer-name)) t)))
  (revert-buffer))

(define-key elmux-dashboard-mode-map (kbd "RET") #'elmux-open)
(define-key elmux-dashboard-mode-map "o" #'elmux-open-other-window)
(define-key elmux-dashboard-mode-map "n" #'elmux-new-session)
(define-key elmux-dashboard-mode-map "x" #'elmux-kill-session)
(define-key elmux-dashboard-mode-map "r" #'elmux-rename-session)

(with-eval-after-load 'evil
  (evil-set-initial-state 'elmux-dashboard-mode 'normal)
  (evil-define-key 'normal elmux-dashboard-mode-map
    (kbd "RET") #'elmux-open
    "o" #'elmux-open-other-window
    "n" #'elmux-new-session
    "x" #'elmux-kill-session
    "r" #'elmux-rename-session
    "gr" #'revert-buffer
    "q" #'quit-window))

(defun elmux--refresh-tick ()
  (let ((buf (get-buffer "*elmux*")))
    (if (and buf (get-buffer-window buf t))
        (with-current-buffer buf (revert-buffer))
      (when elmux--refresh-timer
        (cancel-timer elmux--refresh-timer)
        (setq elmux--refresh-timer nil)))))

(defun elmux-dashboard ()
  "Show an overview of all running vterm sessions."
  (interactive)
  (with-current-buffer (get-buffer-create "*elmux*")
    (unless (derived-mode-p 'elmux-dashboard-mode)
      (elmux-dashboard-mode))
    (revert-buffer)
    (pop-to-buffer (current-buffer)))
  (unless elmux--refresh-timer
    (setq elmux--refresh-timer
          (run-with-timer 5 5 #'elmux--refresh-tick))))

(defvar elmux--badge "")
(put 'elmux--badge 'risky-local-variable t)

(defun elmux--waiting-buffers ()
  (cl-remove-if-not (lambda (b) (eq (elmux--buffer-state b) 'waiting))
                    (elmux--vterm-buffers)))

(defun elmux--update-badge ()
  (let ((text (elmux--badge-text (length (elmux--waiting-buffers)))))
    (setq elmux--badge
          (if (null text) ""
            (concat " " (propertize
                         text
                         'face 'warning
                         'mouse-face 'mode-line-highlight
                         'help-echo "Agents waiting — mouse-1: jump"
                         'local-map (let ((m (make-sparse-keymap)))
                                      (define-key m [mode-line mouse-1]
                                                  #'elmux-jump-to-waiting)
                                      m)))))
    (force-mode-line-update t)))

(defun elmux-jump-to-waiting ()
  "Cycle through vterm sessions waiting for input."
  (interactive)
  (let ((bufs (elmux--waiting-buffers)))
    (if (null bufs)
        (message "No agents waiting")
      (let ((pos (cl-position (current-buffer) bufs)))
        (switch-to-buffer
         (nth (mod (1+ (or pos -1)) (length bufs)) bufs))))))

(defconst elmux--claude-hook-events
  '(("UserPromptSubmit" "working")
    ("PostToolUse" "working")
    ("Notification" "waiting")
    ("PermissionRequest" "waiting")
    ("PreToolUse" "waiting" "AskUserQuestion|ExitPlanMode")
    ("Stop" "done"))
  "Hook specs as (EVENT STATE &optional MATCHER).")

(defconst elmux--codex-hook-events
  '(("UserPromptSubmit" "working")
    ("PostToolUse" "working")
    ("PermissionRequest" "waiting")
    ("Stop" "done")))

(defconst elmux--claude-hook-command-format
  (concat "[ -n \"$ELMUX_SESSION\" ] && "
          "echo %s > \"$HOME/.cache/elmux/$ELMUX_SESSION\" "
          "|| true"))

(defun elmux--hook-entry-matches-p (entry regexp)
  (cl-some (lambda (h)
             (string-match-p regexp (or (alist-get 'command h) "")))
           (append (alist-get 'hooks entry) nil)))

(defun elmux--merged-hooks (json events)
  "Return settings alist JSON with the elmux hook EVENTS merged in.
Arrays are vectors, as produced by `json-parse-buffer'.  Idempotent;
entries from the pre-rename emacs-agents layout are dropped."
  (let ((hooks (alist-get 'hooks json)))
    (dolist (spec events)
      (let* ((event (intern (nth 0 spec)))
             (cmd (format elmux--claude-hook-command-format (nth 1 spec)))
             (matcher (nth 2 spec))
             (entries (cl-remove-if
                       (lambda (entry)
                         (elmux--hook-entry-matches-p entry "emacs-agents"))
                       (append (alist-get event hooks) nil)))
             (present (cl-some (lambda (entry)
                                 (elmux--hook-entry-matches-p entry "\\.cache/elmux"))
                               entries)))
        (setf (alist-get event hooks) (vconcat entries))
        (unless present
          (setf (alist-get event hooks)
                (vconcat entries
                         (vector
                          (append
                           (when matcher `((matcher . ,matcher)))
                           `((hooks . ,(vector `((type . "command")
                                                 (command . ,cmd))))))))))))
    (setf (alist-get 'hooks json) hooks)
    json))

(defun elmux--merged-claude-hooks (json)
  (elmux--merged-hooks json elmux--claude-hook-events))

(defun elmux--install-hooks (file events)
  "Merge the elmux hook EVENTS into the JSON hooks FILE, with confirmation."
  (let* ((json (when (file-exists-p file)
                 (with-temp-buffer
                   (insert-file-contents file)
                   (condition-case nil
                       (json-parse-buffer :object-type 'alist
                                          :array-type 'array)
                     (error (user-error
                             "%s does not parse; merge hooks manually" file))))))
         (merged (elmux--merged-hooks (copy-tree json t) events)))
    (if (equal json merged)
        (message "Claude hooks already installed")
      (with-current-buffer (get-buffer-create "*elmux-claude-hooks*")
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
        (message "Hooks installed into %s; restart running sessions to apply"
                 file)))))

(defun elmux-install-claude-hooks ()
  "Merge the elmux state hooks into ~/.claude/settings.json, with confirmation."
  (interactive)
  (elmux--install-hooks (expand-file-name "~/.claude/settings.json")
                         elmux--claude-hook-events))

(defun elmux-install-codex-hooks ()
  "Merge the elmux state hooks into ~/.codex/hooks.json, with confirmation.
Codex requires trusting new hooks: it will ask on its next interactive
run (its state icon reports via PermissionRequest instead of
Notification, otherwise identically to claude)."
  (interactive)
  (elmux--install-hooks (expand-file-name "~/.codex/hooks.json")
                         elmux--codex-hook-events))

(defvar elmux-opencode-plugin-source
  (expand-file-name "elmux-opencode-plugin.js" user-emacs-directory)
  "The elmux opencode status plugin shipped alongside elmux.el.")

(defvar elmux-opencode-plugin-target
  (expand-file-name "~/.config/opencode/plugins/elmux-status.js")
  "Where the status plugin is installed; opencode auto-loads this directory.")

(defun elmux-install-opencode-hooks ()
  "Install the elmux status plugin into ~/.config/opencode/plugins/.
Opencode has no JSON hooks file like claude and codex, so state is
reported from a plugin.  Files in the plugins directory are auto-loaded
at startup, so no opencode.jsonc entry is needed."
  (interactive)
  (unless (file-exists-p elmux-opencode-plugin-source)
    (user-error "Plugin source missing: %s" elmux-opencode-plugin-source))
  (let ((source (with-temp-buffer
                  (insert-file-contents elmux-opencode-plugin-source)
                  (buffer-string)))
        (target elmux-opencode-plugin-target))
    (if (and (file-exists-p target)
             (string= source (with-temp-buffer
                               (insert-file-contents target)
                               (buffer-string))))
        (message "Opencode plugin already installed")
      (with-current-buffer (get-buffer-create "*elmux-opencode-plugin*")
        (erase-buffer)
        (insert source)
        (when (fboundp 'js-mode) (js-mode))
        (display-buffer (current-buffer)))
      (when (yes-or-no-p (format "Write this to %s? " target))
        (make-directory (file-name-directory target) t)
        (when (file-exists-p target)
          (copy-file target (concat target ".bak-"
                                    (format-time-string "%Y%m%d%H%M%S"))))
        (copy-file elmux-opencode-plugin-source target t)
        (message "Plugin installed into %s; restart running sessions to apply"
                 target)))))

(require 'tab-bar)

(defvar elmux-workspace-main-name "main")
(defvar elmux-sidebar-width 28)
(defvar elmux--sidebar-visible nil)

(defvar elmux--workspace-indicator "")
(put 'elmux--workspace-indicator 'risky-local-variable t)

(defvar elmux--workspaces nil
  "Alist of workspace NAME to root DIRECTORY, in creation order.")

(defun elmux--workspace-tab-name (name)
  (concat "ws:" name))

(defun elmux--workspace-name (tab-name)
  (and (string-prefix-p "ws:" tab-name)
       (substring tab-name (length "ws:"))))

(defun elmux--plain-tab-p (tab-name)
  (not (or (elmux--workspace-name tab-name)
           (string-prefix-p "agent:" tab-name))))

(defun elmux--tab-names ()
  (mapcar (lambda (tab) (alist-get 'name tab))
          (funcall tab-bar-tabs-function)))

(defun elmux--workspace-root (name)
  (alist-get name elmux--workspaces nil nil #'equal))

(defun elmux--workspace-state (root)
  "Most urgent hook state among sessions rooted under ROOT, or nil."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (states
          (delq nil
                (mapcar
                 (lambda (b)
                   (let ((state (elmux--buffer-state b))
                         (dir (expand-file-name
                               (buffer-local-value 'default-directory b))))
                     (and (string-prefix-p root (file-name-as-directory dir))
                          (memq state '(waiting done working))
                          state)))
                 (elmux--vterm-buffers)))))
    (car (sort states (lambda (a b) (< (elmux--state-sort-key a)
                                       (elmux--state-sort-key b)))))))

(defun elmux--ensure-main-tab ()
  "Name the first auto-named plain tab main, unless a main tab exists."
  (when (bound-and-true-p tab-bar-mode)
    (let ((tabs (funcall tab-bar-tabs-function)))
      (unless (member elmux-workspace-main-name
                      (mapcar (lambda (tab) (alist-get 'name tab)) tabs))
        (when-let* ((tab (cl-find-if
                          (lambda (tab)
                            (and (elmux--plain-tab-p (alist-get 'name tab))
                                 (not (alist-get 'explicit-name tab))))
                          tabs)))
          (tab-bar-rename-tab-by-name (alist-get 'name tab)
                                      elmux-workspace-main-name))))))

(defun elmux--ensure-tab-bar ()
  (unless tab-bar-mode
    (setq tab-bar-show nil)
    (tab-bar-mode 1))
  (elmux--ensure-main-tab))

(defun elmux--update-workspace-indicator ()
  (let* ((name (and (bound-and-true-p tab-bar-mode)
                    (alist-get 'name (tab-bar--current-tab))))
         (label (and name (elmux--workspace-name name))))
    (setq elmux--workspace-indicator (if label (format " [%s]" label) ""))
    (force-mode-line-update t)))

(defun elmux--workspace-goto (name)
  "Switch to workspace NAME, creating its tab (landing in dired) if needed."
  (elmux--ensure-tab-bar)
  (let ((tab-name (elmux--workspace-tab-name name)))
    (if (member tab-name (elmux--tab-names))
        (tab-bar-switch-to-tab tab-name)
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (delete-other-windows)
      (dired (or (elmux--workspace-root name) default-directory))))
  (elmux--sidebar-sync))

(defun elmux-workspace-create (name directory)
  "Register workspace NAME rooted at DIRECTORY.
It appears in the sidebar; switch to it from there or with
`elmux-workspace-switch'.  Callable from Lisp to build variant
commands that compute DIRECTORY first (see Commentary)."
  (interactive
   (list (read-string "Workspace name: ")
         (read-directory-name
          "Root directory: "
          (or (when-let* ((pr (project-current))) (project-root pr))
              default-directory))))
  (when (string-empty-p name)
    (user-error "Workspace name cannot be empty"))
  (when (assoc name elmux--workspaces)
    (user-error "Workspace %s already exists" name))
  (setq elmux--workspaces
        (append elmux--workspaces
                (list (cons name (file-name-as-directory
                                  (expand-file-name directory))))))
  (elmux--sidebar-sync)
  (message "Workspace %s created; switch to it from the sidebar" name)
  name)

(defun elmux-workspace-next ()
  (interactive)
  (elmux--ensure-tab-bar)
  (tab-bar-switch-to-next-tab)
  (elmux--sidebar-sync))

(defun elmux-workspace-prev ()
  (interactive)
  (elmux--ensure-tab-bar)
  (tab-bar-switch-to-prev-tab)
  (elmux--sidebar-sync))

(defun elmux--current-workspace-name ()
  "Name of the active workspace: registered name, tab name, or main."
  (let ((tab (and (bound-and-true-p tab-bar-mode)
                  (alist-get 'name (tab-bar--current-tab)))))
    (or (and tab (elmux--workspace-name tab))
        tab
        elmux-workspace-main-name)))

(defun elmux--workspace-app-buffer-name (workspace app)
  (format "*%s:%s*" workspace app))

(defun elmux--workspace-app (app &optional command)
  "Start or switch to the APP vterm of the active workspace.
The buffer is named *WORKSPACE:APP*, so each workspace gets its own
instance.  COMMAND, if given, is run in the new vterm.  Starts in the
workspace root when the workspace is registered."
  (let* ((workspace (elmux--current-workspace-name))
         (buffer-name (elmux--workspace-app-buffer-name workspace app))
         (existing (get-buffer buffer-name)))
    (if existing
        (switch-to-buffer existing)
      (let ((default-directory
             (or (elmux--workspace-root workspace)
                 (when-let* ((pr (project-current))) (project-root pr))
                 default-directory)))
        (with-current-buffer (vterm t)
          (when command
            (vterm-send-string command)
            (vterm-send-return))
          (rename-buffer buffer-name t)
          (when (fboundp 'vterm-app-mode)
            (vterm-app-mode 1)))))))

(defun elmux-workspace-claude ()
  "Start or switch to the claude instance of the active workspace."
  (interactive)
  (elmux--workspace-app "claude" "claude"))

(defun elmux-workspace-vterm ()
  "Start or switch to the shell vterm of the active workspace."
  (interactive)
  (elmux--workspace-app "vterm"))

(defun elmux-workspace-codex ()
  "Start or switch to the codex instance of the active workspace."
  (interactive)
  (elmux--workspace-app "codex" "codex"))

(defun elmux-workspace-opencode ()
  "Start or switch to the opencode instance of the active workspace."
  (interactive)
  (elmux--workspace-app "opencode" "opencode"))

(defvar elmux-workspace-restore-commands
  '(("claude" . "claude --continue")
    ("codex" . "codex resume --last")
    ("opencode" . "opencode --continue")
    ("vterm" . nil))
  "Command run when desktop restores a workspace app vterm, keyed by app.
nil runs no command (plain shell); an unlisted app runs its own name.")

(defvar elmux-desktop-relaunch-agents t
  "When non-nil, desktop restore reruns each app's restore command.
When nil, restored app vterms are plain shells in the workspace root.")

(defun elmux--parse-app-buffer-name (name)
  "Return (WORKSPACE . APP) for a *WORKSPACE:APP* buffer NAME, else nil."
  (when (string-match "\\`\\*\\([^:*]+\\):\\([^:*]+\\)\\*\\'" name)
    (cons (match-string 1 name) (match-string 2 name))))

(defun elmux--restore-app (workspace app root)
  "Recreate the *WORKSPACE:APP* vterm in ROOT unless it exists; return it."
  (let ((buffer-name (elmux--workspace-app-buffer-name workspace app)))
    (or (get-buffer buffer-name)
        (let ((default-directory root))
          (save-window-excursion
            (with-current-buffer (vterm t)
              (when-let* ((command
                           (and elmux-desktop-relaunch-agents
                                (if (assoc app elmux-workspace-restore-commands)
                                    (cdr (assoc app elmux-workspace-restore-commands))
                                  app))))
                (vterm-send-string command)
                (vterm-send-return))
              (rename-buffer buffer-name t)
              (when (fboundp 'vterm-app-mode)
                (vterm-app-mode 1))
              (current-buffer)))))))

(defun elmux--desktop-save-buffer (_desktop-dirname)
  "Desktop data for a workspace app vterm: (WORKSPACE APP ROOT), else nil."
  (when-let* ((parsed (elmux--parse-app-buffer-name (buffer-name))))
    (list (car parsed) (cdr parsed) default-directory)))

(defun elmux--desktop-mark-buffer ()
  (setq-local desktop-save-buffer #'elmux--desktop-save-buffer))

(defun elmux--desktop-restore-buffer (_file-name _buffer-name misc)
  "Desktop handler for vterm buffers: recreate workspace apps from MISC."
  (pcase misc
    (`(,workspace ,app ,root)
     (when (and (stringp workspace) (stringp app) (stringp root)
                (file-directory-p root))
       (elmux--restore-app workspace app root)))))

(defun elmux-workspace-switch ()
  "Pick a workspace: plain tabs (main etc.) or a registered workspace."
  (interactive)
  (elmux--ensure-tab-bar)
  (let* ((plain (cl-remove-if-not #'elmux--plain-tab-p (elmux--tab-names)))
         (candidates (append plain (mapcar #'car elmux--workspaces)))
         (choice (completing-read "Workspace: " candidates nil t)))
    (if (assoc choice elmux--workspaces)
        (elmux--workspace-goto choice)
      (tab-bar-switch-to-tab choice)
      (elmux--sidebar-sync))))

;; Sidebar

(define-derived-mode elmux-sidebar-mode special-mode "ElmuxSide"
  "Sidebar listing agent workspaces."
  (setq truncate-lines t)
  (setq cursor-in-non-selected-windows nil))

(defun elmux--sidebar-rows ()
  "Return (LABEL STATE TARGET) rows: plain tabs first, then workspaces."
  (let* ((tabs (and (bound-and-true-p tab-bar-mode) (elmux--tab-names)))
         (plain (or (cl-remove-if-not #'elmux--plain-tab-p tabs)
                    (list elmux-workspace-main-name))))
    (append
     (mapcar (lambda (name) (list name nil (cons 'tab name))) plain)
     (mapcar (lambda (ws)
               (list (car ws) (elmux--workspace-state (cdr ws))
                     (cons 'ws (car ws))))
             elmux--workspaces))))

(defvar-local elmux--sidebar-marks nil
  "Marked sidebar targets, in the same form as the elmux-target property.")

(defun elmux--sidebar-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun elmux--sidebar-render ()
  (when-let* ((buf (get-buffer "*elmux-sidebar*")))
    (with-current-buffer buf
      (setq elmux--sidebar-marks
            (cl-remove-if (lambda (target)
                            (pcase target
                              (`(ws . ,name)
                               (not (assoc name elmux--workspaces)))
                              (_ nil)))
                          elmux--sidebar-marks))
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
        (dolist (row (elmux--sidebar-rows))
          (pcase-let ((`(,label ,state ,target) row))
            (let* ((currentp (pcase target
                               (`(tab . ,name) (equal name current))
                               (`(ws . ,name)
                                (equal (elmux--workspace-tab-name name)
                                       current))))
                   (markedp (member target elmux--sidebar-marks))
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
                       'elmux-target target
                       'face (and currentp 'highlight)
                       'mouse-face 'highlight)))))
        (elmux--sidebar-goto-line line)
        (dolist (wl window-lines)
          (set-window-point (car wl)
                            (save-excursion
                              (elmux--sidebar-goto-line (cdr wl))
                              (point))))))))

(defun elmux--sidebar-show ()
  (let ((buf (get-buffer-create "*elmux-sidebar*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'elmux-sidebar-mode)
        (elmux-sidebar-mode)))
    (display-buffer-in-side-window
     buf `((side . left) (slot . 0)
           (window-width . ,elmux-sidebar-width)
           (window-parameters . ((no-delete-other-windows . t)))))
    (elmux--sidebar-render)))

(defun elmux--sidebar-sync (&rest _)
  (elmux--ensure-main-tab)
  (elmux--update-workspace-indicator)
  (if elmux--sidebar-visible
      (elmux--sidebar-show)
    (when-let* ((win (get-buffer-window "*elmux-sidebar*")))
      (delete-window win)))
  (elmux--sidebar-render))

(defun elmux-sidebar-close ()
  "Close the workspace sidebar."
  (interactive)
  (setq elmux--sidebar-visible nil)
  (elmux--sidebar-sync))

(defun elmux-sidebar-toggle ()
  "Focus the sidebar, opening it if needed; close it when already focused."
  (interactive)
  (let ((win (get-buffer-window "*elmux-sidebar*")))
    (cond
     ((and win (eq (selected-window) win))
      (elmux-sidebar-close))
     (win (select-window win))
     (t
      (setq elmux--sidebar-visible t)
      (elmux--sidebar-sync)
      (when-let* ((win (get-buffer-window "*elmux-sidebar*")))
        (select-window win))))))

(defun elmux--select-non-side-window ()
  (when-let* ((win (cl-find-if-not
                    (lambda (w) (window-parameter w 'window-side))
                    (window-list))))
    (select-window win)))

(defun elmux--sidebar-act ()
  "Switch to the workspace on the current sidebar line; return non-nil if acted."
  (when-let* ((target (get-text-property (point) 'elmux-target)))
    (pcase target
      (`(tab . ,name)
       (elmux--ensure-tab-bar)
       (unless (equal name (alist-get 'name (tab-bar--current-tab)))
         (tab-bar-switch-to-tab name))
       (elmux--sidebar-sync))
      (`(ws . ,name) (elmux--workspace-goto name)))
    t))

(defun elmux-sidebar-select ()
  "Switch to the workspace on the current sidebar line."
  (interactive)
  (when (elmux--sidebar-act)
    (elmux--select-non-side-window)))

(defun elmux-sidebar-open ()
  "Open the workspace at point, keeping focus in the sidebar."
  (interactive)
  (let ((line (line-number-at-pos)))
    (when (elmux--sidebar-act)
      (when-let* ((win (get-buffer-window "*elmux-sidebar*")))
        (select-window win)
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun elmux-sidebar-mouse-select (event)
  (interactive "e")
  (mouse-set-point event)
  (elmux-sidebar-select))

(defun elmux--sidebar-target-at-point ()
  (get-text-property (point) 'elmux-target))

(defun elmux-sidebar-mark ()
  "Mark the workspace at point and move to the next line."
  (interactive)
  (when-let* ((target (elmux--sidebar-target-at-point)))
    (cl-pushnew target elmux--sidebar-marks :test #'equal)
    (elmux--sidebar-render))
  (forward-line 1))

(defun elmux-sidebar-unmark ()
  "Unmark the workspace at point and move to the next line."
  (interactive)
  (when-let* ((target (elmux--sidebar-target-at-point)))
    (setq elmux--sidebar-marks
          (cl-remove target elmux--sidebar-marks :test #'equal))
    (elmux--sidebar-render))
  (forward-line 1))

(defun elmux-sidebar-unmark-all ()
  "Remove all sidebar marks."
  (interactive)
  (setq elmux--sidebar-marks nil)
  (elmux--sidebar-render))

(defun elmux-sidebar-rename ()
  "Rename the workspace at point."
  (interactive)
  (pcase (elmux--sidebar-target-at-point)
    (`(ws . ,name)
     (let ((new (read-string (format "Rename %s to: " name) name)))
       (when (assoc new elmux--workspaces)
         (user-error "Workspace %s already exists" new))
       (setq elmux--workspaces
             (mapcar (lambda (ws)
                       (if (equal (car ws) name) (cons new (cdr ws)) ws))
                     elmux--workspaces))
       (when (member (elmux--workspace-tab-name name) (elmux--tab-names))
         (tab-bar-rename-tab-by-name (elmux--workspace-tab-name name)
                                     (elmux--workspace-tab-name new))))
     (elmux--sidebar-sync))
    (`(tab . ,name)
     (tab-bar-rename-tab-by-name
      name (read-string (format "Rename %s to: " name) name))
     (elmux--sidebar-sync))
    (_ (user-error "No workspace on this line"))))

(defun elmux-sidebar-delete ()
  "Delete marked workspaces, or the one at point when none are marked.
Session workspaces are killed; plain tabs are closed."
  (interactive)
  (let* ((targets (or elmux--sidebar-marks
                      (when-let* ((target (elmux--sidebar-target-at-point)))
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
           (setq elmux--workspaces (assoc-delete-all name elmux--workspaces))
           (let ((tab-name (elmux--workspace-tab-name name))
                 (names (elmux--tab-names)))
             (when (and (member tab-name names) (> (length names) 1))
               (ignore-errors (tab-bar-close-tab-by-name tab-name)))))
          (`(tab . ,name)
           (let ((names (elmux--tab-names)))
             (when (and (member name names) (> (length names) 1))
               (ignore-errors (tab-bar-close-tab-by-name name)))))))
      (setq elmux--sidebar-marks nil)
      (elmux--sidebar-sync))))

(define-key elmux-sidebar-mode-map (kbd "RET") #'elmux-sidebar-select)
(define-key elmux-sidebar-mode-map [mouse-1] #'elmux-sidebar-mouse-select)
(define-key elmux-sidebar-mode-map "o" #'elmux-sidebar-open)
(define-key elmux-sidebar-mode-map "q" #'elmux-sidebar-close)
(define-key elmux-sidebar-mode-map "R" #'elmux-sidebar-rename)
(define-key elmux-sidebar-mode-map "m" #'elmux-sidebar-mark)
(define-key elmux-sidebar-mode-map "u" #'elmux-sidebar-unmark)
(define-key elmux-sidebar-mode-map "U" #'elmux-sidebar-unmark-all)
(defun elmux--sidebar-visit-app (app &optional command)
  "Open the workspace at point (if any) and its APP vterm."
  (elmux--sidebar-act)
  (elmux--select-non-side-window)
  (elmux--workspace-app app command))

(defun elmux-sidebar-claude ()
  "Open the claude instance of the workspace at point."
  (interactive)
  (elmux--sidebar-visit-app "claude" "claude"))

(defun elmux-sidebar-codex ()
  "Open the codex instance of the workspace at point."
  (interactive)
  (elmux--sidebar-visit-app "codex" "codex"))

(defun elmux-sidebar-opencode ()
  "Open the opencode instance of the workspace at point."
  (interactive)
  (elmux--sidebar-visit-app "opencode" "opencode"))

(defun elmux-sidebar-vterm ()
  "Open the shell vterm of the workspace at point."
  (interactive)
  (elmux--sidebar-visit-app "vterm"))

(define-key elmux-sidebar-mode-map "D" #'elmux-sidebar-delete)
(define-key elmux-sidebar-mode-map "N" #'elmux-workspace-create)
(define-key elmux-sidebar-mode-map (kbd "a c") #'elmux-sidebar-claude)
(define-key elmux-sidebar-mode-map (kbd "a x") #'elmux-sidebar-codex)
(define-key elmux-sidebar-mode-map (kbd "a o") #'elmux-sidebar-opencode)
(define-key elmux-sidebar-mode-map (kbd "a t") #'elmux-sidebar-vterm)
(with-eval-after-load 'evil
  (evil-set-initial-state 'elmux-sidebar-mode 'normal)
  (evil-define-key 'normal elmux-sidebar-mode-map
    (kbd "RET") #'elmux-sidebar-select
    "o" #'elmux-sidebar-open
    "q" #'elmux-sidebar-close
    "R" #'elmux-sidebar-rename
    "m" #'elmux-sidebar-mark
    "u" #'elmux-sidebar-unmark
    "U" #'elmux-sidebar-unmark-all
    "D" #'elmux-sidebar-delete
    "N" #'elmux-workspace-create
    (kbd "a c") #'elmux-sidebar-claude
    (kbd "a x") #'elmux-sidebar-codex
    (kbd "a o") #'elmux-sidebar-opencode
    (kbd "a t") #'elmux-sidebar-vterm))

(unless noninteractive (elmux-setup))

(provide 'elmux)
;;; elmux.el ends here
