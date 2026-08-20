;;; agents.el --- Overview of vterm agent sessions -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'filenotify)
(require 'json)
(require 'project)

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
  "Refresh badge and dashboard after the state store changed."
  (agents--update-badge)
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
  (if-let ((pr (ignore-errors (project-current nil dir))))
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

(unless noninteractive (agents-setup))

(provide 'agents)
;;; agents.el ends here
