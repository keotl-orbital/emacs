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
  nil)

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
  (condition-case nil
      (setq agents--watch
            (file-notify-add-watch agents-state-directory '(change)
                                   #'agents--notify-callback))
    (error
     (setq agents--poll-timer
           (run-with-timer 3 3 #'agents--poll-state-directory)))))

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

(unless noninteractive (agents-setup))

(provide 'agents)
;;; agents.el ends here
