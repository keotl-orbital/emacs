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

(provide 'agents)
;;; agents.el ends here
