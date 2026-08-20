;;; agents-test.el --- Tests for agents.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest agents-parse-state ()
  (should (eq (agents--parse-state "working") 'working))
  (should (eq (agents--parse-state "waiting\n") 'waiting))
  (should (eq (agents--parse-state " done ") 'done))
  (should-not (agents--parse-state "garbage"))
  (should-not (agents--parse-state ""))
  (should-not (agents--parse-state nil)))

(ert-deftest agents-state-sort-key ()
  (should (< (agents--state-sort-key 'waiting)
             (agents--state-sort-key 'done)))
  (should (< (agents--state-sort-key 'done)
             (agents--state-sort-key 'working)))
  (should (< (agents--state-sort-key 'working)
             (agents--state-sort-key 'running)))
  (should (< (agents--state-sort-key 'running)
             (agents--state-sort-key 'exited))))

(ert-deftest agents-tool-name ()
  (should (equal (agents--tool-name "*claude*" nil) "claude"))
  (should (equal (agents--tool-name "*claude:project100x*" nil) "claude"))
  (should (equal (agents--tool-name "*codex*<2>" nil) "codex"))
  (should (equal (agents--tool-name "*vterm*" "fish") "fish"))
  (should (equal (agents--tool-name "*vterm*" nil) "vterm"))
  (should (equal (agents--tool-name "scratch" nil) "?")))

(ert-deftest agents-format-since ()
  (should (equal (agents--format-since 30) "30s"))
  (should (equal (agents--format-since 120) "2m"))
  (should (equal (agents--format-since 7200) "2h"))
  (should (equal (agents--format-since nil) "—")))

(ert-deftest agents-badge-text ()
  (should-not (agents--badge-text 0))
  (should (equal (agents--badge-text 2) "●2")))

(defmacro agents-test--with-state-dir (&rest body)
  `(let ((agents-state-directory
          (file-name-as-directory (make-temp-file "agents-test" t)))
         (agents--states (make-hash-table :test 'equal)))
     (unwind-protect (progn ,@body)
       (delete-directory agents-state-directory t))))

(defun agents-test--write-state (id content)
  (let ((file (expand-file-name id agents-state-directory)))
    (with-temp-file file (insert content))
    file))

(ert-deftest agents-handle-state-file-valid ()
  (agents-test--with-state-dir
   (let ((file (agents-test--write-state "sess-1" "waiting\n")))
     (should (eq (agents--handle-state-file file) 'waiting))
     (let ((info (gethash "sess-1" agents--states)))
       (should (eq (plist-get info :state) 'waiting))
       (should (numberp (plist-get info :since)))))))

(ert-deftest agents-handle-state-file-keeps-since-when-unchanged ()
  (agents-test--with-state-dir
   (let ((file (agents-test--write-state "sess-1" "working")))
     (agents--handle-state-file file)
     (let ((since (plist-get (gethash "sess-1" agents--states) :since)))
       (agents--handle-state-file file)
       (should (equal since
                      (plist-get (gethash "sess-1" agents--states) :since)))))))

(ert-deftest agents-handle-state-file-malformed-removes-entry ()
  (agents-test--with-state-dir
   (agents--handle-state-file (agents-test--write-state "sess-1" "waiting"))
   (agents--handle-state-file (agents-test--write-state "sess-1" "junk"))
   (should-not (gethash "sess-1" agents--states))))

(ert-deftest agents-sweep-stale-files ()
  (agents-test--with-state-dir
   (agents-test--write-state "dead-session" "waiting")
   (agents--sweep-stale-files)
   (should-not (file-exists-p
                (expand-file-name "dead-session" agents-state-directory)))))

(ert-deftest agents-new-session-id-unique ()
  (should-not (equal (agents--new-session-id) (agents--new-session-id))))

(defmacro agents-test--with-vterm-buffer (varname live-process &rest body)
  "Create a fake vterm buffer bound to VARNAME; attach a live process if LIVE-PROCESS."
  (declare (indent 2))
  `(let ((,varname (generate-new-buffer "*claude*")))
     (unwind-protect
         (with-current-buffer ,varname
           (setq major-mode 'vterm-mode)
           (when ,live-process
             (start-process "agents-test-proc" ,varname "cat"))
           ,@body)
       (when-let ((p (get-buffer-process ,varname)))
         (set-process-query-on-exit-flag p nil))
       (kill-buffer ,varname))))

(ert-deftest agents-buffer-state-exited-when-no-process ()
  (agents-test--with-state-dir
   (agents-test--with-vterm-buffer buf nil
     (should (eq (agents--buffer-state buf) 'exited)))))

(ert-deftest agents-buffer-state-running-without-store-entry ()
  (agents-test--with-state-dir
   (agents-test--with-vterm-buffer buf t
     (should (eq (agents--buffer-state buf) 'running)))))

(ert-deftest agents-buffer-state-uses-store ()
  (agents-test--with-state-dir
   (agents-test--with-vterm-buffer buf t
     (setq agents--session-id "sess-1")
     (puthash "sess-1" (list :state 'waiting :since (float-time))
              agents--states)
     (should (eq (agents--buffer-state buf) 'waiting)))))

(ert-deftest agents-entry-hook-tracked-session ()
  (agents-test--with-state-dir
   (agents-test--with-vterm-buffer buf t
     (setq agents--session-id "sess-1")
     (puthash "sess-1" (list :state 'waiting :since (- (float-time) 120))
              agents--states)
     (pcase-let ((`(,id ,row) (agents--entry buf)))
       (should (eq id buf))
       (should (string-match-p "waiting" (aref row 0)))
       (should (equal (aref row 1) "claude"))
       (should (equal (aref row 3) (buffer-name buf)))
       (should (equal (aref row 4) "2m"))))))

(ert-deftest agents-entry-untracked-session-has-no-since ()
  (agents-test--with-state-dir
   (agents-test--with-vterm-buffer buf t
     (pcase-let ((`(,_id ,row) (agents--entry buf)))
       (should (string-match-p "running" (aref row 0)))
       (should (equal (aref row 4) "—"))))))

(ert-deftest agents-dashboard-entries-sorted-waiting-first ()
  (agents-test--with-state-dir
   (agents-test--with-vterm-buffer running-buf t
     (agents-test--with-vterm-buffer waiting-buf t
       (setq agents--session-id "sess-w")
       (puthash "sess-w" (list :state 'waiting :since (float-time))
                agents--states)
       (let ((entries (agents--dashboard-entries)))
         (should (eq (caar entries) waiting-buf)))))))

(ert-deftest agents-dashboard-mode-renders ()
  (agents-test--with-state-dir
   (agents-test--with-vterm-buffer buf t
     (setq agents--session-id "sess-1")
     (puthash "sess-1" (list :state 'waiting :since (float-time))
              agents--states)
     (with-current-buffer (get-buffer-create "*agents-test-dash*")
       (unwind-protect
           (progn
             (agents-dashboard-mode)
             (revert-buffer)
             (should (string-match-p "waiting" (buffer-string)))
             (should (string-match-p "claude" (buffer-string))))
         (kill-buffer))))))

(ert-deftest agents-waiting-buffers ()
  (agents-test--with-state-dir
   (agents-test--with-vterm-buffer buf t
     (setq agents--session-id "sess-1")
     (puthash "sess-1" (list :state 'waiting :since (float-time))
              agents--states)
     (should (equal (agents--waiting-buffers) (list buf))))))

(ert-deftest agents-update-badge ()
  (agents-test--with-state-dir
   (let ((agents--badge ""))
     (agents--update-badge)
     (should (equal agents--badge ""))
     (agents-test--with-vterm-buffer buf t
       (setq agents--session-id "sess-1")
       (puthash "sess-1" (list :state 'waiting :since (float-time))
                agents--states)
       (agents--update-badge)
       (should (string-match-p "●1" agents--badge))))))

(defun agents-test--hook-events (json)
  (mapcar #'car (alist-get 'hooks json)))

(ert-deftest agents-merged-claude-hooks-from-empty ()
  (let ((merged (agents--merged-claude-hooks nil)))
    (dolist (event '(UserPromptSubmit PostToolUse Notification Stop))
      (should (memq event (agents-test--hook-events merged))))
    (let* ((stop (alist-get 'Stop (alist-get 'hooks merged)))
           (cmd (alist-get 'command (aref (alist-get 'hooks (aref stop 0)) 0))))
      (should (string-match-p "emacs-agents" cmd))
      (should (string-match-p "done" cmd)))))

(ert-deftest agents-merged-claude-hooks-idempotent ()
  (let ((once (agents--merged-claude-hooks nil)))
    (should (equal once (agents--merged-claude-hooks (copy-tree once t))))))

(ert-deftest agents-merged-claude-hooks-preserves-existing ()
  (let* ((existing '((hooks . ((Stop . [((hooks . [((type . "command")
                                                   (command . "say done"))]))])))
                     (model . "opus")))
         (merged (agents--merged-claude-hooks (copy-tree existing t)))
         (stop (alist-get 'Stop (alist-get 'hooks merged))))
    (should (equal (alist-get 'model merged) "opus"))
    (should (= (length stop) 2))
    (should (equal (alist-get 'command
                              (aref (alist-get 'hooks (aref stop 0)) 0))
                   "say done"))))

(ert-deftest agents-workspace-tab-name-roundtrip ()
  (should (equal (agents--workspace-session-id (agents--workspace-tab-name "abc-1")) "abc-1"))
  (should-not (agents--workspace-session-id "main"))
  (should-not (agents--workspace-session-id "*claude*")))

(ert-deftest agents-workspace-buffer-lookup ()
  (agents-test--with-state-dir
   (agents-test--with-vterm-buffer buf t
     (setq agents--session-id "sess-ws")
     (should (eq (agents--workspace-buffer (agents--workspace-tab-name "sess-ws")) buf))
     (should-not (agents--workspace-buffer (agents--workspace-tab-name "unknown")))
     (should-not (agents--workspace-buffer "main")))))

(ert-deftest agents-workspace-label ()
  (agents-test--with-state-dir
   (agents-test--with-vterm-buffer buf t
     (setq agents--session-id "sess-ws")
     (should (equal (agents--workspace-label (agents--workspace-tab-name "sess-ws"))
                    (buffer-name buf)))
     (should (equal (agents--workspace-label "main") "main")))))

(ert-deftest agents-sidebar-rows-main-first-then-sessions ()
  (agents-test--with-state-dir
   (agents-test--with-vterm-buffer buf t
     (setq agents--session-id "sess-ws")
     (puthash "sess-ws" (list :state 'waiting :since (float-time))
              agents--states)
     (let ((rows (agents--sidebar-rows)))
       (should (equal (car rows)
                      (list agents-workspace-main-name nil '(tab . "main"))))
       (let ((session-row (assoc (buffer-name buf) rows)))
         (should session-row)
         (should (eq (nth 1 session-row) 'waiting))
         (should (equal (nth 2 session-row) (cons 'buffer buf))))))))

(ert-deftest agents-sidebar-render-lists-workspaces ()
  (agents-test--with-state-dir
   (agents-test--with-vterm-buffer buf t
     (setq agents--session-id "sess-ws")
     (puthash "sess-ws" (list :state 'waiting :since (float-time))
              agents--states)
     (unwind-protect
         (progn
           (get-buffer-create "*agents-sidebar*")
           (with-current-buffer "*agents-sidebar*" (agents-sidebar-mode))
           (agents--sidebar-render)
           (with-current-buffer "*agents-sidebar*"
             (let ((text (buffer-string)))
               (should (string-match-p "workspaces" text))
               (should (string-match-p "main" text))
               (should (string-match-p (regexp-quote (buffer-name buf)) text))
               (should (string-match-p "●" text)))))
       (kill-buffer "*agents-sidebar*")))))

(ert-deftest agents-sidebar-open-binding ()
  (should (eq (lookup-key agents-sidebar-mode-map "o") 'agents-sidebar-open))
  (should (commandp 'agents-sidebar-open)))

(provide 'agents-test)
;;; agents-test.el ends here
