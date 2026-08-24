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
    (dolist (event '(UserPromptSubmit PostToolUse Notification
                     PermissionRequest PreToolUse Stop))
      (should (memq event (agents-test--hook-events merged))))
    (let* ((pre (alist-get 'PreToolUse (alist-get 'hooks merged)))
           (entry (aref pre 0)))
      (should (equal (alist-get 'matcher entry) "AskUserQuestion|ExitPlanMode"))
      (should (string-match-p "waiting"
                              (alist-get 'command
                                         (aref (alist-get 'hooks entry) 0)))))
    (let* ((perm (alist-get 'PermissionRequest (alist-get 'hooks merged)))
           (entry (aref perm 0)))
      (should-not (alist-get 'matcher entry))
      (should (string-match-p "waiting"
                              (alist-get 'command
                                         (aref (alist-get 'hooks entry) 0)))))
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
  (should (equal (agents--workspace-name (agents--workspace-tab-name "feat-x")) "feat-x"))
  (should-not (agents--workspace-name "main"))
  (should-not (agents--workspace-name "*claude*"))
  (should (agents--plain-tab-p "main"))
  (should-not (agents--plain-tab-p "ws:feat-x"))
  (should-not (agents--plain-tab-p "agent:1-2-3")))

(ert-deftest agents-workspace-create-registers ()
  (let ((agents--workspaces nil)
        (dir (make-temp-file "agents-ws" t)))
    (unwind-protect
        (progn
          (agents-workspace-create "feat" dir)
          (should (equal (agents--workspace-root "feat")
                         (file-name-as-directory dir)))
          (should-error (agents-workspace-create "feat" dir)
                        :type 'user-error)
          (should-error (agents-workspace-create "" dir)
                        :type 'user-error))
      (delete-directory dir t))))

(ert-deftest agents-workspace-state-from-sessions-under-root ()
  (agents-test--with-state-dir
   (let ((agents--workspaces nil)
         (root (file-name-as-directory (make-temp-file "agents-root" t))))
     (unwind-protect
         (agents-test--with-vterm-buffer buf t
           (setq default-directory root)
           (setq agents--session-id "sess-ws")
           (puthash "sess-ws" (list :state 'waiting :since (float-time))
                    agents--states)
           (should (eq (agents--workspace-state root) 'waiting))
           (setq default-directory (expand-file-name "~/"))
           (should-not (agents--workspace-state root)))
       (delete-directory root t)))))

(ert-deftest agents-sidebar-rows-main-then-workspaces ()
  (agents-test--with-state-dir
   (let ((agents--workspaces (list (cons "feat" (expand-file-name "~/")))))
     (let ((rows (agents--sidebar-rows)))
       (should (equal (car rows)
                      (list agents-workspace-main-name nil '(tab . "main"))))
       (let ((ws-row (assoc "feat" rows)))
         (should ws-row)
         (should (equal (nth 2 ws-row) '(ws . "feat"))))))))

(ert-deftest agents-sidebar-render-lists-workspaces ()
  (agents-test--with-state-dir
   (let ((agents--workspaces (list (cons "feat" (expand-file-name "~/")))))
     (unwind-protect
         (progn
           (get-buffer-create "*agents-sidebar*")
           (with-current-buffer "*agents-sidebar*" (agents-sidebar-mode))
           (agents--sidebar-render)
           (with-current-buffer "*agents-sidebar*"
             (let ((text (buffer-string)))
               (should (string-match-p "workspaces" text))
               (should (string-match-p "main" text))
               (should (string-match-p "feat" text)))))
       (kill-buffer "*agents-sidebar*")))))

(ert-deftest agents-sidebar-open-binding ()
  (should (eq (lookup-key agents-sidebar-mode-map "o") 'agents-sidebar-open))
  (should (commandp 'agents-sidebar-open)))

(ert-deftest agents-sidebar-close-binding ()
  (should (eq (lookup-key agents-sidebar-mode-map "q") 'agents-sidebar-close))
  (should (commandp 'agents-sidebar-close))
  (should (commandp 'agents-sidebar-toggle)))

(defmacro agents-test--with-sidebar (&rest body)
  `(unwind-protect
       (progn
         (get-buffer-create "*agents-sidebar*")
         (with-current-buffer "*agents-sidebar*"
           (agents-sidebar-mode))
         (agents--sidebar-render)
         (with-current-buffer "*agents-sidebar*" ,@body))
     (kill-buffer "*agents-sidebar*")))

(ert-deftest agents-sidebar-dired-bindings ()
  (dolist (pair '(("R" . agents-sidebar-rename)
                  ("m" . agents-sidebar-mark)
                  ("u" . agents-sidebar-unmark)
                  ("U" . agents-sidebar-unmark-all)
                  ("D" . agents-sidebar-delete)
                  ("N" . agents-workspace-create)
                  ("a c" . agents-sidebar-claude)
                  ("a x" . agents-sidebar-codex)
                  ("a t" . agents-sidebar-vterm)))
    (should (eq (lookup-key agents-sidebar-mode-map (kbd (car pair))) (cdr pair)))))

(ert-deftest agents-sidebar-mark-and-unmark ()
  (agents-test--with-state-dir
   (let ((agents--workspaces (list (cons "feat" (expand-file-name "~/")))))
     (agents-test--with-sidebar
      (goto-char (point-min))
      (search-forward "feat")
      (beginning-of-line)
      (agents-sidebar-mark)
      (should (equal agents--sidebar-marks '((ws . "feat"))))
      (should (string-match-p "^\\*.*feat" (buffer-string)))
      (goto-char (point-min))
      (search-forward "feat")
      (agents-sidebar-unmark)
      (should-not agents--sidebar-marks)))))

(ert-deftest agents-sidebar-delete-marked-unregisters-workspace ()
  (agents-test--with-state-dir
   (let ((agents--workspaces (list (cons "feat" (expand-file-name "~/"))
                                   (cons "other" (expand-file-name "~/")))))
     (agents-test--with-sidebar
      (setq agents--sidebar-marks '((ws . "feat")))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (agents-sidebar-delete))
      (should-not (assoc "feat" agents--workspaces))
      (should (assoc "other" agents--workspaces))
      (should-not agents--sidebar-marks)))))

(ert-deftest agents-sidebar-delete-uses-point-when-unmarked ()
  (agents-test--with-state-dir
   (let ((agents--workspaces (list (cons "feat" (expand-file-name "~/")))))
     (agents-test--with-sidebar
      (goto-char (point-min))
      (search-forward "feat")
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (agents-sidebar-delete))
      (should-not (assoc "feat" agents--workspaces))))))

(ert-deftest agents-sidebar-rename-workspace ()
  (agents-test--with-state-dir
   (let ((agents--workspaces (list (cons "feat" (expand-file-name "~/")))))
     (agents-test--with-sidebar
      (goto-char (point-min))
      (search-forward "feat")
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "feat-renamed")))
        (agents-sidebar-rename))
      (should (assoc "feat-renamed" agents--workspaces))
      (should-not (assoc "feat" agents--workspaces))))))

(ert-deftest agents-workspace-app-buffer-name ()
  (should (equal (agents--workspace-app-buffer-name "feat" "claude") "*feat:claude*"))
  (should (equal (agents--workspace-app-buffer-name "main" "vterm") "*main:vterm*"))
  (should (commandp 'agents-workspace-claude))
  (should (commandp 'agents-workspace-vterm))
  (should (commandp 'agents-workspace-codex)))

(ert-deftest agents-current-workspace-name-defaults-to-main ()
  (let ((tab-bar-mode nil))
    (should (equal (agents--current-workspace-name) "main"))))

(ert-deftest agents-setup-persists-registry-via-desktop ()
  (agents-test--with-state-dir
   (let ((desktop-globals-to-save nil)
         (agents--watch nil)
         (agents--poll-timer nil)
         (had-tab-bar tab-bar-mode))
     (unwind-protect
         (progn
           (agents-setup)
           (should (memq 'agents--workspaces desktop-globals-to-save)))
       (when agents--watch (file-notify-rm-watch agents--watch))
       (when agents--poll-timer (cancel-timer agents--poll-timer))
       (unless had-tab-bar (tab-bar-mode -1))))))

(ert-deftest agents-ensure-main-tab-renames-auto-named-tab ()
  (let ((had tab-bar-mode))
    (unwind-protect
        (progn
          (tab-bar-mode 1)
          (agents--ensure-main-tab)
          (should (member agents-workspace-main-name (agents--tab-names))))
      (tab-bar-rename-tab "")
      (unless had (tab-bar-mode -1)))))

(ert-deftest agents-ensure-main-tab-respects-explicit-names ()
  (let ((had tab-bar-mode))
    (unwind-protect
        (progn
          (tab-bar-mode 1)
          (tab-bar-rename-tab "mytab")
          (agents--ensure-main-tab)
          (should-not (member agents-workspace-main-name (agents--tab-names)))
          (should (member "mytab" (agents--tab-names))))
      (tab-bar-rename-tab "")
      (unless had (tab-bar-mode -1)))))

(defmacro agents-test--with-snapshot-file (&rest body)
  `(let ((agents-snapshot-file (make-temp-file "agents-snapshot")))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file agents-snapshot-file)))))

(defun agents-test--fake-app-buffer (name)
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf (setq major-mode 'vterm-mode))
    buf))

(ert-deftest agents-workspace-apps-discovery ()
  (let ((bufs (mapcar #'agents-test--fake-app-buffer
                      '("*feat:claude*" "*feat:vterm*" "*other:claude*"))))
    (unwind-protect
        (let ((apps (agents--workspace-apps "feat")))
          (should (member "claude" apps))
          (should (member "vterm" apps))
          (should (= (length apps) 2)))
      (mapc #'kill-buffer bufs))))

(ert-deftest agents-snapshot-roundtrip ()
  (agents-test--with-snapshot-file
   (let ((agents--workspaces (list (cons "feat" (expand-file-name "~/"))))
         (tab-bar-mode nil)
         (buf (agents-test--fake-app-buffer "*feat:claude*")))
     (unwind-protect
         (progn
           (agents-workspaces-save)
           (let ((snapshot (agents--snapshot-read)))
             (should (equal (alist-get 'workspaces snapshot) agents--workspaces))
             (should (equal (cdr (assoc "feat" (alist-get 'apps snapshot)))
                            '("claude")))
             (should-not (alist-get 'layouts snapshot))))
       (kill-buffer buf)))))

(ert-deftest agents-workspaces-restore-merges-and-skips ()
  (agents-test--with-snapshot-file
   (let ((had tab-bar-mode)
         (missing (make-temp-file "agents-gone" t)))
     (delete-directory missing)
     (unwind-protect
         (progn
           (let ((agents--workspaces
                  (list (cons "kept" (expand-file-name "~/"))
                        (cons "gone" missing))))
             (agents-workspaces-save))
           (let ((agents--workspaces (list (cons "kept" "/elsewhere/"))))
             (agents-workspaces-restore)
             (should (equal (agents--workspace-root "kept") "/elsewhere/"))
             (should-not (assoc "gone" agents--workspaces))
             (should (= (length agents--workspaces) 1))))
       (tab-bar-rename-tab "")
       (unless had (tab-bar-mode -1))))))

(ert-deftest agents-workspaces-restore-restores-registry ()
  (agents-test--with-snapshot-file
   (let ((had tab-bar-mode))
     (unwind-protect
         (progn
           (let ((agents--workspaces (list (cons "feat" (expand-file-name "~/")))))
             (agents-workspaces-save))
           (let ((agents--workspaces nil))
             (agents-workspaces-restore)
             (should (equal (agents--workspace-root "feat")
                            (expand-file-name "~/")))))
       (dolist (name (agents--tab-names))
         (when (string-prefix-p "ws:" name)
           (ignore-errors (tab-bar-close-tab-by-name name))))
       (tab-bar-rename-tab "")
       (unless had (tab-bar-mode -1))))))

(ert-deftest agents-workspaces-restore-errors-without-snapshot ()
  (let ((agents-snapshot-file "/nonexistent/agents-snapshot.eld"))
    (should-error (agents-workspaces-restore) :type 'user-error)))

(ert-deftest agents-file-watch-bookkeeping ()
  (agents-test--with-state-dir
   (let ((agents--file-watches (make-hash-table :test 'equal))
         (added nil) (removed nil))
     (cl-letf (((symbol-function 'file-notify-add-watch)
                (lambda (file _flags _cb) (push file added) (gensym)))
               ((symbol-function 'file-notify-rm-watch)
                (lambda (desc) (push desc removed))))
       (let ((file (agents-test--write-state "sess-1" "working")))
         (agents--watch-state-file file)
         (should (= (length added) 1))
         (should (gethash file agents--file-watches))
         (agents--watch-state-file file)
         (should (= (length added) 1))
         (agents--watch-state-file
          (expand-file-name "missing" agents-state-directory))
         (should (= (length added) 1))
         (agents--unwatch-state-file file)
         (should (= (length removed) 1))
         (should-not (gethash file agents--file-watches)))))))

(ert-deftest agents-file-callback-updates-store-and-unwatches ()
  (agents-test--with-state-dir
   (let ((agents--file-watches (make-hash-table :test 'equal)))
     (let ((file (agents-test--write-state "sess-1" "waiting")))
       (puthash file 'desc agents--file-watches)
       (cl-letf (((symbol-function 'file-notify-rm-watch) #'ignore))
         (agents--file-notify-callback (list 'desc 'changed file))
         (should (eq (plist-get (gethash "sess-1" agents--states) :state)
                     'waiting))
         (agents--file-notify-callback (list 'desc 'deleted file))
         (should-not (gethash file agents--file-watches)))))))

(ert-deftest agents-dir-callback-adds-file-watch ()
  (agents-test--with-state-dir
   (let ((agents--file-watches (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'file-notify-add-watch)
                (lambda (&rest _) (gensym))))
       (let ((file (agents-test--write-state "sess-1" "working")))
         (agents--notify-callback (list 'desc 'created file))
         (should (gethash file agents--file-watches)))))))

(ert-deftest agents-merged-codex-hooks ()
  (let ((merged (agents--merged-hooks nil agents--codex-hook-events)))
    (dolist (event '(UserPromptSubmit PostToolUse PermissionRequest Stop))
      (should (memq event (agents-test--hook-events merged))))
    (should-not (memq 'Notification (agents-test--hook-events merged)))
    (should (commandp 'agents-install-codex-hooks))))

(provide 'agents-test)
;;; agents-test.el ends here
