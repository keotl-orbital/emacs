;;; elmux-test.el --- Tests for elmux.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest elmux-parse-state ()
  (should (eq (elmux--parse-state "working") 'working))
  (should (eq (elmux--parse-state "waiting\n") 'waiting))
  (should (eq (elmux--parse-state " done ") 'done))
  (should-not (elmux--parse-state "garbage"))
  (should-not (elmux--parse-state ""))
  (should-not (elmux--parse-state nil)))

(ert-deftest elmux-state-sort-key ()
  (should (< (elmux--state-sort-key 'waiting)
             (elmux--state-sort-key 'done)))
  (should (< (elmux--state-sort-key 'done)
             (elmux--state-sort-key 'working)))
  (should (< (elmux--state-sort-key 'working)
             (elmux--state-sort-key 'running)))
  (should (< (elmux--state-sort-key 'running)
             (elmux--state-sort-key 'exited))))

(ert-deftest elmux-tool-name ()
  (should (equal (elmux--tool-name "*claude*" nil) "claude"))
  (should (equal (elmux--tool-name "*claude:project100x*" nil) "claude"))
  (should (equal (elmux--tool-name "*codex*<2>" nil) "codex"))
  (should (equal (elmux--tool-name "*vterm*" "fish") "fish"))
  (should (equal (elmux--tool-name "*vterm*" nil) "vterm"))
  (should (equal (elmux--tool-name "scratch" nil) "?")))

(ert-deftest elmux-format-since ()
  (should (equal (elmux--format-since 30) "30s"))
  (should (equal (elmux--format-since 120) "2m"))
  (should (equal (elmux--format-since 7200) "2h"))
  (should (equal (elmux--format-since nil) "—")))

(ert-deftest elmux-badge-text ()
  (should-not (elmux--badge-text 0))
  (should (equal (elmux--badge-text 2) "●2")))

(defmacro elmux-test--with-state-dir (&rest body)
  `(let ((elmux-state-directory
          (file-name-as-directory (make-temp-file "elmux-test" t)))
         (elmux--states (make-hash-table :test 'equal)))
     (unwind-protect (progn ,@body)
       (delete-directory elmux-state-directory t))))

(defun elmux-test--write-state (id content)
  (let ((file (expand-file-name id elmux-state-directory)))
    (with-temp-file file (insert content))
    file))

(ert-deftest elmux-handle-state-file-valid ()
  (elmux-test--with-state-dir
   (let ((file (elmux-test--write-state "sess-1" "waiting\n")))
     (should (eq (elmux--handle-state-file file) 'waiting))
     (let ((info (gethash "sess-1" elmux--states)))
       (should (eq (plist-get info :state) 'waiting))
       (should (numberp (plist-get info :since)))))))

(ert-deftest elmux-handle-state-file-keeps-since-when-unchanged ()
  (elmux-test--with-state-dir
   (let ((file (elmux-test--write-state "sess-1" "working")))
     (elmux--handle-state-file file)
     (let ((since (plist-get (gethash "sess-1" elmux--states) :since)))
       (elmux--handle-state-file file)
       (should (equal since
                      (plist-get (gethash "sess-1" elmux--states) :since)))))))

(ert-deftest elmux-handle-state-file-malformed-removes-entry ()
  (elmux-test--with-state-dir
   (elmux--handle-state-file (elmux-test--write-state "sess-1" "waiting"))
   (elmux--handle-state-file (elmux-test--write-state "sess-1" "junk"))
   (should-not (gethash "sess-1" elmux--states))))

(ert-deftest elmux-sweep-stale-files ()
  (elmux-test--with-state-dir
   (elmux-test--write-state "dead-session" "waiting")
   (elmux--sweep-stale-files)
   (should-not (file-exists-p
                (expand-file-name "dead-session" elmux-state-directory)))))

(ert-deftest elmux-new-session-id-unique ()
  (should-not (equal (elmux--new-session-id) (elmux--new-session-id))))

(defmacro elmux-test--with-vterm-buffer (varname live-process &rest body)
  "Create a fake vterm buffer bound to VARNAME; attach a live process if LIVE-PROCESS."
  (declare (indent 2))
  `(let ((,varname (generate-new-buffer "*claude*")))
     (unwind-protect
         (with-current-buffer ,varname
           (setq major-mode 'vterm-mode)
           (when ,live-process
             (start-process "elmux-test-proc" ,varname "cat"))
           ,@body)
       (when-let ((p (get-buffer-process ,varname)))
         (set-process-query-on-exit-flag p nil))
       (kill-buffer ,varname))))

(ert-deftest elmux-buffer-state-exited-when-no-process ()
  (elmux-test--with-state-dir
   (elmux-test--with-vterm-buffer buf nil
     (should (eq (elmux--buffer-state buf) 'exited)))))

(ert-deftest elmux-buffer-state-running-without-store-entry ()
  (elmux-test--with-state-dir
   (elmux-test--with-vterm-buffer buf t
     (should (eq (elmux--buffer-state buf) 'running)))))

(ert-deftest elmux-buffer-state-uses-store ()
  (elmux-test--with-state-dir
   (elmux-test--with-vterm-buffer buf t
     (setq elmux--session-id "sess-1")
     (puthash "sess-1" (list :state 'waiting :since (float-time))
              elmux--states)
     (should (eq (elmux--buffer-state buf) 'waiting)))))

(ert-deftest elmux-entry-hook-tracked-session ()
  (elmux-test--with-state-dir
   (elmux-test--with-vterm-buffer buf t
     (setq elmux--session-id "sess-1")
     (puthash "sess-1" (list :state 'waiting :since (- (float-time) 120))
              elmux--states)
     (pcase-let ((`(,id ,row) (elmux--entry buf)))
       (should (eq id buf))
       (should (string-match-p "waiting" (aref row 0)))
       (should (equal (aref row 1) "claude"))
       (should (equal (aref row 3) (buffer-name buf)))
       (should (equal (aref row 4) "2m"))))))

(ert-deftest elmux-entry-untracked-session-has-no-since ()
  (elmux-test--with-state-dir
   (elmux-test--with-vterm-buffer buf t
     (pcase-let ((`(,_id ,row) (elmux--entry buf)))
       (should (string-match-p "running" (aref row 0)))
       (should (equal (aref row 4) "—"))))))

(ert-deftest elmux-dashboard-entries-sorted-waiting-first ()
  (elmux-test--with-state-dir
   (elmux-test--with-vterm-buffer running-buf t
     (elmux-test--with-vterm-buffer waiting-buf t
       (setq elmux--session-id "sess-w")
       (puthash "sess-w" (list :state 'waiting :since (float-time))
                elmux--states)
       (let ((entries (elmux--dashboard-entries)))
         (should (eq (caar entries) waiting-buf)))))))

(ert-deftest elmux-dashboard-mode-renders ()
  (elmux-test--with-state-dir
   (elmux-test--with-vterm-buffer buf t
     (setq elmux--session-id "sess-1")
     (puthash "sess-1" (list :state 'waiting :since (float-time))
              elmux--states)
     (with-current-buffer (get-buffer-create "*elmux-test-dash*")
       (unwind-protect
           (progn
             (elmux-dashboard-mode)
             (revert-buffer)
             (should (string-match-p "waiting" (buffer-string)))
             (should (string-match-p "claude" (buffer-string))))
         (kill-buffer))))))

(ert-deftest elmux-waiting-buffers ()
  (elmux-test--with-state-dir
   (elmux-test--with-vterm-buffer buf t
     (setq elmux--session-id "sess-1")
     (puthash "sess-1" (list :state 'waiting :since (float-time))
              elmux--states)
     (should (equal (elmux--waiting-buffers) (list buf))))))

(ert-deftest elmux-update-badge ()
  (elmux-test--with-state-dir
   (let ((elmux--badge ""))
     (elmux--update-badge)
     (should (equal elmux--badge ""))
     (elmux-test--with-vterm-buffer buf t
       (setq elmux--session-id "sess-1")
       (puthash "sess-1" (list :state 'waiting :since (float-time))
                elmux--states)
       (elmux--update-badge)
       (should (string-match-p "●1" elmux--badge))))))

(defun elmux-test--hook-events (json)
  (mapcar #'car (alist-get 'hooks json)))

(ert-deftest elmux-merged-claude-hooks-from-empty ()
  (let ((merged (elmux--merged-claude-hooks nil)))
    (dolist (event '(UserPromptSubmit PostToolUse Notification
                     PermissionRequest PreToolUse Stop))
      (should (memq event (elmux-test--hook-events merged))))
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
      (should (string-match-p "\\.cache/elmux" cmd))
      (should (string-match-p "done" cmd)))))

(ert-deftest elmux-merged-claude-hooks-idempotent ()
  (let ((once (elmux--merged-claude-hooks nil)))
    (should (equal once (elmux--merged-claude-hooks (copy-tree once t))))))

(ert-deftest elmux-merged-claude-hooks-preserves-existing ()
  (let* ((existing '((hooks . ((Stop . [((hooks . [((type . "command")
                                                   (command . "say done"))]))])))
                     (model . "opus")))
         (merged (elmux--merged-claude-hooks (copy-tree existing t)))
         (stop (alist-get 'Stop (alist-get 'hooks merged))))
    (should (equal (alist-get 'model merged) "opus"))
    (should (= (length stop) 2))
    (should (equal (alist-get 'command
                              (aref (alist-get 'hooks (aref stop 0)) 0))
                   "say done"))))

(ert-deftest elmux-workspace-tab-name-roundtrip ()
  (should (equal (elmux--workspace-name (elmux--workspace-tab-name "feat-x")) "feat-x"))
  (should-not (elmux--workspace-name "main"))
  (should-not (elmux--workspace-name "*claude*"))
  (should (elmux--plain-tab-p "main"))
  (should-not (elmux--plain-tab-p "ws:feat-x"))
  (should-not (elmux--plain-tab-p "agent:1-2-3")))

(ert-deftest elmux-workspace-create-registers ()
  (let ((elmux--workspaces nil)
        (dir (make-temp-file "elmux-ws" t)))
    (unwind-protect
        (progn
          (elmux-workspace-create "feat" dir)
          (should (equal (elmux--workspace-root "feat")
                         (file-name-as-directory dir)))
          (should-error (elmux-workspace-create "feat" dir)
                        :type 'user-error)
          (should-error (elmux-workspace-create "" dir)
                        :type 'user-error))
      (delete-directory dir t))))

(ert-deftest elmux-workspace-state-from-sessions-under-root ()
  (elmux-test--with-state-dir
   (let ((elmux--workspaces nil)
         (root (file-name-as-directory (make-temp-file "elmux-root" t))))
     (unwind-protect
         (elmux-test--with-vterm-buffer buf t
           (setq default-directory root)
           (setq elmux--session-id "sess-ws")
           (puthash "sess-ws" (list :state 'waiting :since (float-time))
                    elmux--states)
           (should (eq (elmux--workspace-state root) 'waiting))
           (setq default-directory (expand-file-name "~/"))
           (should-not (elmux--workspace-state root)))
       (delete-directory root t)))))

(ert-deftest elmux-sidebar-rows-main-then-workspaces ()
  (elmux-test--with-state-dir
   (let ((elmux--workspaces (list (cons "feat" (expand-file-name "~/")))))
     (let ((rows (elmux--sidebar-rows)))
       (should (equal (car rows)
                      (list elmux-workspace-main-name nil '(tab . "main"))))
       (let ((ws-row (assoc "feat" rows)))
         (should ws-row)
         (should (equal (nth 2 ws-row) '(ws . "feat"))))))))

(ert-deftest elmux-sidebar-render-lists-workspaces ()
  (elmux-test--with-state-dir
   (let ((elmux--workspaces (list (cons "feat" (expand-file-name "~/")))))
     (unwind-protect
         (progn
           (get-buffer-create "*elmux-sidebar*")
           (with-current-buffer "*elmux-sidebar*" (elmux-sidebar-mode))
           (elmux--sidebar-render)
           (with-current-buffer "*elmux-sidebar*"
             (let ((text (buffer-string)))
               (should (string-match-p "workspaces" text))
               (should (string-match-p "main" text))
               (should (string-match-p "feat" text)))))
       (kill-buffer "*elmux-sidebar*")))))

(ert-deftest elmux-sidebar-open-binding ()
  (should (eq (lookup-key elmux-sidebar-mode-map "o") 'elmux-sidebar-open))
  (should (commandp 'elmux-sidebar-open)))

(ert-deftest elmux-sidebar-close-binding ()
  (should (eq (lookup-key elmux-sidebar-mode-map "q") 'elmux-sidebar-close))
  (should (commandp 'elmux-sidebar-close))
  (should (commandp 'elmux-sidebar-toggle)))

(defmacro elmux-test--with-sidebar (&rest body)
  `(unwind-protect
       (progn
         (get-buffer-create "*elmux-sidebar*")
         (with-current-buffer "*elmux-sidebar*"
           (elmux-sidebar-mode))
         (elmux--sidebar-render)
         (with-current-buffer "*elmux-sidebar*" ,@body))
     (kill-buffer "*elmux-sidebar*")))

(ert-deftest elmux-sidebar-dired-bindings ()
  (dolist (pair '(("R" . elmux-sidebar-rename)
                  ("m" . elmux-sidebar-mark)
                  ("u" . elmux-sidebar-unmark)
                  ("U" . elmux-sidebar-unmark-all)
                  ("D" . elmux-sidebar-delete)
                  ("N" . elmux-workspace-create)
                  ("a c" . elmux-sidebar-claude)
                  ("a x" . elmux-sidebar-codex)
                  ("a t" . elmux-sidebar-vterm)))
    (should (eq (lookup-key elmux-sidebar-mode-map (kbd (car pair))) (cdr pair)))))

(ert-deftest elmux-sidebar-mark-and-unmark ()
  (elmux-test--with-state-dir
   (let ((elmux--workspaces (list (cons "feat" (expand-file-name "~/")))))
     (elmux-test--with-sidebar
      (goto-char (point-min))
      (search-forward "feat")
      (beginning-of-line)
      (elmux-sidebar-mark)
      (should (equal elmux--sidebar-marks '((ws . "feat"))))
      (should (string-match-p "^\\*.*feat" (buffer-string)))
      (goto-char (point-min))
      (search-forward "feat")
      (elmux-sidebar-unmark)
      (should-not elmux--sidebar-marks)))))

(ert-deftest elmux-sidebar-delete-marked-unregisters-workspace ()
  (elmux-test--with-state-dir
   (let ((elmux--workspaces (list (cons "feat" (expand-file-name "~/"))
                                   (cons "other" (expand-file-name "~/")))))
     (elmux-test--with-sidebar
      (setq elmux--sidebar-marks '((ws . "feat")))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (elmux-sidebar-delete))
      (should-not (assoc "feat" elmux--workspaces))
      (should (assoc "other" elmux--workspaces))
      (should-not elmux--sidebar-marks)))))

(ert-deftest elmux-sidebar-delete-uses-point-when-unmarked ()
  (elmux-test--with-state-dir
   (let ((elmux--workspaces (list (cons "feat" (expand-file-name "~/")))))
     (elmux-test--with-sidebar
      (goto-char (point-min))
      (search-forward "feat")
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (elmux-sidebar-delete))
      (should-not (assoc "feat" elmux--workspaces))))))

(ert-deftest elmux-sidebar-rename-workspace ()
  (elmux-test--with-state-dir
   (let ((elmux--workspaces (list (cons "feat" (expand-file-name "~/")))))
     (elmux-test--with-sidebar
      (goto-char (point-min))
      (search-forward "feat")
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "feat-renamed")))
        (elmux-sidebar-rename))
      (should (assoc "feat-renamed" elmux--workspaces))
      (should-not (assoc "feat" elmux--workspaces))))))

(ert-deftest elmux-workspace-app-buffer-name ()
  (should (equal (elmux--workspace-app-buffer-name "feat" "claude") "*feat:claude*"))
  (should (equal (elmux--workspace-app-buffer-name "main" "vterm") "*main:vterm*"))
  (should (commandp 'elmux-workspace-claude))
  (should (commandp 'elmux-workspace-vterm))
  (should (commandp 'elmux-workspace-codex)))

(ert-deftest elmux-current-workspace-name-defaults-to-main ()
  (let ((tab-bar-mode nil))
    (should (equal (elmux--current-workspace-name) "main"))))

(ert-deftest elmux-setup-persists-registry-via-desktop ()
  (elmux-test--with-state-dir
   (let ((desktop-globals-to-save nil)
         (elmux--watch nil)
         (elmux--poll-timer nil)
         (had-tab-bar tab-bar-mode))
     (unwind-protect
         (progn
           (elmux-setup)
           (should (memq 'elmux--workspaces desktop-globals-to-save)))
       (when elmux--watch (file-notify-rm-watch elmux--watch))
       (when elmux--poll-timer (cancel-timer elmux--poll-timer))
       (unless had-tab-bar (tab-bar-mode -1))))))

(ert-deftest elmux-ensure-main-tab-renames-auto-named-tab ()
  (let ((had tab-bar-mode))
    (unwind-protect
        (progn
          (tab-bar-mode 1)
          (elmux--ensure-main-tab)
          (should (member elmux-workspace-main-name (elmux--tab-names))))
      (tab-bar-rename-tab "")
      (unless had (tab-bar-mode -1)))))

(ert-deftest elmux-ensure-main-tab-respects-explicit-names ()
  (let ((had tab-bar-mode))
    (unwind-protect
        (progn
          (tab-bar-mode 1)
          (tab-bar-rename-tab "mytab")
          (elmux--ensure-main-tab)
          (should-not (member elmux-workspace-main-name (elmux--tab-names)))
          (should (member "mytab" (elmux--tab-names))))
      (tab-bar-rename-tab "")
      (unless had (tab-bar-mode -1)))))

(ert-deftest elmux-file-watch-bookkeeping ()
  (elmux-test--with-state-dir
   (let ((elmux--file-watches (make-hash-table :test 'equal))
         (added nil) (removed nil))
     (cl-letf (((symbol-function 'file-notify-add-watch)
                (lambda (file _flags _cb) (push file added) (gensym)))
               ((symbol-function 'file-notify-rm-watch)
                (lambda (desc) (push desc removed))))
       (let ((file (elmux-test--write-state "sess-1" "working")))
         (elmux--watch-state-file file)
         (should (= (length added) 1))
         (should (gethash file elmux--file-watches))
         (elmux--watch-state-file file)
         (should (= (length added) 1))
         (elmux--watch-state-file
          (expand-file-name "missing" elmux-state-directory))
         (should (= (length added) 1))
         (elmux--unwatch-state-file file)
         (should (= (length removed) 1))
         (should-not (gethash file elmux--file-watches)))))))

(ert-deftest elmux-file-callback-updates-store-and-unwatches ()
  (elmux-test--with-state-dir
   (let ((elmux--file-watches (make-hash-table :test 'equal)))
     (let ((file (elmux-test--write-state "sess-1" "waiting")))
       (puthash file 'desc elmux--file-watches)
       (cl-letf (((symbol-function 'file-notify-rm-watch) #'ignore))
         (elmux--file-notify-callback (list 'desc 'changed file))
         (should (eq (plist-get (gethash "sess-1" elmux--states) :state)
                     'waiting))
         (elmux--file-notify-callback (list 'desc 'deleted file))
         (should-not (gethash file elmux--file-watches)))))))

(ert-deftest elmux-dir-callback-adds-file-watch ()
  (elmux-test--with-state-dir
   (let ((elmux--file-watches (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'file-notify-add-watch)
                (lambda (&rest _) (gensym))))
       (let ((file (elmux-test--write-state "sess-1" "working")))
         (elmux--notify-callback (list 'desc 'created file))
         (should (gethash file elmux--file-watches)))))))

(ert-deftest elmux-merged-codex-hooks ()
  (let ((merged (elmux--merged-hooks nil elmux--codex-hook-events)))
    (dolist (event '(UserPromptSubmit PostToolUse PermissionRequest Stop))
      (should (memq event (elmux-test--hook-events merged))))
    (should-not (memq 'Notification (elmux-test--hook-events merged)))
    (should (commandp 'elmux-install-codex-hooks))))

(defun elmux-test--fake-app-buffer (name)
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf (setq major-mode 'vterm-mode))
    buf))

(defmacro elmux-test--with-stub-vterm (sent-var &rest body)
  "Run BODY with `vterm' stubbed; commands sent are pushed onto SENT-VAR."
  (declare (indent 1))
  `(let ((,sent-var nil))
     (cl-letf (((symbol-function 'vterm)
                (lambda (&optional _)
                  (let ((buf (generate-new-buffer "*vterm-stub*")))
                    (set-buffer buf)
                    (setq major-mode 'vterm-mode)
                    buf)))
               ((symbol-function 'vterm-send-string)
                (lambda (s) (push s ,sent-var)))
               ((symbol-function 'vterm-send-return) #'ignore))
       ,@body)))

(ert-deftest elmux-parse-app-buffer-name ()
  (should (equal (elmux--parse-app-buffer-name "*feat:claude*") '("feat" . "claude")))
  (should-not (elmux--parse-app-buffer-name "*claude*"))
  (should-not (elmux--parse-app-buffer-name "*vterm*<2>"))
  (should-not (elmux--parse-app-buffer-name "sharepoint.txt")))

(ert-deftest elmux-desktop-save-buffer-data ()
  (let ((buf (elmux-test--fake-app-buffer "*feat:claude*")))
    (unwind-protect
        (with-current-buffer buf
          (setq default-directory (expand-file-name "~/"))
          (should (equal (elmux--desktop-save-buffer nil)
                         (list "feat" "claude" (expand-file-name "~/")))))
      (kill-buffer buf)))
  (with-temp-buffer
    (rename-buffer "*vterm*" t)
    (should-not (elmux--desktop-save-buffer nil))))

(ert-deftest elmux-desktop-restore-buffer-recreates-app ()
  (elmux-test--with-stub-vterm sent
    (let ((elmux-desktop-relaunch-agents t)
          (elmux-workspace-restore-commands '(("claude" . "claude --continue"))))
      (let ((buf (elmux--desktop-restore-buffer
                  nil "*feat:claude*" (list "feat" "claude" (expand-file-name "~/")))))
        (unwind-protect
            (progn
              (should (bufferp buf))
              (should (equal (buffer-name buf) "*feat:claude*"))
              (should (equal (buffer-local-value 'default-directory buf)
                             (expand-file-name "~/")))
              (should (equal sent '("claude --continue")))
              (should (eq (elmux--desktop-restore-buffer
                           nil "*feat:claude*" (list "feat" "claude" (expand-file-name "~/")))
                          buf)))
          (kill-buffer buf))))))

(ert-deftest elmux-desktop-restore-buffer-dormant-when-disabled ()
  (elmux-test--with-stub-vterm sent
    (let ((elmux-desktop-relaunch-agents nil))
      (let ((buf (elmux--desktop-restore-buffer
                  nil "*feat:claude*" (list "feat" "claude" (expand-file-name "~/")))))
        (unwind-protect
            (progn (should (bufferp buf)) (should-not sent))
          (kill-buffer buf))))))

(ert-deftest elmux-desktop-restore-buffer-skips-bad-data ()
  (elmux-test--with-stub-vterm sent
    (should-not (elmux--desktop-restore-buffer nil "*vterm*" nil))
    (should-not (elmux--desktop-restore-buffer
                 nil "*feat:claude*" (list "feat" "claude" "/nonexistent/dir/")))))

(defvar desktop-first-buffer)
(defvar desktop-buffer-ok-count)
(defvar desktop-buffer-fail-count)

(ert-deftest elmux-desktop-file-roundtrip-recreates-app-buffer ()
  "Save a desktop with an app vterm, then load the desktop file into a
clean state and check the handler recreates the buffer."
  (elmux-test--with-stub-vterm sent
    (let* ((dir (file-name-as-directory (make-temp-file "elmux-desktop" t)))
           (elmux--workspaces (list (cons "feat" (expand-file-name "~/"))))
           (elmux-desktop-relaunch-agents t)
           (desktop-globals-to-save '(elmux--workspaces))
           (desktop-buffer-mode-handlers
            (list (cons 'vterm-mode #'elmux--desktop-restore-buffer)))
           (desktop-restore-frames nil)
           (buf (elmux-test--fake-app-buffer "*feat:claude*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq default-directory (expand-file-name "~/"))
              (elmux--desktop-mark-buffer))
            (desktop-save dir t)
            (should (string-match-p "feat:claude"
                                    (with-temp-buffer
                                      (insert-file-contents
                                       (expand-file-name ".emacs.desktop" dir))
                                      (buffer-string))))
            (kill-buffer buf)
            (setq elmux--workspaces nil)
            (let ((desktop-first-buffer nil)
                  (desktop-buffer-ok-count 0)
                  (desktop-buffer-fail-count 0))
              (load (expand-file-name ".emacs.desktop" dir) nil t))
            (should (equal (elmux--workspace-root "feat") (expand-file-name "~/")))
            (should (get-buffer "*feat:claude*"))
            (should (equal sent '("claude --continue"))))
        (when (get-buffer "*feat:claude*") (kill-buffer "*feat:claude*"))
        (delete-directory dir t)))))

(ert-deftest elmux-merged-hooks-replaces-legacy-emacs-agents-entries ()
  (let* ((legacy '((hooks . ((Stop . [((hooks . [((type . "command")
                                                 (command . "[ -n \"$EMACS_AGENT_SESSION\" ] && echo done > \"$HOME/.cache/emacs-agents/$EMACS_AGENT_SESSION\" || true"))]))])))))
         (merged (elmux--merged-claude-hooks (copy-tree legacy t)))
         (stop (alist-get 'Stop (alist-get 'hooks merged))))
    (should (= (length stop) 1))
    (should (string-match-p "\\.cache/elmux"
                            (alist-get 'command (aref (alist-get 'hooks (aref stop 0)) 0))))
    (should-not (string-match-p "emacs-agents"
                                (alist-get 'command (aref (alist-get 'hooks (aref stop 0)) 0))))))

;;; Tab line

(defmacro elmux-test--with-tab-order (&rest body)
  "Run BODY with the tab-line globals bound to fresh, empty values."
  (declare (indent 0))
  `(let ((elmux--workspaces nil)
         (elmux--tab-order nil)
         (elmux--tab-order-names nil))
     ,@body))

(defun elmux-test--dir-buffer (name dir)
  "Create a buffer NAME whose `default-directory' is DIR."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq default-directory (file-name-as-directory dir)))
    buf))

(ert-deftest elmux-app-buffer-workspace-requires-registration ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("feat" . "/tmp/feat/")))
    (should (equal (elmux--app-buffer-workspace "*feat:claude*") "feat"))
    ;; main is a workspace for app buffers even though it has no root
    (should (equal (elmux--app-buffer-workspace "*main:vterm*") "main"))
    ;; an unregistered workspace is not one
    (should-not (elmux--app-buffer-workspace "*other:claude*"))
    ;; buffers that merely look like *X:Y* are not workspaces
    (should-not (elmux--app-buffer-workspace "*rg: pattern*"))
    (should-not (elmux--app-buffer-workspace "*scratch*"))))

(ert-deftest elmux-directory-workspace-longest-root-wins ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")
                              ("br" . "/tmp/proj/.worktrees/br/")))
    (should (equal (elmux--directory-workspace "/tmp/proj/src/") "proj"))
    (should (equal (elmux--directory-workspace "/tmp/proj/.worktrees/br/src/")
                   "br"))
    (should-not (elmux--directory-workspace "/tmp/elsewhere/"))))

(ert-deftest elmux-directory-workspace-ignores-remote ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    ;; must not consult a file-name handler: that can block in redisplay
    (should-not (elmux--directory-workspace "/ssh:host:/tmp/proj/src/"))))

(ert-deftest elmux-tab-line-candidate-p ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((app (elmux-test--dir-buffer "*proj:claude*" "/tmp/proj/"))
          (internal (elmux-test--dir-buffer " *hidden*" "/tmp/proj/"))
          (scratch (elmux-test--dir-buffer "*compilation*" "/tmp/proj/"))
          (outside (elmux-test--dir-buffer "*other:claude*" "/tmp/nope/")))
      (unwind-protect
          (progn
            (should (elmux--tab-line-candidate-p app))
            ;; a buffer visiting no file, not dired, not an app vterm
            (should-not (elmux--tab-line-candidate-p scratch))
            (should-not (elmux--tab-line-candidate-p internal))
            (should-not (elmux--tab-line-candidate-p outside))
            (with-current-buffer app
              (setq-local tab-line-exclude t)
              (should-not (elmux--tab-line-candidate-p app))))
        (mapc #'kill-buffer (list app internal scratch outside))))))

(ert-deftest elmux-tab-line-candidate-p-accepts-files-and-dired ()
  (elmux-test--with-tab-order
    (let ((dir (make-temp-file "elmux-ws" t)))
      (unwind-protect
          (let* ((elmux--workspaces (list (cons "proj" (file-name-as-directory dir))))
                 (file (expand-file-name "foo.el" dir)))
            (with-temp-file file (insert ";; x"))
            (let ((fbuf (find-file-noselect file))
                  (dbuf (dired-noselect dir)))
              (unwind-protect
                  (progn
                    (should (elmux--tab-line-candidate-p fbuf))
                    (should (elmux--tab-line-candidate-p dbuf)))
                (kill-buffer fbuf)
                (kill-buffer dbuf))))
        (delete-directory dir t)))))

(defun elmux-test--app-bufs (ws &rest apps)
  "Create workspace WS app buffers for APPS rooted at /tmp/WS/."
  (mapcar (lambda (app)
            (elmux-test--dir-buffer (format "*%s:%s*" ws app)
                                    (format "/tmp/%s/" ws)))
          apps))

(ert-deftest elmux-tab-order-appends-in-registration-order ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude" "codex" "vterm")))
      (unwind-protect
          (progn
            (mapc #'elmux--tab-line-register bufs)
            (should (equal (elmux--workspace-buffers "proj") bufs)))
        (mapc #'kill-buffer bufs)))))

(ert-deftest elmux-tab-order-stable-across-buffer-list-shuffle ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude" "codex" "vterm")))
      (unwind-protect
          (progn
            (mapc #'elmux--tab-line-register bufs)
            ;; reorder buffer-list the way visiting buffers does
            (set-buffer (nth 2 bufs)) (bury-buffer (nth 0 bufs))
            (set-buffer (nth 1 bufs))
            (should (equal (elmux--workspace-buffers "proj") bufs)))
        (mapc #'kill-buffer bufs)))))

(ert-deftest elmux-tab-order-survives-buffer-rename ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude" "codex")))
      (unwind-protect
          (progn
            (mapc #'elmux--tab-line-register bufs)
            ;; uniquify renames buffers behind our back; order must not care
            (with-current-buffer (car bufs) (rename-buffer "*proj:claude*<2>"))
            (should (equal (elmux--workspace-buffers "proj") bufs)))
        (mapc #'kill-buffer bufs)))))

(ert-deftest elmux-tab-order-register-is-idempotent ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude")))
      (unwind-protect
          (progn
            (elmux--tab-line-register (car bufs))
            (elmux--tab-line-register (car bufs))
            (should (equal (elmux--workspace-buffers "proj") bufs)))
        (mapc #'kill-buffer bufs)))))

(ert-deftest elmux-tab-order-ignores-non-candidates ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((junk (elmux-test--dir-buffer "*compilation*" "/tmp/proj/")))
      (unwind-protect
          (progn
            (elmux--tab-line-register junk)
            (should-not (elmux--workspace-buffers "proj")))
        (kill-buffer junk)))))

(ert-deftest elmux-tab-order-prunes-on-unregister ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude" "codex")))
      (unwind-protect
          (progn
            (mapc #'elmux--tab-line-register bufs)
            (elmux--tab-line-unregister (car bufs))
            (should (equal (elmux--workspace-buffers "proj") (cdr bufs))))
        (mapc #'kill-buffer bufs)))))

(ert-deftest elmux-tab-order-seeded-from-persisted-names ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    ;; desktop restored an order in which codex precedes claude
    (setq elmux--tab-order-names '(("proj" "*proj:codex*" "*proj:claude*")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude" "codex")))
      (unwind-protect
          (progn
            ;; registered claude-first, but the persisted order must win
            (mapc #'elmux--tab-line-register bufs)
            (should (equal (elmux--workspace-buffers "proj")
                           (list (nth 1 bufs) (nth 0 bufs)))))
        (mapc #'kill-buffer bufs)))))

(ert-deftest elmux-tab-line-tabs-writes-nothing ()
  "The tabs function runs in redisplay; it must not mutate the order."
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude" "codex")))
      (unwind-protect
          (progn
            (mapc #'elmux--tab-line-register bufs)
            (let ((before (copy-tree elmux--tab-order)))
              (with-current-buffer (car bufs)
                (elmux--tab-line-tabs)
                (elmux--tab-line-tabs))
              (should (equal elmux--tab-order before))))
        (mapc #'kill-buffer bufs)))))

(ert-deftest elmux-tab-line-tabs-scoped-to-workspace ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("a" . "/tmp/a/") ("b" . "/tmp/b/")))
    (let ((abufs (elmux-test--app-bufs "a" "claude" "codex"))
          (bbufs (elmux-test--app-bufs "b" "claude")))
      (unwind-protect
          (progn
            (mapc #'elmux--tab-line-register (append abufs bbufs))
            (with-current-buffer (car abufs)
              (should (equal (elmux--tab-line-tabs) abufs)))
            (with-current-buffer (car bbufs)
              (should (equal (elmux--tab-line-tabs) bbufs))))
        (mapc #'kill-buffer (append abufs bbufs))))))

(ert-deftest elmux-tab-line-tabs-never-empty ()
  "A blank strip still costs a screen line, so never return nil."
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude")))
      (unwind-protect
          (progn
            (mapc #'elmux--tab-line-register bufs)
            ;; the workspace is deleted out from under a live app buffer
            (setq elmux--workspaces nil
                  elmux--tab-order nil)
            (with-current-buffer (car bufs)
              (should (equal (elmux--tab-line-tabs) (list (car bufs))))))
        (mapc #'kill-buffer bufs)))))

(ert-deftest elmux-tab-line-tabs-survives-errors ()
  "An error inside redisplay is very noisy; degrade instead."
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude")))
      (unwind-protect
          (progn
            (mapc #'elmux--tab-line-register bufs)
            (cl-letf (((symbol-function 'elmux--workspace-buffers)
                       (lambda (&rest _) (error "boom"))))
              (with-current-buffer (car bufs)
                (should (equal (elmux--tab-line-tabs) (list (car bufs)))))))
        (mapc #'kill-buffer bufs)))))

(ert-deftest elmux-tab-name-strips-workspace-prefix ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude")))
      (unwind-protect
          (progn
            (should (equal (elmux--tab-name (car bufs)) "claude"))
            (should (equal (elmux--tab-name (get-buffer-create "foo.el"))
                           "foo.el")))
        (mapc #'kill-buffer bufs)
        (kill-buffer "foo.el")))))

(ert-deftest elmux-tab-order-snapshot-projects-live-order ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude" "codex"))
          (gone (elmux-test--app-bufs "dead" "claude")))
      (unwind-protect
          (progn
            (mapc #'elmux--tab-line-register bufs)
            ;; an order entry whose workspace is no longer registered
            (setf (alist-get "dead" elmux--tab-order nil nil #'equal) gone)
            (elmux--tab-order-snapshot)
            (should (equal (alist-get "proj" elmux--tab-order-names
                                      nil nil #'equal)
                           '("*proj:claude*" "*proj:codex*")))
            (should-not (assoc "dead" elmux--tab-order-names)))
        (mapc #'kill-buffer (append bufs gone))))))

(ert-deftest elmux-tab-order-snapshot-truncates ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let* ((elmux-tab-order-max 3)
           (bufs (apply #'elmux-test--app-bufs "proj"
                        '("a" "b" "c" "d" "e"))))
      (unwind-protect
          (progn
            (mapc #'elmux--tab-line-register bufs)
            (elmux--tab-order-snapshot)
            (should (= (length (alist-get "proj" elmux--tab-order-names
                                          nil nil #'equal))
                       3)))
        (mapc #'kill-buffer bufs)))))

(ert-deftest elmux-tab-order-restore-registers-existing-buffers ()
  "Desktop recreates app vterms without displaying them; restore must find them."
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (setq elmux--tab-order-names '(("proj" "*proj:codex*" "*proj:claude*")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude" "codex")))
      (unwind-protect
          (progn
            (elmux--tab-order-restore)
            (should (equal (elmux--workspace-buffers "proj")
                           (list (nth 1 bufs) (nth 0 bufs)))))
        (mapc #'kill-buffer bufs)))))

(ert-deftest elmux-setup-persists-tab-order-via-desktop ()
  (elmux-test--with-state-dir
   (let ((desktop-globals-to-save nil)
         (desktop-save-hook nil)
         (desktop-after-read-hook nil)
         (find-file-hook nil)
         (dired-mode-hook nil)
         (kill-buffer-hook nil)
         (elmux--watch nil)
         (elmux--poll-timer nil)
         (had-tab-bar tab-bar-mode))
     (unwind-protect
         (progn
           (elmux-setup)
           (should (memq 'elmux--tab-order-names desktop-globals-to-save))
           (should (memq 'elmux--tab-order-snapshot desktop-save-hook))
           (should (memq 'elmux--tab-order-restore desktop-after-read-hook))
           (should (memq 'elmux--tab-line-register find-file-hook))
           (should (memq 'elmux--tab-line-register dired-mode-hook))
           ;; vterm-mode-hook is not declared special by vterm, so it cannot
           ;; be let-bound here; dired-mode-hook covers the same wiring.
           (should (memq 'elmux--tab-line-unregister kill-buffer-hook)))
       (when elmux--watch (file-notify-rm-watch elmux--watch))
       (when elmux--poll-timer (cancel-timer elmux--poll-timer))
       (unless had-tab-bar (tab-bar-mode -1))))))

(ert-deftest elmux-workspace-create-sweeps-existing-buffers ()
  "A workspace can be registered after its files are already open."
  (elmux-test--with-tab-order
    (let ((dir (make-temp-file "elmux-ws" t)))
      (unwind-protect
          (let ((file (expand-file-name "foo.el" dir)))
            (with-temp-file file (insert ";; x"))
            (let ((fbuf (find-file-noselect file)))
              (unwind-protect
                  (progn
                    (should-not (elmux--workspace-buffers "late"))
                    (elmux-workspace-create "late" dir)
                    (should (equal (elmux--workspace-buffers "late")
                                   (list fbuf))))
                (kill-buffer fbuf))))
        (delete-directory dir t)))))

(ert-deftest elmux-sidebar-rename-rekeys-tab-order ()
  (elmux-test--with-state-dir
   (elmux-test--with-tab-order
     (setq elmux--workspaces (list (cons "feat" "/tmp/feat/")))
     (let ((bufs (elmux-test--app-bufs "feat" "claude")))
       (unwind-protect
           (progn
             (mapc #'elmux--tab-line-register bufs)
             (elmux-test--with-sidebar
              (goto-char (point-min))
              (search-forward "feat")
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) "feat2")))
                (elmux-sidebar-rename))
              (should (equal (elmux--workspace-buffers "feat2") bufs))
              (should-not (elmux--workspace-buffers "feat"))))
         (mapc #'kill-buffer bufs))))))

(ert-deftest elmux-sidebar-delete-forgets-tab-order ()
  (elmux-test--with-state-dir
   (elmux-test--with-tab-order
     (setq elmux--workspaces (list (cons "feat" "/tmp/feat/")))
     (let ((bufs (elmux-test--app-bufs "feat" "claude")))
       (unwind-protect
           (progn
             (mapc #'elmux--tab-line-register bufs)
             (elmux-test--with-sidebar
              (setq elmux--sidebar-marks '((ws . "feat")))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
                (elmux-sidebar-delete))
              (should-not (assoc "feat" elmux--tab-order))))
         (mapc #'kill-buffer bufs))))))

(ert-deftest elmux-elmux-ui-buffers-are-never-tabs ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((side (elmux-test--dir-buffer "*elmux-sidebar*" "/tmp/proj/"))
          (dash (elmux-test--dir-buffer "*elmux*" "/tmp/proj/")))
      (unwind-protect
          (progn
            (with-current-buffer side (elmux-sidebar-mode))
            (with-current-buffer dash (elmux-dashboard-mode))
            (should-not (elmux--tab-line-candidate-p side))
            (should-not (elmux--tab-line-candidate-p dash)))
        (kill-buffer side)
        (kill-buffer dash)))))

(ert-deftest elmux-restore-app-registers-the-buffer ()
  "vterm-mode-hook fires before rename-buffer, and the restored buffer is
never displayed, so registration must happen at the creation site."
  (elmux-test--with-tab-order
    (setq elmux--workspaces (list (cons "feat" (expand-file-name "~/"))))
    (elmux-test--with-stub-vterm sent
      (let ((buf (elmux--restore-app "feat" "claude" (expand-file-name "~/"))))
        (unwind-protect
            (progn
              (should (equal (buffer-name buf) "*feat:claude*"))
              (should (equal (elmux--workspace-buffers "feat") (list buf)))
              (should (equal (buffer-local-value 'elmux--tab-workspace buf)
                             "feat")))
          (kill-buffer buf))))))

(ert-deftest elmux-workspace-app-registers-the-buffer ()
  (elmux-test--with-tab-order
    (setq elmux--workspaces (list (cons "feat" (expand-file-name "~/"))))
    (elmux-test--with-stub-vterm sent
      (cl-letf (((symbol-function 'elmux--current-workspace-name)
                 (lambda () "feat"))
                ((symbol-function 'switch-to-buffer) #'set-buffer))
        (let ((buf (elmux--workspace-app "claude" "claude")))
          (unwind-protect
              (should (equal (elmux--workspace-buffers "feat")
                             (list (get-buffer "*feat:claude*"))))
            (kill-buffer (get-buffer "*feat:claude*"))))))))

(defun elmux-test--render-tab-line ()
  "Labels the real `tab-line-format' produces for the current buffer."
  (mapconcat (lambda (item)
               (if (stringp item) (substring-no-properties item) ""))
             (tab-line-format) ""))

(ert-deftest elmux-tab-line-renders-workspace-buffers ()
  "Exercise the real rendering path, not just the tabs function.
`redisplay' never evaluates a tab line in batch, but `tab-line-format'
can be called directly."
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((bufs (elmux-test--app-bufs "proj" "claude" "codex")))
      (unwind-protect
          (progn
            (mapc #'elmux--tab-line-register bufs)
            (with-current-buffer (car bufs)
              ;; app buffers show as APP, not *WORKSPACE:APP*
              (should (equal (elmux-test--render-tab-line) "|claude|codex|"))
              ;; and rendering must leave the order untouched
              (let ((before (copy-tree elmux--tab-order)))
                (elmux-test--render-tab-line)
                (should (equal elmux--tab-order before)))))
        (mapc #'kill-buffer bufs)))))

(ert-deftest elmux-tab-line-absent-outside-workspaces ()
  "A tab-line-format that merely evaluates to nil still costs a screen line,
so buffers outside every workspace must not get one at all."
  (elmux-test--with-tab-order
    (setq elmux--workspaces '(("proj" . "/tmp/proj/")))
    (let ((outside (elmux-test--dir-buffer "*elsewhere:claude*" "/tmp/nope/")))
      (unwind-protect
          (with-current-buffer outside
            (elmux--tab-line-register)
            (should-not tab-line-mode)
            (should-not tab-line-format))
        (kill-buffer outside)))))

(provide 'elmux-test)
;;; elmux-test.el ends here
