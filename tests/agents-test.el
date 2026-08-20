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

(provide 'agents-test)
;;; agents-test.el ends here
