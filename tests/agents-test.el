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

(provide 'agents-test)
;;; agents-test.el ends here
