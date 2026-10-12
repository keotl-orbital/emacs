;;; reload-config.el --- -*- lexical-binding: t; -*-

(defun reload-config ()
  "Re-load init.el and everything it loads."
  (interactive)
  (load user-init-file)
  (message "Reloaded %s" user-init-file))
