;;; toggle-transparency.el --- -*- lexical-binding: t; -*-

(defun toggle-transparency ()
  (interactive)
  (let* ((current-alpha (or (frame-parameter nil 'alpha-background) 100))
         (new-alpha (if (< current-alpha 100) 100 90)))
    ;; Set alpha-background (macOS/modern)
    (set-frame-parameter nil 'alpha-background new-alpha)
    (setq default-frame-alist
          (cons (cons 'alpha-background new-alpha)
                (assq-delete-all 'alpha-background default-frame-alist)))
    ;; Set alpha (X11/legacy)
    (set-frame-parameter (selected-frame) 'alpha (list new-alpha new-alpha))
    (setq default-frame-alist
          (cons (cons 'alpha (list new-alpha new-alpha))
                (assq-delete-all 'alpha default-frame-alist)))
    (message "Transparency %s" (if (< new-alpha 100) "enabled" "disabled"))))
