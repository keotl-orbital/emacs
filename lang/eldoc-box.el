;;; eldoc-box.el --- -*- lexical-binding: t; -*-

(use-package eldoc-box
  :ensure t
  :init
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)
  (setq eldoc-box-only-multi-line t) ;; to keep single-line in minibuffer
  )

(defun keotl/eldoc-box-set-offset(child-width child-height)
  "Position eldoc-box based on window width.
   For narrow windows: Bottom if cursor at top of window, at top otherwise
   For wide windows: top right corner of window."
  (let* ((win-width (- (nth 2 (window-absolute-pixel-edges))
                       (nth 0 (window-absolute-pixel-edges))))
         (small-window (< win-width (/ (frame-pixel-width) 2)))
         (current-y (cdr (posn-x-y (posn-at-point))))
         (x (if small-window
                (+ 48 (car (window-inside-pixel-edges)))
              (- (caddr (window-inside-pixel-edges)) 16 child-width)))
         (y (if small-window
                (let ((is-cursor-in-top-of-file (< current-y (+ child-height 64))))
                  (if is-cursor-in-top-of-file
                      ;; (+ (cadr (window-inside-pixel-edges)) current-y 32)
                      (- (nth 3 (window-inside-pixel-edges)) child-height 16)
                      (+ 16 (cadr (window-inside-pixel-edges)))
                    ))
                (+ 16 (cadr (window-inside-pixel-edges))))))
    (cons x y)))

(setq eldoc-box-position-function #'keotl/eldoc-box-set-offset)
(setq eldoc-box-at-point-position-function #'keotl/eldoc-box-set-offset)
