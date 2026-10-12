;;; find-test-file.el --- -*- lexical-binding: t; -*-

(defvar test-file-name-transforms-alist
  '((".*\\.tsx?$" . (lambda (file-name)
		      (let* ((edited1 (replace-regexp-in-string "\\/src\\/" "/test/" file-name))
			     (edited2 (replace-regexp-in-string "\\.ts$" ".test.ts" edited1))
			     (edited3 (replace-regexp-in-string "\\.tsx$" ".test.tsx" edited2))
			     )
			edited3
			)
		      ))

    (".*\\.scala?$" . (lambda (file-name)
		        (let* ((edited1 (replace-regexp-in-string "\\/src\\/main\\/" "/src/test/" file-name))
			       (edited2 (replace-regexp-in-string "\\.scala$" "Spec.scala" edited1))
			       )
			  edited2
			  )
		        ))
    (".*\\.py$" . (lambda (file-name)
		        (let* ((edited1 (replace-regexp-in-string "\\/src\\/" "/test/" file-name))
			       (edited2 (replace-regexp-in-string "\\([^/]+\\)\\.py$" (lambda (x) (concat "test_" x)) edited1))
			       )
			  edited2
			  )
		        ))
    )
  "alist of impl to test file name transforms. "
  ;; To override in dir-locals.el:
  ;; (nil . ((eval . (add-to-list 'test-file-name-transforms-alist
  ;; '(".*\\.tsx?$" . (lambda (file-name)
  ;; 	      (let* ((edited1 (replace-regexp-in-string "\\/src\\/" "/otherdir/" file-name))
  ;;
  ;; 		     )
  ;; 		edited1
  ;; 		)
  ;; 	      )
  ;;   )))))

  )


(defun test-file-name-for-impl (impl-file-name)
  (unless impl-file-name (error "Not a filename"))
  (let ((transform (assoc impl-file-name test-file-name-transforms-alist
			  (lambda (pattern impl-file-name) (string-match-p pattern impl-file-name))
			  )))
    (if transform
	(funcall (cdr transform) impl-file-name)
      (error (format "No transform found for file %s" impl-file-name))
      )
    ))

(defun find-test-file ()
  "Open test file matching the current open file."
  (interactive)
  (find-file (test-file-name-for-impl buffer-file-name)))

(defun find-test-file-other-window ()
  "Open test file matching the current open file in an other window."
  (interactive)
  (find-file-other-window (test-file-name-for-impl buffer-file-name)))
