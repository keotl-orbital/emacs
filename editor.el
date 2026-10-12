;;; editor.el --- -*- lexical-binding: t; -*-

(load  (concat (file-name-directory load-file-name) "vendor/doom-bepo-autoload.el"))
(setq +layout-bepo-cr-rotation-style 'ergodis)

(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  ;; (setq evil-search-module 'evil-search)
  ;; (setq evil-ex-complete-emacs-commands nil)
  ;; (setq evil-vsplit-window-right t)
  ;; (setq evil-split-window-below t)
  ;; (setq evil-shift-round nil)
  ;; (setq evil-want-C-u-scroll t)
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
    (setq evil-want-minibuffer t)
  :config ;; tweak evil after loading it
  (evil-mode)

  (defun my/flymake-goto-next-error-by-severity ()
    (interactive)
    (let* ((diags (flymake-diagnostics))
           (severities (mapcar (lambda (d) (flymake--severity (flymake-diagnostic-type d))) diags))
           (max-sev (and severities (apply #'max severities)))
           (error-sev (flymake--severity :error))
           (warning-sev (flymake--severity :warning)))
      (cond
       ((null max-sev)
        (message "No flymake diagnostics"))
       ((>= max-sev error-sev)
        (flymake-goto-next-error 1 '(:error) t))
       ((>= max-sev warning-sev)
        (flymake-goto-next-error 1 '(:warning) t))
       (t
        (flymake-goto-next-error 1 nil t)))))

  (define-key evil-normal-state-map (kbd "g à") 'my/flymake-goto-next-error-by-severity)
  (define-key evil-normal-state-map (kbd "g À") 'flymake-goto-prev-error)
  (define-key evil-normal-state-map (kbd "g D") 'xref-find-definitions-other-window)
  (define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
  ;; org-mode-map exception: TAB must keep cycling folds
  (with-eval-after-load 'org
    (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle))
  (define-key evil-normal-state-map (kbd "z z") 'recenter-top-bottom)

  ;;   :bind
  ;; (:map evil-normal-state-map
  ;; 	("gà" . flymake-goto-next-error)
  ;; 	)
  )



(use-package evil-collection
  :after evil
  :ensure t
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init '(calendar dired ediff ivy xref vc-annotate))
  (+layout-bepo-rotate-ts-bare-keymap '(read-expression-map))
  (+layout-bepo-rotate-bare-keymap '(evil-window-map) +layout-bepo-cr-rotation-style)
  (+layout-bepo-rotate-evil-keymap +layout-bepo-cr-rotation-style)

  (evil-define-key nil evil-inner-text-objects-map
    "é" 'evil-inner-word
    "É" 'evil-inner-WORD
    )
    (evil-define-key nil evil-outer-text-objects-map
    "é" 'evil-a-word
    "É" 'evil-a-WORD
    )
  ;; dired
  (with-eval-after-load 'dired
    (evil-collection-translate-key nil '(dired-mode-map)
				   (kbd "<normal-state> t") (kbd "<normal-state> j")
				   (kbd "<normal-state> s") (kbd "<normal-state> k")
				   )
    (define-key dired-mode-map (kbd "<SPC>") nil)
    (define-key dired-mode-map (kbd "<normal-state> <SPC>") nil)
    )

  ;; ivy
  (with-eval-after-load 'ivy
    (evil-collection-translate-key nil '(ivy-minibuffer-map) (kbd "<normal-state> t") (kbd "<normal-state> j"))
    (evil-collection-translate-key nil '(ivy-minibuffer-map) (kbd "<normal-state> s") (kbd "<normal-state> k"))
    (evil-collection-translate-key nil '(ivy-minibuffer-map) (kbd "<normal-state> j") (kbd "<normal-state> t"))
    (evil-collection-translate-key nil '(ivy-minibuffer-map) (kbd "<normal-state> k") (kbd "<normal-state> s"))
    ;; (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
    ;; (evil-set-initial-state 'ivy-minibuffer 'normal)

    ;; Evil only sets the minibuffer default state via a buffer-local variable
    ;; that is killed when the minibuffer buffer is reused, so reused
    ;; minibuffers fall back to normal state. Register a proper initial state.
    (evil-set-initial-state 'minibuffer-mode 'insert)

    ;; Ivy installs its keymap as a composed local map, which evil cannot see,
    ;; so evil's insert-state [remap delete-backward-char] shadows
    ;; ivy-backward-delete-char and backspace dies on the read-only prompt.
    ;; Copy the local map's remap into evil's buffer-local insert map instead.
    (defun my/evil-minibuffer-restore-local-remaps ()
      (when (bound-and-true-p evil-local-mode)
        (let ((cmd (lookup-key (current-local-map) [remap delete-backward-char])))
          (when (commandp cmd)
            (evil-local-set-key 'insert [remap delete-backward-char] cmd)))))
    (add-hook 'minibuffer-setup-hook #'my/evil-minibuffer-restore-local-remaps 90)
    
  )
  )

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 1)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-max-height 0.2))

;; (use-package helm
;; :config
;; )

(use-package ivy
  :ensure t
  :init (setq ivy-initial-inputs-alist nil)
  :config (ivy-mode 1))

;; dired
(setq dired-listing-switches "-alh --group-directories-first")

;; key-chord
(load  (concat (file-name-directory load-file-name) "vendor/key-chord.el"))
(key-chord-mode 1)
(key-chord-define evil-normal-state-map ",," 'evil-force-normal-state)
(key-chord-define evil-visual-state-map ",," 'evil-change-to-previous-state)
(key-chord-define evil-insert-state-map ",," 'evil-normal-state)
(key-chord-define evil-replace-state-map ",," 'evil-normal-state)

;; company
(use-package company
  :ensure t
  :after eglot
  :hook (prog-mode . company-mode)
  :bind
  (:map company-mode-map
	("C-." . company-complete)
	)
  (:map company-active-map
	("M-n" . nil)
	)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2)
  ;; (company-dabbrev-downcase 0)
  )

;; (setq flymake-show-diagnostics-at-end-of-line t)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Interaction
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs (list (concat (file-name-directory load-file-name) "snippets")))
  (define-key evil-insert-state-map (kbd "M-m i i") 'yas-expand)
  )

(yas-global-mode)

(use-package markdown-mode
  :ensure t)
;; (defun add-yasnippet
;;     ()
;;     (setq company-backends '((company-capf :with company-yasnippet))))
;; (add-hook 'eglot--managed-mode-hook #'add-yasnippet)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(load  (concat (file-name-directory load-file-name) "vendor/highlight-indentation.el"))
(custom-set-variables '(highlight-indentation-overlay-string "|"))
(add-to-list 'prog-mode-hook 'highlight-indentation-mode)
(add-to-list 'prog-mode-hook 'display-line-numbers-mode)

;; move M-c to M-n to prevent swaywm keybinding conflict
(define-key global-map (kbd "M-n") 'capitalize-word)

;; mouse support in terminal (emacs -nw)
(xterm-mouse-mode 1)

