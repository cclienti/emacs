;; -*- lisp -*-

;;;========== General Package Config ========================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq  package-archive-priorities
  '(("melpa" . 0)
     ("melpa-stable" . 10)
     ("gnu" . 20)))
(package-initialize)
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))


;;;========== Packages  ================================
(use-package emacs
  :ensure nil
  :init
  (defvar my-font-sizes '("Terminus-9" "Terminus-11" "Terminus-13")
    "Liste des tailles de police disponibles.")
  (defvar my-default-font "DejaVu Sans Mono-10"
    "Police par défaut si Terminus n'est pas disponible.")
  (defun my-set-font ()
    "Définit automatiquement la police en fonction de la résolution."
    (when (display-graphic-p)
      (let ((font (cond
                    ((member "Terminus" (font-family-list))
                      (if (and (>= (display-pixel-width) 1920)
                            (>= (display-mm-width) 500))
                        "Terminus-11"
                        "Terminus-9"))
                    (t my-default-font))))  ;; Fallback si Terminus n'est pas dispo
        (set-frame-font font nil t))))

  ;; Move custom into a separate file
  (defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
  (unless (file-exists-p custom-file) (write-region "" nil custom-file))
  (load custom-file)

  ;;(add-to-list 'default-frame-alist '(inhibit-double-buffering . t)) ;; Disable double buffering
  ;;(setq byte-compile-warnings '(not free-vars))  ;; Disable compilation warnings regarding free vars

  (setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))   ;; Window name
  (setq inhibit-startup-message t) ;; No startup message
  (setq max-lisp-eval-depth 20000) ;; Increase Emacs eval depth buffer
  (setq make-backup-files nil)  ;; Prevent Emacs from creating backup files
  (setq auto-save-default nil)  ;; Idem
  (setq ring-bell-function 'ignore)   ;; No beep
  (setq scroll-preserve-screen-position t)
  (setq whitespace-line-column 120)  ;; Line wrap
  (set-fill-column 120)  ;; Line wrap
  (column-number-mode t) ;; Column number
  (scroll-bar-mode -1)   ;; No scroll bar
  (setq-default tab-width 4) ;; Default tab size
  (setq x-select-enable-clipboard t)  ;; use clipboard
  (setq x-select-enable-primary t)  ;; use primary
  ;;(setq mouse-drag-copy-region t)
  (xterm-mouse-mode 1)
  (my-set-font)

  (global-set-key '[f9] (lambda () (interactive) (set-frame-font "Terminus-13" nil t)))
  (global-set-key '[f8] (lambda () (interactive) (set-frame-font "Terminus-11" nil t)))
  (global-set-key '[f7] (lambda () (interactive) (set-frame-font "Terminus-9" nil t)))
  (global-set-key [M-left]  'indent-rigidly-left)  ;; Rigid Indent left region
  (global-set-key [M-right] 'indent-rigidly-right)  ;; Rigid Indent right region
  (global-set-key '[f3] 'whitespace-mode)   ;; display tabs
  (global-set-key '[f5] 'align-regexp)   ;; Align regex shortcut
  (global-set-key '[f1] 'treemacs)

  :hook
  (before-save . delete-trailing-whitespace)
  )

(use-package display-line-numbers
  :ensure nil
  :hook
  (prog-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 't) ;; 't' (abs), 'relative' or 'visual' (visual-line-mode)
  )

(use-package lisp-mode
  :hook (lisp-mode .
          (lambda ()
            (setq lisp-indent-offset 2
              tab-width 2
              indent-tabs-mode nil)))
  )

(use-package color-theme-modern
  :ensure t
  :config
  (load-theme 'tango-dark t t)
  (enable-theme 'tango-dark)
  )

(use-package flyspell
  :diminish flycheck-mode)

(use-package all-the-icons
  :ensure t
  )

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode)
  )

(use-package treemacs
  :init
  (display-line-numbers-mode -1)
  :ensure t
  :defer t
  :after (treemacs-all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons")
  )

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package magit
  :ensure t
  :commands (magit-status magit-blame)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :hook
  (git-commit-mode . flyspell-mode)
  :config
  (setq magit-save-repository-buffers 'dontask
        magit-revert-buffers 'silent
    magit-no-confirm '(stage-all-changes))
  )

(use-package rst
  :ensure t
  :mode ("\\.rst\\'" . rst-mode)
  :hook (rst-mode . visual-line-mode)
  :config
  (setq rst-indent-width 4)
  (setq rst-adornment-level 2)
  (add-hook 'rst-mode-hook
    (lambda ()
      (setq fill-column 100)
      (auto-fill-mode 1))))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown"
    markdown-asymmetric-header 1
    markdown-header-scaling 1)
  :hook (markdown-mode . auto-fill-mode)
  )

(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook
    (lambda ()
      (flyspell-prog-mode)
      (show-paren-mode 1)
      (setq lsp-prefer-flymake nil
        indent-tabs-mode nil
        highlight-indentation-offset 4
        c-basic-offset 4)
      ;; (c-set-offset 'case-label '+)
      (c-set-offset 'innamespace 0)
      (c-set-offset 'substatement-open 0)
      (c-set-offset 'brace-list-open 0)
      (c-set-offset 'arglist-intro '+)
      (c-set-offset 'arglist-close 0)))
  )

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'flyspell-prog-mode)
  )

(use-package python
  :config
  (add-hook 'python-mode-hook #'flyspell-prog-mode)
  (setq indent-tabs-mode nil
    tab-width 4)
  )

(use-package ruff-format
  :ensure t
  :hook (python-mode . ruff-format-on-save-mode)
  )

(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp)
          (c++-mode . lsp)
          (rust-mode . lsp)
          (python-mode . lsp))
  :config
  (setq lsp-enable-snippet nil
    lsp-enable-indentation nil
    lsp-enable-on-type-formatting nil
    lsp-completion-enable-additional-text-edit nil
    lsp-completion-provider :capf
    lsp-completion-show-detail t)
  )

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable nil)
  )
