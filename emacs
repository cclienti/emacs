;; -*- lisp -*-

;;;========== General Package Config ========================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-archive-priorities '(("melpa" . 0) ("melpa-stable" . 10) ("gnu" . 20)))
(package-initialize)
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

;;;========== Packages  ================================
(use-package emacs
  :ensure nil
  :init
  (defvar my-default-font "DejaVu Sans Mono-10"
    "Police par défaut si Terminus n'est pas disponible.")
  (defun my-set-font ()
    "Choose automatically the size of the font depending on the screen resolution"
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
  ;;(setq use-package-verbose t)

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

  (global-set-key '[f1] 'treemacs)
  (global-set-key '[f3] 'whitespace-mode)   ;; display tabs
  (global-set-key '[f5] 'align-regexp)   ;; Align regex shortcut

  (global-set-key '[f7] (lambda () (interactive) (set-frame-font "Terminus-9" nil t)))
  (global-set-key '[f8] (lambda () (interactive) (set-frame-font "Terminus-11" nil t)))
  (global-set-key '[f9] (lambda () (interactive) (set-frame-font "Terminus-13" nil t)))

  (global-set-key [M-left]  'indent-rigidly-left)  ;; Rigid Indent left region
  (global-set-key [M-right] 'indent-rigidly-right)  ;; Rigid Indent right region

  :hook
  (before-save . delete-trailing-whitespace)
  )

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  )

(use-package emerge
  :ensure nil
  :config
  (setq emerge-diff-options "--ignore-all-space")
  (setq emerge-split-window-function 'split-window-horizontally)
  (setq emerge-window-setup 'same-window)
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
            (setq lisp-indent-offset 2)
            (setq tab-width 2)
            (setq indent-tabs-mode nil)))
  )

(use-package color-theme-modern
  :ensure t
  :config
  (load-theme 'tango-dark t t)
  (enable-theme 'tango-dark)
  )

(use-package flyspell
  :diminish flycheck-mode)

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode)
  )

(use-package treemacs
  :ensure t
  :defer t
  ;;:config
  ;;(setq treemacs-no-png-images t)
  :custom-face
  (treemacs-root-face ((t (:height 1.0))))
  (treemacs-file-face ((t (:height 1.0))))
  ;;:custom
  ;;(treemacs-load-all-the-icons-with-workaround-font t)
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
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-revert-buffers 'silent)
  (setq magit-no-confirm '(stage-all-changes))
  )

(use-package rst
  :ensure t
  :mode ("\\.rst\\'" . rst-mode)
  :hook ((rst-mode . visual-line-mode)
         (rst-mode . (lambda ()
                       (setq fill-column 100)
                       (auto-fill-mode 1))))
  :custom
  (rst-indent-width 4)
  (rst-adornment-level 2)
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . auto-fill-mode)
  :custom
  (markdown-command "markdown") ;; Use external markdown processor
  (markdown-asymmetric-header t) ;; Enable asymmetric headers
  ;;(markdown-header-scaling t)   ;; Scale headers dynamically
  )

(use-package cc-mode
  :hook (c-mode-common . my-cc-mode-setup)
  :custom
  (lsp-prefer-flymake nil) ;; Prefer LSP instead of Flymake
  (c-basic-offset 4)       ;; Standard indentation in C/C++
  (indent-tabs-mode nil)   ;; Disable tab characters (use spaces instead)
  (highlight-indentation-offset 4) ;; Visual indentation offset
  :config
  (defun my-cc-mode-setup ()
    "Custom configuration for C/C++ mode."
    (flyspell-prog-mode)  ;; Enable spell checking in programming mode
    (show-paren-mode 1)   ;; Enable matching parentheses highlighting
    ;; Configure specific indentation rules
    (c-set-offset 'innamespace 0)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'brace-list-open 0)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)))

(use-package rust-mode
  :ensure t
  :hook ((rust-mode . flyspell-prog-mode)
         (rust-mode . lsp-deferred)) ;; Enable LSP if available
  :custom
  (rust-format-on-save t) ;; Auto-format code on save
  )

(use-package python
  :hook (python-mode . flyspell-prog-mode)
  :custom
  (indent-tabs-mode nil) ;; Use spaces instead of tabs
  (tab-width 4))         ;; Set tab width to 4 spaces

(use-package snakemake-mode
  :ensure t
  :mode (("Snakefile\\'" . snakemake-mode)
          ("\\.smk\\'" . snakemake-mode))
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
         (python-mode . lsp)
         (snakemake-mode . lsp))
  :custom
  (lsp-enable-snippet nil)                 ;; Disable snippets
  (lsp-enable-indentation nil)             ;; Disable indentation
  (lsp-enable-on-type-formatting nil)      ;; Disable on-type formatting
  (lsp-completion-enable-additional-text-edit nil) ;; Disable additional text edit
  (lsp-completion-provider :capf)          ;; Use `capf` for completion
  (lsp-completion-show-detail t)           ;; Show completion details
  :config
  (add-to-list 'lsp-language-id-configuration '(snakemake-mode . "python")))

(use-package lsp-ui
  :defer t
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  )

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode)
  )

(use-package groovy-mode
  :ensure t
  :mode ("Jenkinsfile\\'" . groovy-mode)
  :hook (groovy-mode . (lambda ()
                         (setq indent-tabs-mode nil)
                         (setq groovy-indent-offset 4))))

(use-package jenkinsfile-mode
  :ensure t
  :mode "Jenkinsfile\\'")
