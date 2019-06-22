;; -*- lisp -*-

;;========== General ==================================================
;; Move custom into a separate file
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; Disable double buffering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; No Antiliazing
(setq mac-allow-anti-aliasing nil)

;; No startup message
(setq inhibit-startup-message t)

;; Increase emacs eval depth buffer
(setq max-lisp-eval-depth 20000)

;; Prevent emacs to create backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Disable compilation warning regarding free vars
(setq byte-compile-warnings '(not free-vars))

;; window name
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

;; Linum except in speedbar
(setq linum-format " %5d \u2502 ")
(global-linum-mode t)
(add-hook 'speedbar-mode-hook (lambda () (linum-mode 0)))

;; Column number
(column-number-mode t)

;; Line wrap
(setq whitespace-line-column 120)
(set-fill-column 119)

;; No bip
(setq ring-bell-function 'ignore)

;; No scroll bar
(scroll-bar-mode -1)

;; Preserve screen position while scrolling to avoid weird problems
(setq scroll-preserve-screen-position t)

;; default tab size
(setq default-tab-width 4)

;; default frame size
;;(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 120))


;;;========== Proxy =====================================================
;; The url-proxy-services already manages http_proxy env variable.
;; The following is not necessary.
;;
;; (defconst proxy-value (getenv "http_proxy") "proxy environment value")
;; (if proxy-value
;; 	(progn (message "setting proxy to: %s" proxy-value)
;; 		   (setq url-proxy-services
;; 				 '(("http"     . proxy-value)
;; 				   ("https"    . proxy-value)
;; 				   ("ftp"      . proxy-value)))))


;;;========== Packages ==================================================
;; Define package repository
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")))
  (package-initialize))

;; user emacs package dir
(defconst my-emacs-pkg-dir (expand-file-name "packages" user-emacs-directory))
(unless (file-directory-p my-emacs-pkg-dir)
  (message "Creating emacs package dir %s" my-emacs-pkg-dir)
  (package-refresh-contents)
  (make-directory my-emacs-pkg-dir t))

(add-to-list 'load-path my-emacs-pkg-dir)

;; Automatically download packages
(setq my-package-list
      '(flycheck flycheck-pycheckers flycheck-pyflakes elpy
        helm company lsp-mode lsp-ui ccls company-lsp
        dot-mode cmake-mode bison-mode markdown-mode yaml-mode protobuf-mode
        ein magit undo-tree sr-speedbar highlight-indent-guides yasnippet))

;;----------------------------------------------------------------------
;; Manage automatic installation of package.
;;
;; The following line works fine but it is quite slow when everithing
;; is already installed:
;;
;; (mapc #'package-install my-package-list)
;;
;; Prefer this (from stack overflow):
;;
;; https://stackoverflow.com/questions/10092322/
;; how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

(defun ensure-package-installed (package)
  (if (package-installed-p package) nil (package-install package)))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Install packages if needed
(mapcar #'ensure-package-installed my-package-list)


;;;========== Copy paste & mouse ===========================================
;; copy-paste to old default emacs option
;; http://www.emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)
;;(global-set-key (kbd "<mouse-2>") 'x-clipboard-yank)
(xterm-mouse-mode 1)


;;;========== various shortcuts ============================================
;; Rigid Indent region
(global-set-key [M-left]  'indent-rigidly-left)
(global-set-key [M-right] 'indent-rigidly-right)
;; display tabs
(global-set-key '[f3] 'whitespace-mode)
;; Align regex shortcut
(global-set-key '[f5] 'align-regexp)
;; window resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)


;;;========== General color ==================================================
(cond (window-system
       (set-foreground-color "wheat")
       (set-background-color "DarkSlateGray")
       (set-cursor-color "MediumOrchid")
       (set-mouse-color "MediumOrchid")
       (set-face-foreground 'menu "wheat")
       (set-face-background 'menu "DarkSlateGray")
       (set-face-background 'fringe "DarkSlateGray")
       (set-face-foreground 'region "white")
       (set-face-background 'region "SteelBlue")))

(setq frame-background-mode 'dark)

;; Various font settings depending on computer names
(cond ((equal system-name "high-res")
       (set-default-font "DejaVu Sans Mono-9:antialias=none"))
      (t (add-to-list 'default-frame-alist '(font . "-misc-fixed-medium-r-semicondensed-*-13-*-*-*-*-*-*-*"))))


;;;========== ediff =====================================================
(setq ediff-window-setup-function (quote ediff-setup-windows-plain))
(custom-set-faces
 '(ediff-odd-diff-A         ((t (:background "DimGray"))))
 '(ediff-odd-diff-Ancestor  ((t (:background "DimGray"))))
 '(ediff-odd-diff-B         ((t (:background "DimGray"))))
 '(ediff-odd-diff-C         ((t (:background "DimGray"))))
 '(ediff-even-diff-A        ((t (:background "DimGray"))))
 '(ediff-even-diff-Ancestor	((t (:background "DimGray"))))
 '(ediff-even-diff-B		((t (:background "DimGray"))))
 '(ediff-even-diff-C		((t (:background "DimGray")))))


;;;========== Highlight indentation ===========================================
(require 'highlight-indent-guides)
(global-set-key '[f4] 'highlight-indent-guides-mode)
;;(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-method 'column)
(set-face-background 'highlight-indent-guides-odd-face "#3f5f5f")
(set-face-background 'highlight-indent-guides-even-face "#4f6f6f")


;;;========== Undo Tree ===========================================
(global-undo-tree-mode)
(defun undo-tree-visualizer-update-linum (&rest args)
  (linum-update undo-tree-visualizer-parent-buffer))
(advice-add 'undo-tree-visualize-undo :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualize-redo :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualize-undo-to-x :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualize-redo-to-x :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualizer-mouse-set :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualizer-set :after #'undo-tree-visualizer-update-linum)

;;;========== Magit ===============================================
(add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))


;;;========== Hide Show ===========================================
(load-library "hideshow")
(global-set-key (kbd "M-+") 'hs-toggle-hiding)
(global-set-key (kbd "M-=") 'hs-hide-level)
(global-set-key (kbd "M-*") 'hs-show-all)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'verilog-mode-hook    'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)


;;;========= sr-speedbar =========================================================
(require 'sr-speedbar)
;;(setq sr-speedbar-right-side nil)
(setq speedbar-use-images nil) ; use text for buttons
;;(setq sr-speedbar-skip-other-window-p t)
(sr-speedbar-refresh-turn-off)
(global-set-key '[f2] 'speedbar-toggle-show-all-files)
(global-set-key '[f1] 'sr-speedbar-toggle)


;;;========= Yasnippet =========================================================
(yas-global-mode -1)


;;;========= RST Mode ==========================================================
(speedbar-add-supported-extension ".rst")
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
		("\\.rest$" . rst-mode)) auto-mode-alist))

(defun rst-mode-hook-setting ()
  ;; (setq frame-background-mode 'dark)
  (setq rst-slides-program "open -a Firefox")
  (setq indent-tabs-mode nil))
(add-hook 'rst-mode-hook 'rst-mode-hook-setting)


;;;========= Text mode === spelling ============================================
(speedbar-add-supported-extension ".txt")

(add-hook 'text-mode-hook
          (lambda ()
            (flyspell-mode 1)
            (turn-on-auto-fill)
            (setq fill-column 120)))

(setq ispell-program-name "aspell")
(setq flyspell-issue-welcome-flag nil)


;;========= Make ==================================================
(defun my-makefile-hook ()
  (setq show-trailing-whitespace t))
(add-hook 'makefile-mode-hook 'my-makefile-hook)
(speedbar-add-supported-extension ".make")


;;========= CMake ==================================================
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))
(speedbar-add-supported-extension ".cmake")


;;========= YAML ==================================================
(require 'yaml-mode)
(speedbar-add-supported-extension ".yml")
(speedbar-add-supported-extension ".yaml")
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;;========= Verilog ================================================
(setq verilog-mode-pkg-dir
      (concat (file-name-as-directory my-emacs-pkg-dir) "verilog-mode.el"))
(unless (file-exists-p verilog-mode-pkg-dir)
  (url-copy-file "https://www.veripool.org/ftp/verilog-mode.el" verilog-mode-pkg-dir))

(require 'verilog-mode)

(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(add-to-list 'auto-mode-alist '("\\.[ds]?vh?\\'" . verilog-mode))

(when (require 'pymacs nil 'noerror)
  (eval-after-load "pymacs" '(add-to-list 'pymacs-load-path "~/.emacs.d/packages/svmodule"))
  (pymacs-load "emacs" "svm-")
  (global-set-key (kbd "M-p M-w") 'svm-copy-module)
  (global-set-key (kbd "M-p M-r") 'svm-reverse-module)
  (global-set-key (kbd "M-p M-m") 'svm-paste-as-module)
  (global-set-key (kbd "M-p M-g") 'svm-paste-as-packages)
  (global-set-key (kbd "M-p M-i") 'svm-paste-as-instance)
  (global-set-key (kbd "M-p M-b") 'svm-paste-as-clockingblock)
  (global-set-key (kbd "M-p M-c") 'svm-paste-as-parameters)
  (global-set-key (kbd "M-p M-s") 'svm-paste-as-signals)
  (global-set-key (kbd "M-p M-o") 'svm-paste-as-logic)
  (global-set-key (kbd "M-p M-l") 'svm-paste-as-init-latch)
  (global-set-key (kbd "M-p M-a") 'svm-paste-as-init-wire)
  (global-set-key (kbd "M-p M-t") 'svm-paste-as-doc-table)
  (global-set-key (kbd "M-p M-y") 'svm-paste-as-yaml)
  (global-set-key (kbd "M-p M-x") 'svm-paste-as-pandaxml))

(defun my-verilog-hook ()
  (flyspell-prog-mode)
  (setq indent-tabs-mode nil)
  (setq highlight-indentation-offset 3)
  (setq verilog-indent-level 3)
  (setq verilog-indent-level-module 3)
  (setq verilog-indent-level-declaration 3)
  (setq verilog-indent-level-behavioral 3)
  (setq verilog-indent-level-directive 0)
  (setq verilog-case-indent 3)
  (setq verilog-auto-lineup 'assignment)
  (setq verilog-tab-always-indent t)
  (setq verilog-auto-newline nil)
  (setq verilog-auto-indent-on-newline nil)
  (setq verilog-auto-endcomments nil)
  (setq verilog-indent-begin-after-if nil))

(add-hook 'verilog-mode-hook 'my-verilog-hook)

(speedbar-add-supported-extension ".sv")
(speedbar-add-supported-extension ".v")
(speedbar-add-supported-extension ".do")
(speedbar-add-supported-extension ".xml")
(speedbar-add-supported-extension ".qip")
(speedbar-add-supported-extension ".sdc")


;;========= LangServer===========================================
(require 'lsp)
(add-hook 'python-mode-hook 'lsp)
(add-hook 'c++-mode-common-hook 'lsp)
(add-hook 'c-mode-common-hook 'lsp)

(custom-set-variables
 '(lsp-enable-indentation nil)
 '(lsp-enable-on-type-formatting nil)
 '(lsp-ui-sideline-enable nil))

(custom-set-faces
 '(lsp-ui-doc-background ((t (:background "Wheat"))))
 '(lsp-ui-sideline-global ((t (:background "black"))))
 '(lsp-ui-sideline-symbol ((t (:background "black" :foreground "grey" :box (:line-width -1 :color "grey") :height 0.99))))
 '(lsp-ui-sideline-symbol-info ((t (:background "black" :slant italic :height 0.99)))))


;;========= C/C++ ==================================================
(defun my-c-mode-hook ()
  (setq c-doc-comment-style '((c-mode    . javadoc)
                              (c++-mode  . javadoc)))
  (flyspell-prog-mode)
  (show-paren-mode 1)
  (setq indent-tabs-mode nil)
  (setq highlight-indentation-offset 4)
  (setq c-basic-offset 4)
  ;; (c-set-offset 'case-label '+)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'brace-list-open 0))

(add-hook 'c++-mode-common-hook 'my-c-mode-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;========= Smart tabs ==================================================
(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
    ad-do-it))

(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
        (indent-tabs-mode
         (save-excursion
           (beginning-of-line)
           (while (looking-at "\t*\\( +\\)\t+")
             (replace-match "" nil nil nil 1)))
         (setq tab-width tab-width)
         (let ((tab-width fill-column)
               (,offset fill-column)
               (wstart (window-start)))
           (unwind-protect
               (progn ad-do-it)
             (set-window-start (selected-window) wstart))))
        (t
         ad-do-it)))))

(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)


;;========= Google Protobuf=========================================
(speedbar-add-supported-extension ".proto")
(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
	  (lambda () (c-add-style "my-style" my-protobuf-style t)))


;;======== Flex/Bison ==============================================
(defun my-bison-hook ()
  (setq indent-tabs-mode nil)
  (setq bison-electric-greater-than-v nil)
  (setq c-indent-level 4)
  (setq tab-width 4))
(add-hook 'bison-mode-hook 'my-bison-hook)

(add-to-list 'auto-mode-alist '("\\.y\\'" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.yy\\'" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.ypp\\'" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.l\\'" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.ll\\'" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.lpp\\'" . bison-mode))
(speedbar-add-supported-extension ".y")
(speedbar-add-supported-extension ".yy")
(speedbar-add-supported-extension ".ypp")
(speedbar-add-supported-extension ".l")
(speedbar-add-supported-extension ".ll")
(speedbar-add-supported-extension ".lpp")


;;======== python-mode hook ========================================
(defun my-python-mode-hook ()
  (flyspell-prog-mode)
  (flycheck-mode)
  (setq indent-tabs-mode nil)
  (setq tab-width 4))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(elpy-enable)
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")


;;======== sh-mode hook ============================================
(defun my-sh-mode-hook ()
  (setq tab-width 4)
  (setq sh-basic-offset 4)
  (setq indent-tabs-mode nil))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)
(speedbar-add-supported-extension ".sh")


;;======== tcl-mode hook ============================================
(defun my-tcl-mode-hook ()
  (setq tab-width 4)
  (setq tcl-indent-level 4)
  (setq indent-tabs-mode nil))

(add-hook 'tcl-mode-hook 'my-tcl-mode-hook)
(speedbar-add-supported-extension ".tcl")


;;======= Perl mode =================================================
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

(defun my-perl-mode-hook ()
  (setq tab-width 4)
  (setq cperl-indent-level 4)
  (setq indent-tabs-mode nil))

(add-hook 'cperl-mode-hook 'my-perl-mode-hook)


;;======== css-mode hook ============================================
(speedbar-add-supported-extension ".css")


;;======== dot-mode hook ============================================
(speedbar-add-supported-extension ".dot")


;;======== plantuml-mode hook =========================================
(speedbar-add-supported-extension ".uml")


;;======== scss-mode hook ===========================================
(speedbar-add-supported-extension ".scss")


;;======== color in compilation buffer ==============================
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)


;;======== Whitespace line trimmer =================================
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;========= Upcase region and all matches ==========================
(defun upcase-region-and-matches (start end)
  "Upcase the selection and all strings that match"
  (interactive "r")
  (setq to-upcase (buffer-substring start end))
  (setq upcased (upcase to-upcase))
  (message "%s -> %s" to-upcase upcased)
  (goto-char 0)
  (while (search-forward to-upcase nil t) (replace-match upcased)))
(global-set-key '[f6] 'upcase-region-and-matches)
(put 'upcase-region 'disabled nil)


;;========== Latex ==================================================
;; Inverse search.
;; (require 'dbus)
;; (defun auctex-evince-inverse-sync (file linecol timestamp)
;;   (let ((buf (get-file-buffer (substring file 7)))
;;      (line (car linecol))
;;      (col (cadr linecol)))
;;     (if (null buf)
;;      (message "Sorry, %s is not opened..." file)
;;       (switch-to-buffer buf)
;;       (goto-line (car linecol))
;;       (unless (= col -1)
;;      (move-to-column col)))))

;; (dbus-register-signal
;;  :session nil "/org/gnome/evince/Window/0"
;;  "org.gnome.evince.Window" "SyncSource"
;;  'auctex-evince-inverse-sync)

;; (add-hook 'LaTeX-mode-hook
;;        (lambda ()
;;          (TeX-PDF-mode 1)
;;          (setq-default ispell-program-name "aspell")
;;          (flyspell-mode 1)
;;          (ispell-change-dictionary "english")
;;          (turn-on-auto-fill)
;;          (setq fill-column 120)
;;          ))
