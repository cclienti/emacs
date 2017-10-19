;;========== General ==================================================
;; Suppression du message d'accueil
(setq inhibit-startup-message t)

;; Increase emacs eval depth buffer
(setq max-lisp-eval-depth 10000)

;; window name
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

;; emacs mode dir
(add-to-list 'load-path "~/.emacs.d/packages")

;; load speedbar
;;(require 'speedbar)

;; Pas d'antialiasing
(setq mac-allow-anti-aliasing nil)

;; Speed bar shortcut
;;(add-hook 'speedbar-before-popup-hook (global-set-key '[f1] 'speedbar))
;;(global-set-key '[f1] 'speedbar-get-focus)
(global-set-key [M-down] 'next-buffer)
(global-set-key [M-up]  'previous-buffer)

;; Comment region
(global-set-key "\C-c c" 'comment-region)

;; Rigid Indent region
(global-set-key [M-left]  'indent-rigidly-left)
(global-set-key [M-right] 'indent-rigidly-right)

;; linum except in speedbar
(setq linum-format " %4d \u2502 ")
(add-hook 'find-file-hook (lambda () (linum-mode 1)))

;; column number
(column-number-mode t)

;; display tabs
(global-set-key '[f3] 'whitespace-mode)
(setq whitespace-line-column 120)
;; display only tabs in whitespace-mode
;; side effect: it prevents the whitespace-cleanup to work properly
;;(setq whitespace-style '(tabs tab-mark)) ;turns on white space mode only for tabs

;; line wrap
(set-fill-column 119)

;; prevent emacs to create backup files
(setq make-backup-files nil)

;; pas de bip
(setq ring-bell-function 'ignore)

;; no scroll bar
(scroll-bar-mode -1)

;; copy-paste to old default emacs option
;; http://www.emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)
;;(global-set-key (kbd "<mouse-2>") 'x-clipboard-yank)

;; default tab size
;; (setq default-tab-width 4)

(global-set-key '[f5] 'align-regexp)

;; Disable compilation warning regarding
(setq byte-compile-warnings '(not free-vars ))

;; Disable backup files
(setq make-backup-files nil)

;; Preserve screen position while scrolling to avoid weird problems
(setq scroll-preserve-screen-position t)

;; Disable eldoc
; (global-eldoc-mode -1)

;;;========== Proxy =====================================================
;; (setq url-proxy-services
;;       '(("http"     . "http://localhost:3128")
;;         ("https"    . "http://localhost:3128")
;;         ("ftp"      . "http://localhost:3128")
;;         ("no_proxy" . "^\\(localhost\\|10.*\\)")))

;;;========== Packages ==================================================
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			   ;;("marmalade" . "https://marmalade-repo.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

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

;;(add-to-list 'default-frame-alist '(font . "-misc-fixed-medium-r-semicondensed-*-13-*-*-*-*-*-*-*"))
(setq frame-background-mode 'dark)

;; Various font settings depending on computer names
(if (equal system-name "fixe")
    (set-default-font "DejaVu Sans Mono-9:antialias=none")
  (if (equal system-name "laptop.home")
      (add-to-list 'default-frame-alist '(font . "-misc-fixed-medium-r-semicondensed-*-13-*-*-*-*-*-*-*"))
    (set-default-font "DejaVu Sans Mono-10:antialias=none")))


;;;========== Highlight indentation ===========================================
(require 'highlight-indent-guides)
(global-set-key '[f4] 'highlight-indent-guides-mode)
;;(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-method 'column)
(set-face-background 'highlight-indent-guides-odd-face "#3f5f5f")
(set-face-background 'highlight-indent-guides-even-face "#4f6f6f")

;;;========= sr-speedbar =========================================================
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq speedbar-use-images nil) ; use text for buttons
(sr-speedbar-refresh-turn-off)
(global-set-key '[f2] 'speedbar-toggle-show-all-files)
(global-set-key '[f1] 'sr-speedbar-toggle)


;;;========= minimap =========================================================
;;(load "~/.emacs.d/packages/minimap.el")
;;(require 'minimap)
;;(setq minimap-window-location 'right)
;;(global-set-key '[f4] 'minimap-mode)


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
                (setq-default ispell-program-name "aspell")
            (flyspell-mode 1)
            (ispell-change-dictionary "english")
            (turn-on-auto-fill)
            (setq fill-column 120)))

(setq ispell-program-name "aspell")
(setq ispell-dictionary   "english")
(setq fill-column 120)

(setq flyspell-issue-welcome-flag nil)
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

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
(load "~/.emacs.d/packages/verilog-mode.el")
(require 'verilog-mode)

(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(add-to-list 'auto-mode-alist '("\\.[ds]?vh?\\'" . verilog-mode))

(require 'pymacs)
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path "~/.emacs.d/packages/svmodule"))
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
(global-set-key (kbd "M-p M-x") 'svm-paste-as-pandaxml)

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

;;========= GLSL ===================================================
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(speedbar-add-supported-extension ".glsl")
(speedbar-add-supported-extension ".vert")
(speedbar-add-supported-extension ".frag")
(speedbar-add-supported-extension ".geom")


;;========= C/C++ ==================================================
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags")
(require 'package)
(package-initialize)
(require 'rtags)
(require 'company)
(require 'flycheck-rtags)

(defun rtags-c-mode-hook ()
        (setq rtags-autostart-diagnostics t)
        (rtags-diagnostics)
        (setq rtags-completions-enabled t)
        (push 'company-rtags company-backends)
        (flycheck-select-checker 'rtags))

(defun my-c-mode-hook ()
  (setq c-doc-comment-style '((c-mode    . javadoc)
			      (c++-mode  . javadoc)))
  (flyspell-prog-mode)
  (show-paren-mode 1)
  (setq highlight-indentation-offset 3)
  (setq c++-tab-always-indent 1)
  (setq c-indent-level 3)
  (setq tab-width 3)
  (setq indent-tabs-mode t)
  (global-company-mode)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))

(add-hook 'c++-mode-common-hook 'rtags-c-mode-hook)
(add-hook 'c-mode-common-hook 'rtags-c-mode-hook)
;;(add-hook 'c++-mode-common-hook 'global-flycheck-mode)
;;(add-hook 'c-mode-common-hook 'global-flycheck-mode)
(add-hook 'c++-mode-common-hook 'my-c-mode-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
(define-key c-mode-base-map (kbd "M-.") (function rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,") (function rtags-find-references-at-point))


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
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (python-guess-indent nil)
  (python-indent 4))

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'global-flycheck-mode)


;;======== sh-mode hook ============================================
(defun my-sh-mode-hook ()
  (setq tab-width 3)
  (setq sh-basic-offset 3)
  (setq indent-tabs-mode nil))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)
(speedbar-add-supported-extension ".sh")


;;======== tcl-mode hook ============================================
(defun my-tcl-mode-hook ()
  (setq tab-width 3)
  (setq tcl-indent-level 3)
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

;;========== Custom Set Var ==========================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck glsl-mode company-jedi markdown-mode dot-mode bison-mode yasnippet yaml-mode sr-speedbar jedi highlight-indent-guides helm company cmake-mode))))
