;; -*- lexical-binding: t; -*-

;;; Preference
;; Put auto gen custom func in seperate file
(defconst custom-file (expand-file-name "~/.emacs.d/custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
; (setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;; Put backup files in a comman place
(setq backup-directory-alist `(("." . "~/.local/share/emacs/backup")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;;; Seting up package repo
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; use-package and other dependencies
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)

;;; Fonts
;; Setting up default fonts
(defun sb/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-13")
   ((eq system-type 'gnu/linux) "JetBrainsMono Nerd Font-14")))

(add-to-list 'default-frame-alist `(font . ,(sb/get-default-font)))

(defun sb/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                         (lambda (i) (string-equal (car i) block-name))
                         unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block))))

;; Setting up glypgs and unicodes
(use-package unicode-fonts
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config
  ;; Fix the font mappings to use the right emoji font
  (mapc
    (lambda (block-name)
      (sb/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
    '("Dingbats"
      "Emoticons"
      "Miscellaneous Symbols and Pictographs"
      "Transport and Map Symbols"))
  (unicode-fonts-setup))

;;; Theme
(use-package doom-themes
  :config (load-theme 'doom-Iosvkem t))

;;; Evil
(global-set-key (kbd "C-M-u") 'universal-argument)        ; rebinding default C-u
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)   ; ESC Cancels All

;; add undo capabilities to evil
(use-package undo-tree
  :init
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1)
  :diminish undo-tree-mode)

;; Vi like bindings
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)

  (evil-mode 1))

(use-package evil-collection
  :after evil
  ;; :init (setq evil-collection-company-use-tng nil)
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (delete 'lispy evil-collection-mode-list)
  (delete 'org-present evil-collection-mode-list)
  (evil-collection-init))


;; Tpopes commentary plugin
(use-package evil-commentary
  :init (evil-commentary-mode 1)
  :diminish evil-commentary-mode)

;; Tpopes surrond plugin :)
(use-package evil-surround
  :hook (prog-mode . evil-surround-mode))

;; General
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer sb/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer sb/normal-key-def
    :keymaps 'normal)

  (general-create-definer sb/insert-key-def
    :keymaps 'insert)

  (general-create-definer sb/visual-key-def
    :keymaps 'visual)

  (general-create-definer sb/ctrl-c-keys
    :prefix "C-c"))

;; Some vim bindings
(sb/visual-key-def
  "J" (kbd ":m'>+1 RET gv=gv")
  "K" (kbd ":m'<-2 RET gv=gv")
  "L" (kbd ">gv"))
  "H" (kbd "<gv")


(sb/normal-key-def
  "SPC eb"  'eval-buffer)

(sb/visual-key-def
  "SPC er" 'eval-region)

;;; User Interface
(setq inhibit-startup-message t)
(scroll-bar-mode -1)	; disable visible scrollbar
(tool-bar-mode -1)		; disable toolbar
(tooltip-mode -1)		; disable tooltips
(set-fringe-mode 10)	; give some breathing room

(setq bar-adjust-thumb-portion nil)
(setq scroll-margin 8)
(menu-bar-mode -1)		; disable the menu bar
(setq visible-bell nil)	; disable visible-bell

;; Better scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed t) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

;; Transpernacy
(defvar sb/frame-transparency 85)

(set-frame-parameter (selected-frame) 'alpha `(,sb/frame-transparency . ,sb/frame-transparency))
(add-to-list 'default-frame-alist `(alpha . (,sb/frame-transparency . ,sb/frame-transparency)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defun set-transparency (num)
  (interactive "nLevel: ")
  (setq sb/frame-transparency num)
  (set-frame-parameter nil 'alpha `(,sb/frame-transparency . ,sb/frame-transparency)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         `(,sb/frame-transparency . ,sb/frame-transparency) '(100 . 100)))))
 (global-set-key (kbd "C-c t") 'toggle-transparency)

;; Line number
;;Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1)
		   (setq display-line-numbers-type 'relative))))

;; Rainbow Delimeter
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Rainbow Mode
(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))

;;; Compile
;; (use-package compile

;;; Projects
;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/uio")
    (setq projectile-project-search-path '("~/uio")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Dasboard
; dependencies
(use-package page-break-lines)
(use-package all-the-icons)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Ello My EMacs")
  (setq dashboard-startup-banner "/home/sully/.emacs.d/dasboard-banner/hydra.txt")
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents . 5) (projects . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))


;;; Terminal
;; Vterm
(use-package vterm
  :config
  (sb/normal-key-def "SPC t" 'vterm))

;;; Dired
(require 'dired)
 (evil-collection-define-key 'normal 'dired-mode-map
     "h" 'dired-single-up-directory
     "H" 'dired-omit-mode
     "l" 'dired-single-buffer
     "n" 'dired-create-empty-file
     "y" 'dired-ranger-copy
     "X" 'dired-ranger-move
     "p" 'dired-ranger-paste)


;;; Mode Line
;; Basic Customization
;; (setq display-time-format "%l:%M %p %b %y"
;;       display-time-default-load-average nil)

;; Doom Modeline
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  ;; :init (doom-modeline-mode 1)
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))


;;; Completion
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-selection-wrap-around t)
  (setq company-require-match nil)
  (sb/insert-key-def
    "C-n" 'company-select-next
    "C-p" 'company-select-previous
    "C-e" 'company-abort))

;; Auto save
(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;;; Auto revert changed file
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;;; Tramp
;; Set default connection mode to SSH
(setq tramp-default-method "ssh")


;;; M-x Package
;; Ivy
(use-package ivy
  :init (ivy-mode)
  :bind (:map ivy-minibuffer-map
              ("<return>" . ivy-alt-done))
  :config
  (setq ivy-re-builders-alist
	'((swiper . ivy--regex-plus)
	  (t . ivy--regex-fuzzy))))

(use-package counsel
  :init (counsel-mode))

;;; Treesitter
;; (use-package tree-sitter
;;   :init (tree-sitter-mode))

;;; File Specific Settings
;;; Whitespace mode
(defun sb/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'tuareg-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'c++-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'c-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode 'sb/set-up-whitespace-handling)
(add-hook 'java-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'rust-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'scala-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'haskell-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'erlang-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'asm-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'nasm-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'nim-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'sb/set-up-whitespace-handling)
(add-hook 'porth-mode-hook 'sb/set-up-whitespace-handling)

;; Language Server Protocol
(use-package lsp-mode
  :config
  (sb/normal-key-def
  ;; "SPC l"  '(:ignore t :which-key "lsp")
  "SPC ld" 'xref-find-definitions
  "SPC lr" 'xref-find-references
  "SPC ln" 'lsp-ui-find-next-reference
  "SPC lp" 'lsp-ui-find-prev-reference
  "SPC ls" 'counsel-imenu
  "SPC le" 'lsp-ui-flycheck-list
  "SPC lS" 'lsp-ui-sideline-mode
  "SPC lX" 'lsp-execute-code-action))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

;; DAP
(use-package dap-mode
  :after lsp-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))

;; Meta Lisp
;; (use-package lispy
;;   :hook ((emacs-lisp-mode . lispy-mode)
;;          (scheme-mode . lispy-mode)))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))



;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))


;; C/C++ Objective-C Cuda
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
	 (lambda () (require 'ccls) (lsp))))

;; Java
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook #'lsp))

;; Python
(use-package lsp-jedi
  :config
  (add-hook 'python-mode #'lsp))
  ;; (with-eval-after-load "lsp-mode"
  ;;   (add-to-list 'lsp-disabled-clients 'pyls)
  ;;   (add-to-list 'lsp-enabled-clients 'jedi)))

(use-package lsp-ltex
  :config
  (add-hook 'latex-mode-hook #'lsp))

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun sb/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun dw/markdown-mode-hook ()
    (dw/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'sb/markdown-mode-hook))
