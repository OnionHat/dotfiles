;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Suleyman Boyar"
      user-mail-address "suleymanboyar02@gmail.com")


(defvar sb/dark-theme 'doom-sourcerer)
(defvar sb/light-theme 'doom-acario-light)
(defvar sb/fontsize 14)
(if (string= (system-name) "lenovo") (setq sb/fontsize 16))

(setq doom-theme sb/dark-theme
      display-line-numbers-type 'relative
      scroll-margin 8
      doom-font (font-spec :family "JetBrains Mono" :size sb/fontsize :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size sb/fontsize :weight 'regular))

(setq-default fill-column 80
              tab-width 4
              c-basic-offset 4
              js2-basic-offset 4)

(defun toggle-light-dark-mode ()
  "Toggle between light and dark color scheme"
  (interactive)
  (if (eq (car custom-enabled-themes) sb/dark-theme)
      (consult-theme sb/light-theme)
    (consult-theme sb/dark-theme)))

;;; Transperancy
(defvar sb/frame-transparency 85)

(set-frame-parameter (selected-frame) 'alpha `(,sb/frame-transparency . ,sb/frame-transparency))
(add-to-list 'default-frame-alist `(alpha . (,sb/frame-transparency . ,sb/frame-transparency)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;;; Org-mode
(setq org-directory "~/org/")

;;; Search
(use-package orderless
  :config
  (setq completion-styles '(basic substring partial-completion flex)))

(use-package! company
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0))

;;; Evil-bindings
(use-package! evil
  :config
  (evil-global-set-key 'insert (kbd "C-v") 'clipboard-yank)
  (evil-define-key 'normal 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'normal 'global (kbd "k") 'evil-previous-visual-line))

;; ;;; Vterm
(use-package! vterm
  :config
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes"))

;;; Latex
(setq +latex-viewers '(zathura))

;;; Calendar
(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:ical-create-source "Personelig" "https://calendar.google.com/calendar/u/0?cid=c3VsZXltYW5ib3lhcjAyQGdtYWlsLmNvbQ" "goldenrod") ; UiO Timeplan
    (cfw:ical-create-source "UiO" "https://minestudier.uio.no/api/calendar/7770dls5/schedule?version=1644543510330&locale=nb" "SteelBlue") ; UiO Timeplan
    )))
;; First day of the week
(setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday

;; keybindings for calendar-mode
(map! :leader :desc "Calendar" :n "o c" #'my-open-calendar)
(map! :map cfw:calendar-mode-map :desc "Today" "t" #'cfw:navi-goto-today-command)
(map! :map cfw:calendar-mode-map :desc "2 Week view" "T" #'cfw:change-view-two-weeks)
(map! :desc "Toggle Transparency" :g "C-c t" #'toggle-transparency)

;;; DAP
(map! :map lsp-mode-map :localleader :desc "Dap Hydra" :n "d" #'dap-hydra
      :map lsp-mode-map :leader :desc "Breakpoint" :n "db" #'dap-breakpoint-toggle
      :map lsp-mode-map :leader :desc "Debug" :n "dd" #'dap-debug)
(use-package! dap-mode
  :config
  (dap-register-debug-template
   "Java Run Configuration"
   (list :name "Java Run Configuration"
         :type "java"
         :request "launch"
         :args ""
         :vmArgs "--enable-preview"
         :cwd nil
         :stopOnEntry :json-false
         :host "localhost"
         :request "launch"
         :modulePaths []
         :classPaths nil
         :projectName nil
         :mainClass nil)))

;;; Hooks
(setq-hook! '(c-mode-hook c++-mode-hook java-mode-hook python-mode-hook) tab-width 4)  ; C/C++, java, python
(setq-hook! '(c-mode-hook c++-mode-hook java-mode-hook) c-basic-offset 4)  ; C/C++, java

;; PDF
(map! :map pdf-view-mode-map :n "drag-mouse-1" nil)
(add-hook 'pdf-view-mode-hook #'pdf-view-themed-minor-mode) ; pdf
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;;; Tree-sitter
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;;; FillColumnIndicator
(use-package! fill-column-indicator
  :hook
  (text-mode . fci-mode)
  :config
  (setq fci-rule-column 80)
  (setq fci-rule-width 2)
  (setq fci-rule-color "#686858"))

;;; Higlight-indent-guides
(use-package! highlight-indent-guides
  :config
  (setq highlight-indent-guides-auto-odd-face-perc 20)
  (setq highlight-indent-guides-auto-even-face-perc 20)
  (setq highlight-indent-guides-auto-character-face-perc 30))

(map! :desc "Swithc buffer" :n "SPC b b" #'+vertico/switch-workspace-buffer)
(map! :gni "C-<return>" #'nil)

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq eldoc-echo-area-use-multiline-p nil)
;; (python-mode
;;  . ((eglot-workspace-configuration
;;      . ((:pylsp . (:plugins (:mypy-ls (:enable t) (:strict t))))))))
(after! flycheck-mode
  (setq flycheck-relevant-error-other-file-show nil))
(setq eglot-workspace-configuration
      '((pylsp
         (plugins
          (mypy-ls
           (enabled . t))
          (mypy-ls
           (strict . t))
          ))))

;; (doom-moddeline--set-font-widths doom-modeline-rhs-icons-alist)
;; (setq all-the-icons-scale-factor 1.2)
;; (add-hook! 'doom-modeline-mode-hook
;;   (let ((char-table char-width-table))
;;     (while (setq char-table (char-table-parent char-table)))
;;     (dolist (pair doom-modeline-rhs-icons-alist)
;;       (let ((width 2)  ; <-- tweak this
;;             (chars (cdr pair))
;;             (table (make-char-table nil)))
;;         (dolist (char chars)
;;           (set-char-table-range table char width))
;;         (optimize-char-table table)
;;         (set-char-table-parent table char-table)
;;         (setq char-width-table table)))))
;; (custom-set-faces!
;;   '(mode-line :family "JetBrainsMono Nerd Font" :height 0.9)
;;   '(mode-line-inactive :family "JetBrainsMono Nerd Font" :height 0.9))
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
