;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Suleyman Boyar"
      user-mail-address "suleymanboyar02@gmail.com")

(use-package! mu4e
  :config
  (setq user-full-name "Suleyman Boyar"
        user-mail-address "suleymanboyar02@gmail.com"
        mu4e-get-mail-command "mbsync -c ~/.config/mu4e/mbsyncrc -a"
        mu4e-update-interval 300
        mu4e-compose-signature
        (concat
         "Mvh\n"
         "Suleyman M. Boyar")
        message-send-mail-function 'smtpmail-send-it
        mu4e-sent-folder "/Sent"
        mu4e-drafts-folder "/Draft"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/All Mail"
        mu4e-maildir-shortcuts
        '(("/suleymanboyar02/Inbox"	. ?i)
          ("/suleymanboyar02/Sent"	. ?s)
          ("/suleymanboyar02/All Mail"	. ?a)
          ("/suleymanboyar02/Trash"	. ?t))))

(defun mu4e-headers-mark-all-unread-read ()
  "Put a ! \(read) mark on all visible unread messages"
  (interactive)
  (mu4e-headers-mark-for-each-if
   (cons 'read nil)
   (lambda (msg param)
     (memq 'unread (mu4e-msg-field msg :flags)))))

(setq scroll-margin 8)
(setq doom-theme 'doom-sourcerer)
;;
;; Cutsom Font
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'regular)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 18 :weight 'regular))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq-default fill-column 80)

(use-package! org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Increase the size of various headings
(set-face-attribute 'org-document-title nil :font "JetBrainsMono Nerd Font" :weight 'bold :height 1.3)
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'medium :height (cdr face)))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil)



(setq display-line-numbers-type 'relative)
;;

;; Transpernat
(defvar sb/frame-transparency 90)

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

(use-package orderless
  :config
  (setq completion-styles '(basic substring partial-completion flex)))

(use-package! company
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0))

(use-package! evil
  :config
  (evil-global-set-key 'insert (kbd "C-v") 'clipboard-yank)
  (evil-define-key 'normal 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'normal 'global (kbd "k") 'evil-previous-visual-line))

(use-package! eshell
  :config
  (setq eshell-aliases-file "~/.doom.d/eshell-aliases"))

;; (use-package! company
;;   :config
;;   (setq company-backends '((company-capf company-dabbrev-code company-files))))

(setq +latex-viewers '(zathura))
(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")

;;; Calendar stuff
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

;;; Java
;; (setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)
;; (custom-set-variables
;;  '(tab-width 4))

(setq-default c-basic-offset 4)
(setq-default js2-basic-offset 4)

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; FillColumnIndicator
(use-package! fill-column-indicator
  :init
  (fci-mode)
  :config
  (setq fci-rule-column 80)
  (setq fci-rule-width 2)
  (setq fci-rule-color "#686858"))

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
