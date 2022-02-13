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


(setq doom-theme 'doom-Iosvkem)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq display-line-numbers-type 'relative)
;;
;; Cutsom Font
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'regular)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 18 :weight 'regular))

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

(use-package! evil
  :config
  (evil-global-set-key 'insert (kbd "C-v") 'clipboard-yank)
  (evil-define-key 'normal 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'normal 'global (kbd "k") 'evil-previous-visual-line))

(use-package eshell
  :config
  (setq eshell-aliases-file "~/.doom.d/eshell-aliases"))


(setq +latex-viewers '(zathura))
(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")

;;; Calendar stuff
(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:ical-create-source "UiO" "https://minestudier.uio.no/api/calendar/7770dls5/schedule?version=1644543510330&locale=nb" "SteelBlue") ; UiO Timeplan
   )))


(map! :leader :desc "Calendar" :n "o c" #'my-open-calendar)
(map! :map cfw:calendar-mode-map :desc "Today" "t" #'cfw:navi-goto-today-command)
(map! :map cfw:calendar-mode-map :desc "2 Week view" "T" #'cfw:change-view-two-weeks)
(map! :desc "Toggle Transparency" :g "C-c t" #'toggle-transparency)

;;; DAP
(map! :map lsp-mode-map :localleader :desc "Dap Hydra" :n "d" #'dap-hydra)
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
