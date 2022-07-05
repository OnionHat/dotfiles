;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Suleyman Boyar"
      user-mail-address "suleymanboyar02@gmail.com")

(defvar sb/dark-theme 'doom-sourcerer)
(defvar sb/light-theme 'doom-acario-light)
(defvar sb/fontsize 14)
(if (string= (system-name) "lenovo") (setq sb/fontsize 20))

(setq doom-theme sb/dark-theme
      display-line-numbers-type 'relative
      scroll-margin 8
      fancy-splash-image (concat doom-private-dir "splash.png")
      doom-font (font-spec :family "JetBrains Mono" :size sb/fontsize :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size sb/fontsize  :weight 'regular))

;; (require 'smart-tab "~/.doom.d/smart-tab.el")
;; (global-smart-tab-mode 1)

;; (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ; one line at a time
;; (setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling

;; (setq
;;       scroll-step 1
;;       scroll-conservatively 10000
;;       scroll-preserve-screen-position 1)

(setq-default fill-column 80
              tab-width 4
              c-basic-offset 4
              js2-basic-offset 4)

(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  "))) ; <-- added padding here
(custom-set-faces!
  '(mode-line :family "JetBrains Mono" :height 0.9)
  '(mode-line-inactive :family "JetBrains Mono" :height 0.9))

(defun toggle-light-dark-mode ()
  "Toggle between light and dark color scheme"
  (interactive)
  (if (eq (car custom-enabled-themes) sb/dark-theme)
      (consult-theme sb/light-theme)
    (consult-theme sb/dark-theme)))

;;; Transperancy
(defvar sb/frame-transparency 85)
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
;; (use-package orderless
;;   :config
;;   (setq completion-styles '(basic substring partial-completion flex)))

(use-package! company
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay nil))
;; (map! :mode compnay-mode-map :n "C-SPC" #'company-complete-common)
;; (use-)

;;; Evil-bindings
(defun sb/evil-up-center ()
  (interactive)
  (let (current-linenumber (line-number-at-pos)))
  (evil-scroll-up 0)
  (evil-scroll-line-to-center current-linenumber))

(defun sb/evil-down-center()
  (interactive)
  (let (current-linenumber (line-number-at-pos)))
  ;; (message "Line %s" current-linenumber))
  (evil-scroll-down 0)
  (evil-scroll-line-to-center current-linenumber))

(use-package! evil
  :config
  (setq evil-split-window-below t
        evil-vsplit-window-right t)
  (evil-global-set-key 'insert (kbd "C-v") 'clipboard-yank)
  ;; Dont remember why I did this...
  (evil-define-key 'normal 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'normal 'global (kbd "k") 'evil-previous-visual-line))

(map! :map prog-mode-map
      :n "C-u" #'sb/evil-up-center
      :n "C-d" #'sb/evil-down-center)

(if (string= (system-name) "lenovo")
    (mapc #'disable-mouse-in-keymap
          (list evil-insert-state-map)))

(setq evil-snipe-scope 'visible)
;;; LSP
;; (use-package! eglot
;;   :config
;;   (set-eglot-client! 'cc-mode '("ccls" "--init={\"index\": {\"threads\": 3}}")))

(set-eglot-client! 'cc-mode '("ccls" "--init={\"index\": {\"threads\": 3}}"))

;; ((javascript-mode
;;   . ((eglot-workspace-configureation
;;       . ((:javascript . (:format (:tabSize 4))))))))
(add-hook 'rjsx-mode-hook
          (lambda ()
            (setq eglot-workspace-configuration
                  '((:javascript . (:format (:tabSize 4)))))
            (eglot-ensure)))

(map! :map eglot-mode-map :n "TAB" 'completion-at-point)

(setq eldoc-echo-area-use-multiline-p nil
      eldoc-idle-delay 0)

;;; Vterm
(use-package! vterm
  :config
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes"))

;;; Hooks
(setq-hook! '(c-mode-hook c++-mode-hook java-mode-hook python-mode-hook html-mode-hook) tab-width 4)  ; C/C++, java, python
(setq-hook! '(c-mode-hook c++-mode-hook java-mode-hook) c-basic-offset 4)  ; C/C++, java
(setq-hook! '(html-mode-hook) sgml-basic-offset 4)
(setq-hook! '(rjsx-mode-hook) js2-basic-offset 4)
;; Latex
(setq +latex-viewers '(zathura))
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

;;; Buuufffer
(defun compilation-project-buffer ()
  (interactive)
  (display-buffer (get-buffer-create compilation-last-buffer)))

(use-package! consult
  :config
  (setq consult-buffer-filter '("\\` " "\\`\\*Flymake log\\*\\'" "\\`\\*Semantic SymRef\\*\\'" "\\`\\*tramp/.*\\*\\'")
        consult-buffer-sources '(consult--source-hidden-buffer
                                 consult--source-buffer
                                 consult--source-project-buffer))
  (map! :desc "Compilation buffer" :leader :n "b c" #'compilation-project-buffer))

;;; Verticallity
(map! :gni "C-<return>" #'nil)
(use-package! orderless
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package! flycheck
  :config
  (setq flycheck-relevant-error-other-file-show nil))

(use-package! flycheck-posframe
  :config
  (setq flycheck-posframe-warning-prefix "➤ "
        flycheck-posframe-info-prefix "➤ "
        flycheck-posframe-error-prefix "➤ "))

(map! :gvn "C-e" #'doom/forward-to-last-non-comment-or-eol)

(map! :map c-mode-map
      :i "TAB" 'indent-for-tab-command
      :n "TAB" 'evil-jump-item)

(setq-default indent-tabs-mode nil)

;;; GGTAGS
(use-package! ggtags
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode 1))))
  (setq ggtags-highlight-tag nil
        ggtags-enable-navigation-keys t)
  (map! :map ggtags-mode-map
        :n "M-." 'ggtags-find-tag-dwim
        :n "M-," 'ggtags-prev-mark))

;;; WINNER
(use-package! winner
  :config
  (map! :map winner-mode-map
        :g "C-c <left>" 'winner-undo
        :g "C-c <right>" 'winner-redo))

;;; Modes
(add-to-list 'load-path "~/.doom.d/plugins/")
;; yukc
;(require 'yuck-mode)
(autoload 'yuck-mode "yuck-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yuck\\'" . yuck-mode))



;; - `Load!' for loading external *.el files relative to this one
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
