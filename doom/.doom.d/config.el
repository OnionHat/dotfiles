;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Suleyman Boyar"
      user-mail-address "suleymanboyar02@gmail.com")

(defvar sb/dark-theme 'doom-sourcerer)
(setq doom-theme sb/dark-theme
      display-line-numbers-type 'relative
      scroll-margin 8
      fancy-splash-image (concat doom-private-dir "splash.png")
      doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 14 :weight 'regular))

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
      (setq doom-theme 'doom-one-light)
    (message "set to dark mode")
    (setq doom-theme sb/dark-theme)))

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

(use-package! company
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay nil))
;; (map! :mode compnay-mode-map :n "C-SPC" #'company-complete-common)
;; (use-)

;;; Evil-bindings
(use-package! evil
  :config
  (setq evil-split-window-below t
        evil-vsplit-window-right t)
  (evil-global-set-key 'insert (kbd "C-v") 'clipboard-yank)
  ;; Dont remember why I did this...
  (evil-define-key 'normal 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'normal 'global (kbd "k") 'evil-previous-visual-line))

;;; LSP
;; (use-package! eglot
;;   :config
;;   (set-eglot-client! 'cc-mode '("ccls" "--init={\"index\": {\"threads\": 3}}")))

(set-eglot-client! 'cc-mode '("ccls" "--init={\"index\": {\"threads\": 3}}"))

(setq eldoc-echo-area-use-multiline-p nil
      eldoc-idle-delay 0)

;;; Vterm
(use-package! vterm
  :config
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes"))

;;; Hooks
(setq-hook! '(c-mode-hook c++-mode-hook java-mode-hook python-mode-hook) tab-width 4)  ; C/C++, java, python
(setq-hook! '(c-mode-hook c++-mode-hook java-mode-hook) c-basic-offset 4)  ; C/C++, java

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

(map! :gvn "C-e" #'move-end-of-line)

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
