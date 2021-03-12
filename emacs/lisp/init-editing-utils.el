;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package unfill)

;;Dash - A modern list api for Emacs. No 'cl required.
(use-package dash :defer)
(use-package dash-functional :defer)


;; Super-save auto-saves your buffers, when certain events happen
(use-package super-save
  :init
  (add-hook 'after-init-hook 'super-save-mode)
  :config
  ;; (setq super-save-idle-duration 10)
  (setq super-save-auto-save-when-idle t)
  (setq super-save-remote-files nil)
  (setq super-save-exclude '(".gpg"))
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook))


;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)


;; Crux
(use-package crux
  :bind(("C-k" . crux-smart-kill-line)
        ("M-o" . crux-smart-open-line)
        ("M-O" . crux-smart-open-line-above)
        ("C-c n" . crux-cleanup-buffer-or-region)
        ("C-c e" . crux-eval-and-replace)
        ("C-c D" . crux-delete-file-and-buffer)
        ("C-c d" . crux-duplicate-current-line-or-region)
        ("C-c r" . crux-rename-file-and-buffer)
        ("C-c f" . crux-recentf-find-file)
        ([remap kill-whole-line] . crux-kill-whole-line)
        ([remap move-beginning-of-line] . crux-move-beginning-of-line)
        ("C-<backspace>" . crux-kill-line-backwards)))


;; Page break lines
(use-package page-break-lines
  :init
  (add-hook 'after-init-hook 'global-page-break-lines-mode))


;;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :init
  (add-hook 'after-init-hook 'whole-line-or-region-global-mode))


;; Move text
;; Moves the current line (or if marked, the current region’s, whole lines).
(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))


;; Aggressive-indent
(use-package aggressive-indent
  :hook ((css-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (js-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode))
  :custom (aggressive-indent-comments-too))


;; Highlight indentation

;; Highlight the current line
(use-package hl-line
  :init
  (add-hook 'after-init-hook 'global-hl-line-mode t))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character))

(use-package highlight-symbol
  :delight
  :hook (prog-mode . highlight-symbol-mode)
  :custom-face
  (highlight-symbol-face ((t (:background "#44475a")))) ;; Good for Doom themes
  :custom
  (highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-operators
  :hook (prog-mode . highlight-operators-mode))

(use-package highlight-escape-sequences
  :init
  (add-hook 'after-init-hook 'hes-mode))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :hook (prog-mode . volatile-highlights-mode))

;; Which key
(use-package which-key
  :init
  (add-hook 'after-init-hook 'which-key-mode)
  :config
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.4))


;; Helpful - A better emacs help buffer
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))



;; Electric pair
(use-package elec-pair
  :config
  (electric-pair-mode +1))

;; electric-operator
;; electric-operator is an emacs minor-mode to automatically add spacing around operators.
(use-package electric-operator
  :delight
  :hook (python-mode . electric-operator-mode))

;; Undo
(use-package undo-tree
  :delight
  :bind ("C--" . undo-tree-redo)
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))


(use-package browse-kill-ring
  :init
  (setq browse-kill-ring-separator "\f")
  :bind (("M-Y" . browse-kill-ring)
         :map browse-kill-ring-mode-map
         ("C-g" . browse-kill-ring-quit)
         ("M-n" . browse-kill-ring-forward)
         ("M-p" . browse-kill-ring-previous))
  :config
  (with-eval-after-load 'page-break-lines
    (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode)))


(use-package symbol-overlay
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind (("M-i" . symbol-overlay-put)
         ("M-I" . symbol-overlay-remove-all)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)))

(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview)
  :config
  (when (fboundp 'display-line-numbers-mode)
    (defun sanityinc/with-display-line-numbers (f &rest args)
      (let ((display-line-numbers t))
        (apply f args)))
    (advice-add 'goto-line-preview :around #'sanityinc/with-display-line-numbers)))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Point and Region
;; Increase region by semantic units. It tries to be smart about it and adapt to the structure of the current major mode.
;; If you expand too far, you can contract the region by pressing - (minus key),
;; or by prefixing the shortcut you defined with a negative argument: C-- C-=.
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package multiple-cursors
  ;; multiple-cursors
  :bind(("C-<" . mc/mark-previous-like-this)
        ("C->" . mc/mark-next-like-this)
        ("C-+" . mc/mark-next-like-this)
        ("C-c C-<" . mc/mark-all-like-this)))

(use-package beacon
  :init
  (add-hook 'after-init-hook 'beacon-mode)
  :config
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20))


;; hungry-delete
;; Deleting a whitespace character will delete all whitespace until the next non-whitespace character.
(use-package hungry-delete
  :init
  (add-hook 'after-init-hook 'global-hungry-delete-mode))


(setq-default show-trailing-whitespace nil)


;;; Whitespace

(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'sanityinc/show-trailing-whitespace))


(use-package whitespace-cleanup-mode
  :init
  (add-hook 'after-init-hook 'global-whitespace-cleanup-mode))

(global-set-key [remap just-one-space] 'cycle-spacing)

;; Smartparens
(use-package smartparens
  :commands smartparens-global-mode
  :hook (prog-mode-hook markdown-mode-hook)
  :config
  ;; smart pairing for all
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings))

;; General keybindings

;; M-^ is inconvenient, so also bind M-j
(global-set-key (kbd "M-j") 'join-line)




(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
