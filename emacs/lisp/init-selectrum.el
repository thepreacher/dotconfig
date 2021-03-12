;;; init-selectrum.el --- Config for selectrum       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Prescient is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :config
  (prescient-persist-mode 1))

;; Selectrum is a better solution for incremental narrowing
(use-package selectrum
  :init
  (add-hook 'after-init-hook 'selectrum-mode)
  :config
  ;; (selectrum-mode +1)
  (use-package selectrum-prescient
    :requires prescient
    :config
    ;; to make sorting and filtering more intelligent
    (selectrum-prescient-mode +1)
    (global-set-key [remap execute-extended-command] 'execute-extended-command)))

;; Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ("M-g e" . consult-error)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)                  ;; alt. consult-locate, find-fd
         ("M-s g" . consult-git-grep)              ;; alt. consult-grep
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         (:map isearch-mode-map
           ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
           ("M-s l" . consult-line)))                 ;; required by consult-line to detect isearch

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Custom command wrappers. It is generally encouraged to write your own
  ;; commands based on the Consult commands. Some commands have arguments which
  ;; allow tweaking. Furthermore global configuration variables can be set
  ;; locally in a let-binding.
  (defun find-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial)))

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds zebra stripes, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure a function which returns the project root directory.
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  (when (executable-find "rg")
    (defun sanityinc/consult-ripgrep-at-point (&optional dir initial)
      (interactive (list prefix-arg (let ((s (symbol-at-point)))
                                      (when s (symbol-name s)))))
      (consult-ripgrep dir initial)))
  (global-set-key (kbd "M-?") 'sanityinc/consult-ripgrep-at-point)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame))

(nqa/leader-key-def
  "b"   '(:ignore t :which-key "buffer")
  "bs"  'consult-buffer)


;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  ;; (marginalia-mode)
  (add-hook 'after-init-hook 'marginalia-mode))



;; Embark
(use-package embark
  :bind
  ("C-S-a" . embark-act))              ; pick some comfortable binding

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))





(provide 'init-selectrum)
;;; init-selectrum.el ends here
