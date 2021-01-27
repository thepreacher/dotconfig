;;; init-selectrum.el --- Config for selectrum       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Prescient is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :config
  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode 1)
  ;; The default settings seem a little forgetful to me. Let's try
  ;; this out.
  (setq prescient-history-length 1000))

(use-package selectrum
  :init
  (add-hook 'after-init-hook 'selectrum-mode)
  :config
  (use-package selectrum-prescient
    :config
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
    (selectrum-prescient-mode 1)
    (global-set-key [remap execute-extended-command] 'execute-extended-command))

  (use-package embark
    :bind (:map selectrum-minibuffer-map
            ("C-c C-o" . embark-export)
            ("C-c C-c" . embark-act-noexit))
          ("C-," . embark-act))

  (use-package consult
    :init
    ;; Optionally configure a function which returns the project root directory
    ;; (autoload 'projectile-project-root "projectile")
    (setq-default consult-project-root-function 'projectile-project-root)
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (("C-x M-:" . consult-complex-command)
           ("C-c h" . consult-history)
           ("C-c m" . consult-mode-command)
           ("C-x b" . consult-buffer)
           ("C-x 4 b" . consult-buffer-other-window)
           ("C-x 5 b" . consult-buffer-other-frame)
           ("C-x r x" . consult-register)
           ("C-x r b" . consult-bookmark)
           ("M-g g" . consult-goto-line)
           ("M-g M-g" . consult-goto-line)
           ("M-g o" . consult-outline)       ;; "M-s o" is a good alternative.
           ("M-g l" . consult-line)          ;; "M-s l" is a good alternative.
           ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
           ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
           ("M-g r" . consult-git-grep)      ;; or consult-grep, consult-ripgrep
           ("M-g f" . consult-find)          ;; or consult-locate, my-fdfind
           ("M-g i" . consult-project-imenu) ;; or consult-imenu
           ("M-g e" . consult-error)
           ;; ("M-s m" . consult-multi-occur)
           ("M-y" . consult-yank-pop)
           ("<help> a" . consult-apropos))
    :config
    (when (executable-find "rg")
      (global-set-key (kbd "M-?") 'consult-ripgrep))
    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)

    (use-package embark-consult
      :config
      (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode))

    (use-package consult-flycheck
      :bind (:map flycheck-command-map
              ("!" . consult-flycheck))))

  (use-package marginalia
    :init
    ;; (add-hook 'after-init-hook 'marginalia-mode)

    ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
    (advice-add #'marginalia-cycle :after
                (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

    (setq-default marginalia-annotators '(marginalia-annotators-heavy))
    :config
    (marginalia-mode)))




(provide 'init-selectrum)
;;; init-selectrum.el ends here
