;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

;; Git / Magit

(use-package magit
  :commands (magit-git-repo-p magit-status magit-get-current-branch)
  :bind (("C-c g" . magit-file-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos :after magit)

(use-package fullframe
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window))

(use-package git-commit
  :after magit
  :hook ((git-commit-mode . my/git-commit-auto-fill-everywhere)
         (git-commit-mode . goto-address-mode))
  :init
  (setq git-commit-summary-max-length 50)
  :preface
  (defun my/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil)))


;; In addition to that, I like to see the lines that are being modified in the file while it is being edited.
(use-package git-gutter
  :defer 0.3
  :delight
  :init
  (global-git-gutter-mode))

;; Finally, one last package that I like to use with Git to easily see the changes made by previous commits.
(use-package git-timemachine
  :defer 1
  :delight)










;; General keybindings

;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
;; quickly open magit on any one of your projects.
(global-set-key [(meta f12)] 'magit-status)
;; Add a super-convenient global binding for magit-status
(global-set-key (kbd "C-M-;") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

(when *is-a-mac*
  (with-eval-after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))




(provide 'init-git)
;;; init-git.el ends here
