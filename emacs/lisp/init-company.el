;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation



;;; Code completion

;; Company - Auto completion system
;; smart tab behavior - indent or complete
;; (setq tab-always-indent 'complete)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
;; (add-to-list 'completion-styles 'initials t)

(defun nqa/company-complete-selection ()
  "Insert the selected candidate or the first if none are selected."
  (interactive)
  (if company-selection
      (company-complete-selection)
    (company-complete-number 1)))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))

  (advice-add 'company-capf--candidates :around #'just-one-face)
  :after lsp-mode
  ;; :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
          ("RET" . nil)
          ("[return]" . nil)
          ("TAB" . nqa/company-complete-selection)
          ("<tab>" . nqa/company-complete-selection)
          ("M-." . company-show-location))
  :config
  ;; Make completions display twice as soon.
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 3)
  ;; (setq company-tooltip-minimum company-tooltip-limit)
  ;; (setq company-frontends '(company-pseudo-tooltip-frontend))
  (setq company-show-numbers t)
  ;; (setq company-require-match #'company-explicit-action-p)
  (setq company-dabbrev-other-buffers nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t))

;; With use-package:
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :after company
  :config
  ;; Use `prescient' for Company menus.
  (company-prescient-mode 1)


  ;; Suspend page-break-lines-mode while company menu is active
  ;; (see https://github.com/company-mode/company-mode/issues/416)
  (with-eval-after-load 'page-break-lines
    (defvar-local sanityinc/page-break-lines-on-p nil)

    (defun sanityinc/page-break-lines-disable (&rest ignore)
      (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
      (when sanityinc/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
    (add-hook 'company-after-completion-hook 'sanityinc/page-break-lines-maybe-reenable)))


;; General keybindings
(global-set-key (kbd "M-C-/") 'company-complete)


(provide 'init-company)
;;; init-company.el ends here
