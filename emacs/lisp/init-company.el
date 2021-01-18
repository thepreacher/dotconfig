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

(use-package orderless
  :custom
  (completion-styles '(orderless))
  :config
  (setq orderless-component-separator " +"))

(use-package company
  :after orderless
  :init
  (add-hook 'after-init-hook 'global-company-mode t)
  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))

  (advice-add 'company-capf--candidates :around #'just-one-face)
  :bind (:map company-active-map
          ("RET" . nil)
          ("[return]" . nil)
          ("TAB" . company-complete-common)
          ("<tab>" . company-complete-common)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("M-." . company-show-location))
  :config
  ;; Make completions display twice as soon.
  (setq company-idle-delay 0)

  ;; Make completions display when you have only typed one character,
  ;; instead of three.
  (setq company-minimum-prefix-length 3)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t))

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




(provide 'init-company)
;;; init-company.el ends here
