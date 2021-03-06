;;; init-lsp.el --- Support for the Language Servers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Language Server Protocol
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (
          ((java-mode
             python-mode
             go-mode
             js-mode
             js2-mode
             typescript-mode
             web-mode
             c-mode
             cpp-mode
             objc-mode
             elixir-mode
             elm-mode
             rust-mode
             haskell-literate-mode
             haskell-mode) . lsp-deferred)
          (lsp-mode . lsp-enable-which-key-integration)
          (lsp-mode . (lambda ()
                        (add-hook 'before-save-hook 'lsp-format-buffer nil t))))
  :init
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-print-io nil ;; Set to t when debugging lsp errors
        lsp-lens-enable nil
        lsp-idle-delay 0.5
        lsp-modeline-diagnostics-enable t
        lsp-modeline-diagnostics-scope :workspace
        lsp-restart 'auto-restart
        lsp-eldoc-enable-hover t ;; Eldoc
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-enable-file-watchers t
        lsp-enable-semantic-highlighting t
        lsp-headerline-breadcrumb-enable nil ;; Don't have any for this now.
        lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (setq lsp-completion-enable-additional-text-edit nil)
  ;; file watcher directories to ignore
  (defvar nqa/ignored-directories '("[/\\\\]\\.elixir_ls\\'" ;; elixir lang server directory
                                    "[/\\\\]_build\\'" ;; elixir build directory
                                    "[/\\\\]assets\\'" ;; elixir assets directory
                                    "[/\\\\]deps" ;; elixir deps directory
                                    "[/\\\\]elm-stuff\\'")) ;; elm packages directory

  (dolist (re nqa/ignored-directories)
    (push re lsp-file-watch-ignored-directories))

  ;; don't scan 3rd party javascript libraries
  (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored-files)) ; json


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t ;; * Show only hover symbols
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-max-height 20
        lsp-ui-doc-max-width 80
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-position 'bottom)
  :bind (:map lsp-ui-mode-map
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references] . lsp-ui-peek-find-references)
          ("C-c u" . lsp-ui-imenu)))



(use-package dap-mode
  :after lsp-mode
  :bind
  (:map dap-mode-map
    (("<f12>" . dap-debug)
     ("<f8>" . dap-continue)
     ("<f9>" . dap-next)
     ("<M-f11>" . dap-step-in)
     ("C-M-<f11>" . dap-step-out)
     ("<f7>" . dap-breakpoint-toggle)))
  :hook  ((python-mode . (lambda () (require 'dap-python)))
          (haskell-mode . (lambda () (require 'dap-haskell)))
          (rust-mode . (lambda () (require 'dap-gdb-lldb)))
          (ruby-mode . (lambda () (require 'dap-ruby)))
          (go-mode . (lambda () (require 'dap-go)))
          (java-mode . (lambda () (require 'dap-java)))
          ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
          (php-mode . (lambda () (require 'dap-php)))
          (elixir-mode . (lambda () (require 'dap-elixir)))
          ((js-mode js2-mode typescript-mode) . (lambda () (require 'dap-firefox))))
  :config
  (dap-mode 1)
  (use-package dap-ui
    :ensure nil
    :config
    (dap-ui-mode t))
  (dap-tooltip-mode t)
  (tooltip-mode t))




(provide 'init-lsp)
;;; init-lsp.el ends here
