;;; init-lsp.el --- Support for the Language Servers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Language Server Protocol
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((java-mode
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
                      (add-hook 'before-save-hook 'lsp-format-buffer nil t)))
  :init
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-print-io nil ;; Set to t when debugging lsp errors
        lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr")
        lsp-enable-snippet nil
        lsp-prefer-flymake nil
        lsp-lens-enable nil
        lsp-idle-delay 0.5
        lsp-session-file (expand-file-name "var/.lsp-session-v1" user-emacs-directory)
        lsp-modeline-diagnostics-enable t
        lsp-modeline-diagnostics-scope :workspace
        lsp-completion-provider :capf
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
        lsp-ui-doc-position 'bottom))

(use-package dap-mode
  :after lsp-mode
  :init
  (add-hook 'after-init-hook #'(lambda ()
                                 (dap-mode)
                                 (dap-ui-mode))))


(provide 'init-lsp)
;;; init-lsp.el ends here
