;;; init-elixir.el --- Support for the Elixir language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Elxir
;; https://github.com/elixir-lsp/elixir-ls
(defvar lsp-elixir--config-options (make-hash-table))

(use-package elixir-mode
  :mode ("\\.exs?\\'")
  ;; :init
  ;; ;; for executable of language server, if it's not symlinked on your PATH
  ;; (add-to-list 'exec-path (expand-file-name "~/lang-servers/elixir-ls/release/")) ;; Uncomment for lsp-mode
  :config
  (add-hook 'lsp-after-initialize-hook
            (lambda ()
              (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options)))))




(provide 'init-elixir)
;;; init-elixir.el ends here
