;;; init-macos.el --- Macos specific configs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Make environment variables available in Emacs
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-debug nil)
  ;; (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-arguments nil)
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (dolist (var '("SSH_AUTH_SOCK"
                 "LANG"
                 "WORKON_HOME"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;  Keybindings
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)
;; Make mouse wheel / trackpad scrolling less jerky
(setq mouse-wheel-scroll-amount '(1
                                   ((shift) . 5)
                                   ((control))))




(provide 'init-macos)
;;; init-macos.el ends here
