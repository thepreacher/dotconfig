;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:

;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'winner-mode)

;; Don’t ask before killing a buffer. I know what I’m doing.
(global-set-key [remap kill-buffer] #'kill-this-buffer)

;; Give focus to newly created windows
(use-package window
  :ensure nil
  :bind (("C-x 3" . hsplit-last-buffer)
         ("C-x 2" . vsplit-last-buffer))
  :preface
  (defun hsplit-last-buffer ()
    "Gives the focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun vsplit-last-buffer ()
    "Gives the focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1)))

;; Make "C-x o" prompt for a target window when there are more than 2
(use-package switch-window
  :init
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil)
  :bind (("C-x o" . switch-window)
         ("C-x w" . switch-window-then-swap-buffer)))

;; windmove - window config management
;; By default, this one is bound to the key sequence ctrl-c left. If you change
;; your mind (while undoing), you can press ctrl-c right (calling winner-redo)
(use-package windmove
  :bind (("C-c h" . windmove-left)
         ("C-c j" . windmove-down)
         ("C-c k" . windmove-up)
         ("C-c l" . windmove-right)))

;; winner
(use-package winner
  :ensure nil
  :init
  (add-hook 'after-init-hook 'winner-mode))

(unless (memq window-system '(nt w32))
  (use-package windswap)
  (add-hook 'after-init-hook (apply-partially 'windmove-default-keybindings 'control))
  (add-hook 'after-init-hook (apply-partially 'windswap-default-keybindings 'shift 'control)))


(provide 'init-windows)
;;; init-windows.el ends here
