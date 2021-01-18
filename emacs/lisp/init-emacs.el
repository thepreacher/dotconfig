;;; init-emacs.el --- Configure Emacs without packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Better defaults
(setq-default
  visible-bell t                                   ; Set up the visible bell
  create-lockfiles nil                             ; Do not create lockfiles
  auto-save-default nil                            ; Do not save files automatically
  save-interprogram-paste-before-kill t            ;
  mouse-yank-at-point t                            ;
  enable-recursive-minibuffers t                   ; Allow commands to be run on minibuffers
  kill-whole-line t                                ; Let C-k delete the whole line
  ad-redefinition-action 'accept                   ; Silence warnings for redefinition
  cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
  display-time-default-load-average nil            ; Don't display load average
  fill-column 80                                   ; Set width for automatic line breaks
  help-window-select t                             ; Focus new help windows when opened
  indent-tabs-mode nil                             ; Prefers spaces over tabs
  inhibit-startup-screen t                         ; Disable start-up screen
  initial-scratch-message ";; Happy Hacking!!!\n\n"; Empty the initial *scratch* buffer
  kill-ring-max 128                                ; Maximum length of kill ring
  load-prefer-newer t                              ; Prefers the newest version of a file
  mark-ring-max 128                                ; Maximum length of mark ring
  read-process-output-max (* 1024 1024)            ; Increase the amount of data reads from the process
  message-log-max 10000                            ; Maximum message log entries
  scroll-conservatively most-positive-fixnum       ; Always scroll by one line
  select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
  tab-width 4                                      ; Set width for tabs
  ;; tab-always-indent 'complete                     ; make tab key do indent first then completion.
  user-full-name "Napoleon Ahiable"                ; Set the full name of the current user
  user-mail-address "219075+thepreacher@users.noreply.github.com" ; Set the email address of the current user
  vc-follow-symlinks t                             ; Always follow the symlinks
  next-line-add-newlines t                         ; insert newlines if the point is at the end of the buffer
  view-read-only t                                 ; Always open read-only buffers in view-mode
  cursor-type 'bar                                 ; Default is 'box (options - 'bar ,'hollow)
  require-final-newline t                          ; Newline at end of file
  column-number-mode 1)                            ; Show the column number


;; Customize default emacs file save locations
(setq-default
  auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t))
  auto-save-list-file-name `((".*" ,(expand-file-name "auto-save/auto-save-list" user-emacs-directory) t))
  backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Replace yes/no prompts with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; History
(add-hook 'after-init-hook 'savehist-mode t)
(setq history-delete-duplicates t
      history-length t
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      savehist-file (expand-file-name "var/history" user-emacs-directory)
      savehist-save-minibuffer-history 1)



;; Make typing delete/overwrites selected text
(add-hook 'after-init-hook 'delete-selection-mode)
(setq shift-select-mode nil)


;;;; Fonts and Themes

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Fonts
(defvar nqa/default-font-size 150)
(defvar nqa/default-variable-font-size 150)

(set-face-attribute
  'default nil
  :font "JetBrainsMono Nerd Font"
  :height nqa/default-font-size)
;; Set the fixed pitch face
(set-face-attribute
  'fixed-pitch nil
  :font "JetBrainsMono Nerd Font"
  :height nqa/default-font-size)
;; Set the variable pitch face
(set-face-attribute
  'variable-pitch nil
  :font "SF Pro Display"
  :height nqa/default-variable-font-size
  :weight 'regular)


;; Move cursor by camelCase
(global-subword-mode 1)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)

;; Huge files
(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))

;; cua goodness without copy/paste etc.
(cua-selection-mode t)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)




;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Show matching parens
(add-hook 'after-init-hook 'show-paren-mode)



;; Fill column
(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?\u254e)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))


;; Recent Files
(add-hook 'after-init-hook 'recentf-mode)
(setq-default recentf-auto-cleanup 'never
              recentf-max-saved-items 500
              recentf-max-menu-items 15
              recentf-save-file (concat user-emacs-directory "var/recentf")
              recentf-exclude (list "COMMIT_EDITMSG"
                                    "~$"
                                    "/scp:"
                                    "/ssh:"
                                    "/sudo:"
                                    "/tmp/"
                                    "/\\.git/.*\\'"
                                    "/elpa/.*\\'"
                                    "/tramp.*\\'"
                                    "/sudo.*\\'"
                                    "/node_modules/.*\\'"
                                    (concat package-user-dir "/.*-autoloads\\.el\\'")))





;; Dired
(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(with-eval-after-load 'dired
  (setq dired-recursive-deletes 'top
        dired-recursive-copies 'always
        delete-by-moving-to-trash t)
  (setq dired-listing-switches "-agho --group-directories-first")
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode)
  ;; auto refresh dired when file changes
  (add-hook 'dired-mode-hook 'auto-revert-mode))

 ;; Abbreviations
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))


;;;; General keybindings

;;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

;; ESC cancels all
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Zap *up* to char is a handy pair for zap-to-char
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])




(provide 'init-emacs)
;;; init-emacs.el ends here
