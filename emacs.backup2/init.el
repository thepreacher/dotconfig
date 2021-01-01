;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Author: Napleon Ahiable
;;; Commentary:

;; Targeted for Emacs 27+

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)


;; CheckVer
(cond ((version< emacs-version "26.1")
       (warn "M-EMACS requires Emacs 26.1 and above!"))
      ((let* ((early-init-f (expand-file-name "early-init.el" user-emacs-directory))
	      (early-init-do-not-edit-d (expand-file-name "early-init-do-not-edit/" user-emacs-directory))
	      (early-init-do-not-edit-f (expand-file-name "early-init.el" early-init-do-not-edit-d)))
	 (and (version< emacs-version "27")
	      (or (not (file-exists-p early-init-do-not-edit-f))
		  (file-newer-than-file-p early-init-f early-init-do-not-edit-f)))
	 (make-directory early-init-do-not-edit-d t)
	 (copy-file early-init-f early-init-do-not-edit-f t t t t)
	 (add-to-list 'load-path early-init-do-not-edit-d)
	 (require 'early-init))))


(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(defvar nqa/gc-cons-threshold (* 10 1000 1000))


(add-hook 'emacs-startup-hook ; hook run after loading init files
	  #'(lambda ()
	      (setq gc-cons-threshold nqa/gc-cons-threshold
		    gc-cons-percentage 0.1
		    file-name-handler-alist file-name-handler-alist-original)))

(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (message "Emacs ready in %s with %d garbage collections."
		       (format "%.2f seconds"
			       (float-time
        				(time-subtract after-init-time before-init-time)))
        		    gcs-done)))

(add-hook 'minibuffer-setup-hook #'(lambda ()
				     (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda ()
				    (garbage-collect)
				    (setq gc-cons-threshold nqa/gc-cons-threshold)))

(setq message-log-max 10000)

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Initialize package sources
(require 'package)

;; Don't use the compiled code if its the older package.
(setq load-prefer-newer t)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(require 'cl-lib)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux plaforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; To aid benchmarking
;; Show a message whenever a package takes longer than 0.1s to load
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(use-package gnu-elpa-keyring-update)

(use-package async
  :config
  (async-bytecomp-package-mode 1))

(use-package delight)

;;============================================================================;;

;;; Make environment variables available in Emacs
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  ;;(setq exec-path-from-shell-debug t)
  (setq exec-path-from-shell-arguments nil)
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (progn
    (dolist (var '("PATH"
		   "SSH_AUTH_SOCK"
		   "LANG"
		   "WORKON_HOME"
		   ))
      (add-to-list 'exec-path-from-shell-variables var)))
  (exec-path-from-shell-initialize))



;;; Constants
(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst *rg*
  (executable-find "rg")
  "Do we have ripgrep?")

(defconst *python*
  (executable-find "python")
  "Do we have python?")

(defconst *python3*
  (executable-find "python3")
  "Do we have python3?")



;;; Frame fonts
(defvar nqa/default-font-size 150)
(defvar nqa/default-variable-font-size 150)

;;; Font Configuration
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height nqa/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height nqa/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "SF Pro Display" :height nqa/default-variable-font-size :weight 'regular)



;;; Better Defaults

(setq-default
  ;; Set up the visible bell
  visible-bell t
  ;; Display column number
  column-number-mode t
  ;; Do not show the startup message.
  inhibit-startup-message t
  ;; Do not put 'customize' config in init.el; give it another file.
  custom-file (expand-file-name "etc/custom-file.el" user-emacs-directory)
  ;; 72 is too less for the fontsize that I use.
  fill-column 90
  ;; Use your name in the frame title. :)
  frame-title-format (format "%s's Emacs" (capitalize user-login-name))
  ;; Do not create lockfiles.
  create-lockfiles nil
  ;; 4 is more popular.
  tab-width 4
  sentence-end-double-space nil
  ;; Don't use hard tabs
  indent-tabs-mode nil
  ;; Emacs can automatically create backup files. This tells Emacs to put all backups in
  ;; ~/.emacs.d/backups. More info:
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
  backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
  ;; Do not autosave.
  auto-save-default nil
  ;; Allow commands to be run on minibuffers.
  enable-recursive-minibuffers t
  ;; Let C-k delete the whole line.
  kill-whole-line t
  ;;
  frame-resize-pixelwise t
  ;; Initial Greeting
  initial-scratch-message ";; Happy Hacking!!!\n"

  save-interprogram-paste-before-kill t
  mouse-yank-at-point t
  blink-cursor-interval 0.4
  bookmark-default-file (expand-file-name "var/bookmarks.el" user-emacs-directory)
  buffers-menu-max-size 30
  case-fold-search t
  ediff-split-window-function 'split-window-horizontally
  ediff-window-setup-function 'ediff-setup-windows-plain
  set-mark-command-repeat-pop t
 )

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; make typing delete/overwrites selected text
(delete-selection-mode 1)
(setq shift-select-mode nil)

;; Parens
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)
;; don't delay show parens immediately
(setq show-paren-delay 0)

;; Flash cursor position
(use-package beacon
  :init
  (setq-default beacon-lighter "")
  (setq-default beacon-size 5)
  :config
  (beacon-mode))

;; make tab key do indent first then completion.
(setq tab-always-indent 'complete)

;; make return key also do indent, globally
(electric-indent-mode 0) ;; +1 to enable

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode +1)

;; hightlight current line
(global-hl-line-mode +1)

;; visual-fill-column
(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))

;; auto revert mode
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(with-eval-after-load 'autorevert
  (delight 'auto-revert-mode))

(add-hook 'after-init-hook 'transient-mark-mode)

;; Huge files
(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))

;; move cursor by camelCase
(use-package subword-mode
  :ensure nil
  :hook (after-init . global-subword-mode))

(setq use-dialog-box nil)

(progn
  ;; no need to warn
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil))

;; Page break lines
(use-package page-break-lines
  :hook (after-init . global-page-break-lines-mode))

;;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :hook (after-init . whole-line-or-region-global-mode))

; Auto-save settings
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))




;;; Keybindings

;; MacOS Specific keybindings
(when *sys/mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
				    ((shift) . 5)
				    ((control)))))


;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)

;; Crux
(use-package crux
  :bind (("C-k" . crux-smart-kill-line)
         ("M-o" . crux-smart-open-line)
         ("C-M-o" . crux-smart-open-line-above)
         ("C-c f" . crux-recentf-find-file)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c e" . crux-eval-and-replace)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c r" . crux-rename-file-and-buffer)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-<backspace>" . crux-kill-line-backwards)))




;;; Themes
;; Doom
(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package all-the-icons) ;; required by doom-modeline

;; Doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode +1)
  :init (setq doom-modeline-height 15))



;;; Evil / General
(use-package general
  :config
  (general-create-definer nqa/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (nqa/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook ((after-init . evil-mode)
         (evil-insert-state-exit . (lambda ()
                                    (call-interactively #'save-buffer))))
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (use-package evil-collection
    :config
    (evil-collection-init)))



;;; Directory and Files
(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package dired
  :ensure nil
  :after evil
  :init
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  ;; auto refresh dired when file changes
  :hook (dired-mode . auto-revert-mode)

  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (delete-by-moving-to-trash t)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package diredfl
  :config
  (diredfl-global-mode)
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(use-package diff-hl
  :hook (dired-mode-hook . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :init
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
				("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; Saveplace
(use-package saveplace
  :init
  (setq save-place-file (expand-file-name "var/places" user-emacs-directory))
  :config
  (save-place-mode +1))

;; Uniquify
(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Treemacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
      	  treemacs-deferred-git-apply-delay      0.5
      	  treemacs-directory-name-transformer    #'identity
      	  treemacs-display-in-side-window        t
      	  treemacs-eldoc-display                 t
      	  treemacs-file-event-delay              5000
      	  treemacs-file-extension-regex          treemacs-last-period-regex-value
      	  treemacs-file-follow-delay             0.2
      	  treemacs-file-name-transformer         #'identity
      	  treemacs-follow-after-init             t
      	  treemacs-git-command-pipe              ""
      	  treemacs-goto-tag-strategy             'refetch-index
      	  treemacs-indentation                   2
      	  treemacs-indentation-string            " "
      	  treemacs-is-never-other-window         nil
      	  treemacs-max-git-entries               5000
      	  treemacs-missing-project-action        'ask
      	  treemacs-move-forward-on-expand        nil
      	  treemacs-no-png-images                 nil
      	  treemacs-no-delete-other-windows       t
      	  treemacs-project-follow-cleanup        nil
      	  treemacs-persist-file                  (concat user-emacs-directory "/var/treemacs-persist")
      	  treemacs-position                      'left
      	  treemacs-position                      'right
      	  treemacs-recenter-distance             0.1
      	  treemacs-recenter-after-file-follow    nil
      	  treemacs-recenter-after-tag-follow     nil
      	  treemacs-recenter-after-project-jump   'always
      	  treemacs-recenter-after-project-expand 'on-distance
      	  treemacs-show-cursor                   nil
      	  treemacs-show-hidden-files             t
      	  treemacs-silent-filewatch              nil
      	  treemacs-silent-refresh                nil
      	  treemacs-sorting                       'alphabetic-asc
      	  treemacs-space-between-root-nodes      t
      	  treemacs-tag-follow-cleanup            t
      	  treemacs-tag-follow-delay              1.5
      	  treemacs-user-mode-line-format         nil
      	  treemacs-user-header-line-format       nil
      	  treemacs-width                         35
      	  treemacs-workspace-switch-cleanup      nil))

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
		 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
  :bind (:map global-map
	     ("M-0"       . treemacs-select-window)
	     ("C-x t 1"   . treemacs-delete-other-windows)
	     ("C-x t t"   . treemacs)
	     ("C-x t B"   . treemacs-bookmark)
	     ("C-x t C-t" . treemacs-find-file)
	     ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :after (treemacs lsp-mode))


;; Modify the default ibuffer-formats (toggle with `)
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 22 22 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 12 12 :left :elide)
              " "
              vc-relative-file)
        (mark modified read-only vc-status-mini " "
              (name 22 22 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 14 14 :left :elide)
              " "
              (vc-status 12 12 :left)
              " "
              vc-relative-file)))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)


(use-package fullframe
  :after ibuffer
  :config
  (fullframe ibuffer ibuffer-quit))


(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(use-package ibuffer-vc
  :hook (ibuffer-mode .  ibuffer-set-up-preferred-filters))




;;; Dev Utils
(use-package projectile
  :hook (after-init . projectile-mode)
  :init
  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))

  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
      (setq projectile-project-search-path '("~/projects/learn/elixir/"
				                       "~/projects/learn/python/"
				                       "~/projects/learn/elm/"
				                       "~/projects/learn/rust/"
				                       "~/projects/learn/ai/"
				                       "~/projects/learn/c++/"
				                       "~/projects/learn/js/"
				                       "~/projects/probono/"
				                       "~/projects/paid/")))
  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-known-projects-file (expand-file-name "var/projectile-bookmarks.eld" user-emacs-directory))
  ;;(projectile-mode +1)
  :bind (:map projectile-mode-map
       ("C-c p" . projectile-command-map))
  :config
  ;; Counsel integration for projectile
  (use-package counsel-projectile
    :config (counsel-projectile-mode))
  (use-package ibuffer-projectile))

;; Git / Magit
(use-package magit
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge :after magit)

;;help you comment code efficiently. For example, you can press 99,ci to comment out 99 lines.
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Lsp
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
      	  c++-mode
      	  objc-mode
      	  elixir-mode
      	  elm-mode
      	  rust-mode
          haskell-literate-mode
      	  haskell-mode) . lsp-deferred)
  :init
  (setq lsp-print-io nil) ;; Set to nil or comment out when debugging lsp errors
  (setq lsp-session-file (expand-file-name "var/.lsp-session-v1" user-emacs-directory))
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  (setq lsp-completion-provider :capf)
  (setq lsp-modeline-diagnostics-enable t)
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace)
  ;; This variable determines how often lsp-mode will refresh the highlights, lenses, links, etc while you type.
  (setq lsp-idle-delay 0.500)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-max-height 20)
  (setq lsp-ui-doc-max-width 80)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :after (lsp-mode ivy)
  :commands lsp-ivy-workspace-symbol)

;; DAP
(use-package dap-mode
  :defer
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :bind
  (:map dap-mode-map
        (("<f12>" . dap-debug)
         ("<f8>" . dap-continue)
         ("<f9>" . dap-next)
         ("<M-f11>" . dap-step-in)
         ("C-M-<f11>" . dap-step-out)
         ("<f7>" . dap-breakpoint-toggle)))
  :hook ((python-mode . (lambda () (require 'dap-python)))
         (haskell-mode . (lambda () (require 'dap-haskell)))
         (ruby-mode . (lambda () (require 'dap-ruby)))
         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
         (php-mode . (lambda () (require 'dap-php)))
         (elixir-mode . (lambda () (require 'dap-elixir)))
         ((js-mode js2-mode typescript-mode) . (lambda () (require 'dap-firefox))))
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

;; Flycheck
(use-package flycheck
  :init (global-flycheck-mode))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :hook (org-mode
	 emacs-lisp-mode
	 web-mode
	 typescript-mode
	 js2-mode))

(use-package highlight-indent-guides
  :delight
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character 9615) ; left-align vertical bar
  (highlight-indent-guides-auto-character-face-perc 20))

(use-package highlight-symbol
  :delight
  :hook (prog-mode . highlight-symbol-mode)
  :custom-face
  (highlight-symbol-face ((t (:background "#44475a"))))
  :custom
  (highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-operators
  :hook (prog-mode . highlight-operators-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))



;;; Search and fuzzy matching

; (use-package amx
;   :init
;   (setq amx-save-file (expand-file-name "var/amx-items" user-emacs-directory)))

;(use-package flx)

; (use-package flx-ido
;   :after ido
;   :init
;   (setq ido-use-faces nil)
;   :config
;   (flx-ido-mode 1))


;; Ido vertical
; (use-package ido-vertical-mode
;   :after ido
;   :init
;   (setq ido-vertical-define-keys 'C-n-and-C-p-only)
;   :config
;   (ido-vertical-mode 1))

;; Ivy, Swiper and Counsel

;; Swiper
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper))
  :custom
  (swiper-action-recenter t)
  (swiper-goto-start-of-match t))

(use-package counsel
  :after ivy
  :init
  (setq-default ivy-initial-inputs-alist
              '((Man-completion-table . "^")
                (woman . "^")))
  :bind (("M-x" . counsel-M-x)
         ("C-x C-r" . counsel-recentf)
         ("C-x C-f" . counsel-find-file)
	     ("C-M-j" . counsel-switch-buffer)
         ("M-?" . sanityinc/counsel-search-project)
	       :map minibuffer-local-map
	            ("C-r" . counsel-minibuffer-history))
  :custom
  (counsel-find-file-ignore-regexp "\\.DS_Store\\|.git")
  :config
  (when (require 'projectile)
    (let ((search-function
           (cond
            ((executable-find "rg") 'counsel-rg)
            ((executable-find "ag") 'counsel-ag)
            ((executable-find "pt") 'counsel-pt)
            ((executable-find "ack") 'counsel-ack))))
      (when search-function
        (defun sanityinc/counsel-search-project (initial-input &optional use-current-dir)
          "Search using `counsel-rg' or similar from the project root for INITIAL-INPUT.
          If there is no project root, or if the prefix argument
          USE-CURRENT-DIR is set, then search from the current directory
          instead."
          (interactive (list (let ((sym (thing-at-point 'symbol)))
                               (when sym (regexp-quote sym)))
                             current-prefix-arg))
          (let ((current-prefix-arg)
                (dir (if use-current-dir
                         default-directory
                       (condition-case err
                           (projectile-project-root)
                         (error default-directory)))))
            (funcall search-function initial-input dir)))))
    (with-eval-after-load 'ivy
      (add-to-list 'ivy-height-alist (cons 'counsel-ag 20)))))

(use-package ivy
  :hook (after-init . ivy-mode)
  :init
  (setq-default ivy-use-virtual-buffers t
                ivy-display-style nil
                ivy-virtual-abbreviate 'abbreviate
                ivy-initial-inputs-alist nil ;; Don't start searches with ^
                enable-recursive-minibuffers t
                ivy-use-selectable-prompt t
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-count-format "(%d/%d) "     ; Show current match and matches
                ivy-extra-directories nil) ; Do not show "./" and "../"
  :bind (("C-c C-r" . ivy-resume)
         ("C-M-j" . ivy-immediate-done)
         ("C-x b" . ivy-switch-buffer)
         :map ivy-switch-buffer-map
              ("C-k" . ivy-previous-line)
              ("C-l" . ivy-done)
              ("C-d" . ivy-switch-buffer-kill)
         :map ivy-minibuffer-map
              ("<tab>" . ivy-alt-done)
              ("C-l" . ivy-alt-done)
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
         :map ivy-reverse-i-search-map
              ("C-k" . ivy-previous-line)
              ("C-d" . ivy-reverse-i-search-kill))
  :config
  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7)
  ;; Use different regex strategies per completion command
  (setq ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                                (counsel-projectile-rg . ivy--regex-plus)
                                (counsel-ag . ivy--regex-plus)
                                (counsel-projectile-ag . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-fuzzy))))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode +1))

(use-package prescient
  :config
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after (prescient ivy)
  :init
  (setq ivy-prescient-mode t)
  (setq ivy-prescient-enable-filtering t)
  (setq ivy-prescient-retain-classic-highlighting t))

(use-package ivy-xref :after ivy)


;; snippets
(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets))


;;; Help and Discoverability

;; Which-key
(use-package which-key
  :init (which-key-mode)
  :delight
  :config
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.4))

;; Company - Auto completion system
(add-to-list 'completion-styles 'initials t)

(use-package company
  :after lsp-mode
  :init
  (setq company-selection-wrap-around t
        company-minimum-prefix-length 1
        company-idle-delay 0.0)
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-indent-or-complete-common))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :config
  ;; Documentation popups
  (use-package company-quickhelp
    :hook (company-mode . company-quickhelp-mode))

  (use-package company-box
    :hook (company-mode . company-box-mode))

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

(use-package company-prescient
  :after (company prescient)
  :hook (company-mode . company-prescient-mode))


;; Helpful - A better emacs help buffer
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Recentf
(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-save-file (concat user-emacs-directory "var/recentf")
        recentf-exclude '("/tmp/"
              "/ssh:"))
  :config
  (recentf-mode +1))


;; Hydra
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))


(nqa/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))




;;; Org

(defun nqa/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "SF Pro Display" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch))

(defun nqa/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


(defun nqa/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


(use-package org
  :pin org
  :hook ((org-mode . nqa/org-mode-setup)
	 (org-mode . nqa/org-mode-visual-fill))
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
	'("~/Orgfiles/Tasks.org"
	  "~/Orgfiles/Habits.org"
	  "~/Orgfiles/Birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
	'(("Archive.org" :maxlevel . 1)
	  ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
	'((:startgroup)
                                        ; Put mutually exclusive tags here
	  (:endgroup)
	  ("@errand" . ?E)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("agenda" . ?a)
	  ("planning" . ?p)
	  ("publish" . ?P)
	  ("batch" . ?b)
	  ("note" . ?n)
	  ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))

	  ("W" "Work Tasks" tags-todo "+work-email")

	  ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))

	  ("w" "Workflow Status"
	   ((todo "WAIT"
		  ((org-agenda-overriding-header "Waiting on External")
		   (org-agenda-files org-agenda-files)))
	    (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))
	    (todo "PLAN"
		  ((org-agenda-overriding-header "In Planning")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "BACKLOG"
		  ((org-agenda-overriding-header "Project Backlog")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
	    (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "COMPLETED"
		  ((org-agenda-overriding-header "Completed Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "CANC"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/Orgfiles/Tasks.org" "Inbox")
	   "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
	   (file+olp+datetree "~/Orgfiles/Journal.org")
	   "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
	   ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
	   :clock-in :clock-resume
	   :empty-lines 1)
	  ("jm" "Meeting" entry
	   (file+olp+datetree "~/Orgfiles/Journal.org")
	   "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
	   :clock-in :clock-resume
	   :empty-lines 1)

	  ("w" "Workflows")
	  ("we" "Checking Email" entry (file+olp+datetree "~/Orgfiles/Journal.org")
	   "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

	  ("m" "Metrics Capture")
	  ("mw" "Weight" table-line (file+headline "~/Orgfiles/Metrics.org" "Weight")
	   "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (nqa/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '((emacs-lisp . t)
;;     (python . t)))

;; (push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
;; (defun nqa/org-babel-tangle-config ()
;;   (when (string-equal (file-name-directory (buffer-file-name))
;;                       (expand-file-name user-emacs-directory))
;;     ;; Dynamic scoping to the rescue
;;     (let ((org-confirm-babel-evaluate nil))
;;       (org-babel-tangle))))

;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'nqa/org-babel-tangle-config)))



;;; Languages

;; Elm
(use-package elm-mode
  :after company
  :hook (elm-mode . elm-format-on-save-mode))

;; Elxir
;; https://github.com/elixir-lsp/elixir-ls
(defvar lsp-elixir--config-options (make-hash-table))

(use-package elixir-mode
  :after flycheck
  :mode ("\\.exs?\\'" . elixir-mode)
  :init
  ;; for executable of language server, if it's not symlinked on your PATH
  (add-to-list 'exec-path (expand-file-name "~/lang-servers/elixir-ls/release/")) ;; Uncomment for lsp-mode
  :hook
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (elixir-mode . (lambda ()
		   (add-hook 'before-save-hook 'elixir-format nil t)))
  :config
  (use-package flycheck-credo
    :init
    (setq flycheck-elixir-credo-strict t)
    :config
    (flycheck-credo-setup))

    (add-hook 'lsp-after-initialize-hook
    	  (lambda ()
    	    (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options)))))

;; Python
(use-package python-mode
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))
  :config
  ;; Lsp client for python
  (use-package lsp-pyright)

(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :custom
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] "))))

;; Python code formatter
(use-package blacken
  :hook (python-mode . blacken-mode))

;; Pipenv Environment management
(use-package pipenv
  :hook (python-mode . pipenv-mode))


;; Toml
(use-package toml-mode :defer t)

;; Rust
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook (before-save . (lambda ()
                          (when (eq 'rust-mode major-mode)
                              (lsp-format-buffer)))))


;; Add keybindings for interacting with Cargo
; (use-package cargo
;   :hook (rust-mode . cargo-minor-mode))

; (use-package flycheck-rust
;   :after (rust-mode)
;   :hook (flycheck-mode . flycheck-rust-setup))

;; Go
(use-package go-mode
  :mode ("\\.go$" . go-mode))

;; Haskell
(use-package haskell-mode
  :mode ("\\.l?hs\\'" . haskell-mode)
  :hook (haskell-mode . turn-on-haskell-indentation)
  :config
  (use-package lsp-haskell))


;; Typescript / Javascript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :init
  (setq typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :init
  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
	 (typescript-mode . prettier-js-mode))
  :init
  (setq prettier-js-show-errors nil))

;; C / C++
(use-package ccls
  :mode "\\.c(pp)?\\'")




;;; Spelling
(require 'ispell)

(use-package flyspell
  :ensure nil
  :if (executable-find ispell-program-name)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (ispell-program-name "/usr/local/bin/aspell"))






;;; Misc Utils

;; IDO
; (setq ido-enable-flex-matching t)
; (setq ido-everywhere t)
; (ido-mode 1)

;;Dash - A modern list api for Emacs. No 'cl required.
(use-package dash)
(use-package dash-functional)

;; Show event history and command history of some or all buffers.
(use-package command-log-mode)


;; Display line numbers
(setq-default display-line-numbers-type t ;; for relative numbering set this to 'visual or 'relative
          display-line-numbers-current-absolute t
          display-line-numbers-width 4
          display-line-numbers-widen t)

;; This should be at the bottom for it to work
(set-face-attribute 'line-number nil
            :font "JetBrainsMono Nerd Font")

(set-face-attribute 'line-number-current-line nil
            :weight 'bold
            :font "JetBrainsMono Nerd Font"
            :foreground "goldenrod")

(when (fboundp 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


;;; Locales and UTF-8
(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun sanityinc/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (sanityinc/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))


;; Start emacs by default using the following directory
(setq default-directory (expand-file-name "~/projects/"))




(provide 'init)
;; Local Variables:
;;; init.el ends here
