;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Initial Dooom frame size
;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
; (add-to-list 'initial-frame-alist '(width  . 87))
; (add-to-list 'initial-frame-alist '(height . 54))
; (add-to-list 'default-frame-alist '(width  . 87))
; (add-to-list 'default-frame-alist '(height . 54))

;; doom specific way of setting initial frame size
(pushnew! initial-frame-alist
          '(top . 0)
          '(left . 962)
          '(width . 90)
          '(height . 60))

;; doom specific way of setting default frame size
(pushnew! default-frame-alist
          '(top . 0)
          '(left . 952)
          '(width . 90)
          '(height . 60))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Consts
(defconst IS-GUI
  (display-graphic-p)
  "Are we running on a GUI Emacs?")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Napoleon Ahiable"
      user-mail-address "nahiable@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15 :weight 'light))
(setq doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 16 :weight 'light))

;; Variable Pitch Font
(setq doom-variable-pitch-font (font-spec :family "SF Pro Display" :size 14 :weight 'normal))

(when IS-MAC
  (setq ns-use-thin-smoothing t))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/projects/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; https://github.com/zaiste/.doom.d/blob/master/config.el

;;; :defaults
;; Auto load file it modifed by another program
(global-auto-revert-mode t)

(setq
  history-delete-duplicates t
  web-mode-markup-indent-offset 2
  web-mode-code-indent-offset 2
  web-mode-css-indent-offset 2
  prettier-js-args '("--single-quote")
  kill-whole-line t
  treemacs-width 32
  ;; opt for manual completion
  company-idle-delay nil
  dired-dwim-target t
  org-ellipsis " ▾ "
  org-bullets-bullet-list '("·")
  org-tags-column -80
  org-agenda-files (ignore-errors (directory-files +org-dir t "\\.org$" t))
  org-log-done 'time
  css-indent-offset 2)


;; projectile
(setq projectile-project-search-path '("~/projects/learn/elixir/"
                                       "~/projects/learn/python/"
                                       "~/projects/learn/elm/"
                                       "~/projects/learn/rust/"
                                       "~/projects/learn/ai/"
                                       "~/projects/learn/c++/"
                                       "~/projects/learn/js/"
                                       "~/projects/probono/"
                                       "~/projects/paid/"))


;; Refresh projectle project list before switching projects
;; This is so newly created projects will be listed for selection.

(defadvice! refresh-projects-before-switching (&rest _args) ; This refreshes the project list every you press `SPC p p`
  :before #'counsel-projectile-switch-project
  (projectile-discover-projects-in-search-path))

;; The counsel-buffer-or-recentf function by default shows duplicated entries
;;because it does not abbreviate the paths of the open buffers.
;;https://github.com/abo-abo/swiper/pull/2687
;; https://zzamboni.org/post/my-doom-emacs-configuration-with-commentary/
(defun zz/counsel-buffer-or-recentf-candidates ()
  "Return candidates for `counsel-buffer-or-recentf'."
  (require 'recentf)
  (recentf-mode)
  (let ((buffers
         (delq nil
               (mapcar (lambda (b)
                         (when (buffer-file-name b)
                           (abbreviate-file-name (buffer-file-name b))))
                       (delq (current-buffer) (buffer-list))))))
    (append
     buffers
     (cl-remove-if (lambda (f) (member f buffers))
                   (counsel-recentf-candidates)))))

(advice-add #'counsel-buffer-or-recentf-candidates
            :override #'zz/counsel-buffer-or-recentf-candidates)



(use-package! switch-buffer-functions
  :after recentf
  :preface
  (defun my-recentf-track-visited-file (_prev _curr)
    (and buffer-file-name
         (recentf-add-file buffer-file-name)))
  :init
  (add-hook 'switch-buffer-functions #'my-recentf-track-visited-file))

;;; Modules
;; I prefer search matching to be ordered; it's more precise
(add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus))

;; Org mode
(setq org-directory "~/Org/"
    ;; org-default-notes-file
    org-default-notes-file "~/Org/inbox.org"
      org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "notes/")
      org-journal-encrypt-journal t
      org-journal-file-format "%d%m%Y.org"
      org-ellipsis " ▼ ")
      ;;org-superstar-headline-bullets-list '("#")


(add-hook 'org-mode-hook #'auto-fill-mode)

(defun +org*update-cookies ()
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let (org-hierarchical-todo-statistics)
      (org-update-parent-todo-statistics))))

(advice-add #'+org|update-cookies :override #'+org*update-cookies)

(add-hook! 'org-mode-hook (company-mode -1))
(add-hook! 'org-capture-mode-hook (company-mode -1))

;; Turn off line numbers in org-mode
(add-hook! org-mode (lambda () (display-line-numbers-mode -1)))


;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)


;; Direnv silence all that useless output
(setq direnv-always-show-summary nil)


;; Whitespace
(setq show-trailing-whitespace 't)
(global-whitespace-mode 't)

(progn
 ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://ergoemacs.org/emacs/whitespace-mode.html set lcs=tab:▸\ ,trail:·,eol:¬,nbsp:_
  (setq whitespace-style (quote (face spaces newline newline-mark)))

  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [172 10])))) ; LINE FEED,


;; Evil-undo behave like vim
(setq evil-want-fine-undo t)

;; Wakatime-mode
(use-package! wakatime-mode
  :init
  (setq wakatime-cli-path (expand-file-name "~/.local/bin/wakatime"))
  :config
  (global-wakatime-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Language customizations

;; Markdown
(custom-theme-set-faces! 'doom-dracula
  `(markdown-code-face :background ,(doom-darken 'bg 0.075))
  `(font-lock-variable-name-face :foreground ,(doom-lighten 'magenta 0.6)))

;; Define Elixir lsp server for lsp-mode
(after! lsp-mode
  (lsp-register-custom-settings '(("elixirLS.mixEnv" "dev")))
  (setq lsp-clients-elixir-server-executable "language_server.sh"))


;; Configure lsp-ui - user interface of lsp-mode
(after! lsp-ui
  (setq lsp-ui-doc-max-height 13
        lsp-ui-doc-max-width 80
        lsp-ui-doc-header t
        ;;lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'top
        lsp-ui-imenu-kind-position 'left
        ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
        ;; disable it by default.
        lsp-ui-sideline-enable nil
        sp-enable-symbol-highlighting nil
        lsp-ui-sideline-code-actions-prefix "💡"
        company-lsp-cache-candidates nil
        ;; fix for completing candidates not showing after “Enum.”:
        company-lsp-match-candidate-predicate #'company-lsp-match-candidate-prefix))

;; Use lsp-ui-doc-webkit only in GUI
(if IS-GUI
  (setq lsp-ui-doc-use-webkit t))

;; Elm


;; Rust
(after! rustic
  (setq! rustic-lsp-server 'rls))


;; Common Lisp
(setq! inferior-lisp-program "sbcl")


;; C-C++
(after! lsp-clients
  (set-lsp-priority! 'clangd 1))  ; ccls has priority 0


;; Elixir
;; Enable formatting on save and send reload command to Elixir’s REPL
;;(add-hook! elixir-mode
;;            (lambda ()
;;              (add-hook 'before-save-hook 'lsp-format-buffer nil t)
;;              (add-hook 'after-save-hook 'alchemist-iex-reload-module)))

;; credo with the '--strict' argument
(use-package! flycheck-credo
  :after elixir-mode
  :config
  (setq flycheck-elixir-credo-strict t))

;; (defun dap-elixir--populate-start-file-args (conf)
;;   "Populate CONF with the required arguments."
;;   (-> conf
;;       (dap--put-if-absent :dap-server-path '("debugger.sh"))
;;       (dap--put-if-absent :type "mix_task")
;;       (dap--put-if-absent :name "mix test")
;;       (dap--put-if-absent :request "launch")
;;       (dap--put-if-absent :task "test")
;;       (dap--put-if-absent :taskArgs (list "--trace"))
;;       (dap--put-if-absent :projectDir (lsp-find-session-folder (lsp-session) (buffer-file-name)))
;;       (dap--put-if-absent :cwd (lsp-find-session-folder (lsp-session) (buffer-file-name)))
;;       (dap--put-if-absent :requireFiles (list
;;                                          "test/**/test_helper.exs"
;;                                          "test/**/*_test.exs"))))

;; (dap-register-debug-provider "Elixir" 'dap-elixir--populate-start-file-args)
;; (dap-register-debug-template "Elixir Run Configuration"
;;                              (list :type "Elixir"
;;                                    :cwd nil
;;                                    :request "launch"
;;                                    :program nil
;;                                    :name "Elixir::Run"))

;; Python
(defalias 'workon 'pyvenv-workon)

(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))

;; Restart workspace after changing virtualenv
;(add-hook 'pipenv-mode-hook 'lsp-workspace-restart)
;(add-hook 'pyvenv-mode-hook 'lsp-workspace-restart)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.[l]eex\\'" . web-mode))

;; Julia
;(setq lsp-julia-package-dir nil) ;; Uncomment to use own LanguageServer.jl instead of the bundled one
(setq lsp-julia-default-environment "~/.julia/environments/v1.0")

;; zig-mode
(use-package! zig-mode)

;; Keybindings
;; change evil-escape key from ESC to jk
(setq-default evil-escape-key-sequence "jk")

(map! "C-x b"   #'counsel-buffer-or-recentf
      "C-x C-b" #'counsel-switch-buffer)

;; :tools magit
(setq magit-repository-directories '(("~/projects" . 2))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t)
