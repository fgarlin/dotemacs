;;; init.el -*- lexical-binding: t; -*-

;; Elpaca bootstrap.
;; See the Elpaca README.md installation instructions for the latest version.
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support for Elpaca.
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Allow navigating through `use-package' declarations using imenu.
(setopt use-package-enable-imenu-support t)

(use-package emacs
  :ensure nil
  :preface
  (defun my/unfill-paragraph ()
    "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.
`http://xahlee.info/emacs/emacs/emacs_unfill-paragraph.html'"
    (interactive)
    (let ((fill-column most-positive-fixnum))
      (fill-paragraph)))

  :bind
  (("C-x C-k" . kill-current-buffer)
   ("C-'" . duplicate-dwim)
   ("M-o" . other-window)
   ("M-Q" . my/unfill-paragraph))

  :custom
  ;; I don't use M-x customize, and I don't want Emacs to pollute my init.el,
  ;; so write any changes made by customize to a temporary file.
  (custom-file (make-temp-file "emacs-custom"))

  ;; Silence the startup message. See the docstring.
  (inhibit-startup-echo-area-message user-login-name)

  ;; No splash screen.
  (inhibit-startup-screen t)

  ;; Make the *scratch* buffer blank.
  (initial-scratch-message "")

  ;; No bell ringing at all.
  (ring-bell-function 'ignore)

  ;; No graphical dialogs.
  (use-dialog-box nil)
  (use-file-dialog nil)

  ;; Use short answers (e.g. y and n instead of yes and no).
  (use-short-answers t)

  ;; Ask before quitting Emacs.
  (confirm-kill-emacs 'yes-or-no-p)

  ;; Fix copying to the Windows clipboard on WSL.
  (select-active-regions nil)

  ;; Disable backups, autosave files and interlocking.
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)

  ;; Display absolute line numbers.
  (display-line-numbers t)
  ;; Enable column display.
  (column-number-mode t)

  ;; Better scrolling.
  (scroll-conservatively 101)
  (scroll-margin 5)

  ;; Indentation-related stuff. Force spaces. Tabs are rendered as 4 spaces.
  (tab-width 4)
  (indent-tabs-mode nil)
  ;; Ensure there is always a newline at the end of the file when saving.
  (require-final-newline t)
  ;; Set the default fill column to 80.
  (fill-column 80)
  ;; I guess this setting made sense in the typewriter era, but not anymore.
  (sentence-end-double-space nil)

  ;; Focus the newly opened help window.
  (help-window-select t)

  ;; Move point to the new line after calling `duplicate-dwim'.
  (duplicate-line-final-position 1)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Enable indentation+completion using the TAB key.
  (tab-always-indent 'complete)
  ;; Emacs 30 and newer: Disable Ispell completion function.
  (text-mode-ispell-word-completion nil)
  ;; Hide commands in M-x which do not work in the current mode.
  ;; Used directly by Corfu and Vertico, and useful beyond those.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :init
  ;; Enable `visual-line-mode'.
  (global-visual-line-mode)
  ;; Enable `context-menu-mode'.
  (context-menu-mode))

;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in packages ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Colorize in the *compile* buffer.
(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Enable automatic reverting of files when they have been changed by another
;; program. NOTE: Auto revert will not revert a buffer if it has unsaved
;; changes, or if its file on disk is deleted or renamed.
(use-package autorevert
  :ensure nil
  :custom
  ;; Revert non-file buffers like Dired.
  (global-auto-revert-non-file-buffers t)
  ;; Auto reverting slows down TRAMP.
  (auto-revert-remote-files nil)
  ;; No notification when a revert has occurred.
  (auto-revert-verbose nil)
  :init (global-auto-revert-mode))

;; Basically every program out there will delete the selected text as soon as
;; the user types something. Emacs does not do this by default, but it does have
;; a built-in package.
(use-package delsel
  :ensure nil
  :init (delete-selection-mode))

;; Some custom configuration for Dired.
(use-package dired
  :ensure nil
  :commands (dired)
  :custom
  (dired-isearch-filenames 'dwim)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  ;; Human-readable sizes (-h) and natural sort (-v) (file1, file, file10
  ;; instead of file1, file10, file2). NOTE: Requires GNU coreutils.
  (dired-listing-switches "-alh -v --group-directories-first"))

;; Enable electric pairs. Typing an open parenthesis automatically inserts the
;; corresponding closing parenthesis.
(use-package elec-pair
  :ensure nil
  :init (electric-pair-mode))

;; On-the-fly syntax checking.
(use-package flymake
  :ensure nil
  :custom
  ;; The left fringe is used by the vc gutter, so use the right one.
  (flymake-fringe-indicator-position 'right-fringe))

;; Highlight the current line in all buffers.
(use-package hl-line
  :ensure nil
  :init (global-hl-line-mode))

;; Highlight matching parenthesis.
(use-package paren
  :ensure nil
  :custom
  (show-paren-delay 0)
  :init (show-paren-mode))

;; Keep track of recent files.
(use-package recentf
  :ensure nil
  :init (recentf-mode))

;; The built-in `savehist' package keeps track of user inputs and stores them
;; across sessions. The user will see their latest choices closer to the top.
(use-package savehist
  :ensure nil
  :init (savehist-mode))

;; TRAMP tweaks.
;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr.
(use-package tramp
  :ensure nil
  :custom
  ;; Prevent TRAMP from creating a bunch of extra files and use scp
  ;; directly when moving files.
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-cache 60)
  (remote-file-name-inhibit-auto-save-visited t)
  (tramp-use-scp-direct-remote-copying t)
  ;; Increase the threshold for using scp to copy files.
  (tramp-copy-size-limit (* 1024 1024)) ; 1MB
  ;; Decreasing the debug level improves performance a tiny bit.
  (tramp-verbose 2)
  ;; Supress reading the remote shell history file.
  (shell-history-file-name t)
  ;; Disable version control on remote buffers.
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp))
  ;; Looking for version control backends is expensive. Limit the search to git.
  (vc-handled-backends '(Git))
  :config
  ;; OpenSSH configuration files can use an Include option for further
  ;; configuration files. Default TRAMP host name completion ignores this option.
  ;; Use all files in ~/.ssh/conf.d/ for host name completion:
  (dolist (method '("ssh" "scp"))
    (tramp-set-completion-function
     method (append (tramp-get-completion-function method)
                    (when (file-directory-p "~/.ssh/conf.d/")
                      (mapcar (lambda (file) `(tramp-parse-sconfig ,file))
                              (directory-files
                               "~/.ssh/conf.d/"
                               'full directory-files-no-dot-files-regexp))))))
  ;; Use direct async
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)
  (setq magit-tramp-pipe-stty-settings 'pty))

;; Use the built-in `whitespace-mode' to highlight incorrect indentation.
(use-package whitespace
  :ensure nil
  :preface
  (defun my/whitespace-highlight-incorrect-indentation ()
    "Highlight tabs if `indent-tabs-mode' is `nil', and highlight spaces at the
beginnings of lines if `indent-tabs-mode' is `t'. This makes any incorrect
indentation obvious. This functionality is inspired by Doom Emacs.

Does nothing if `global-whitespace-mode' is already active or if the current
buffer is in `fundamental-mode', read-only or not file-visiting."
    (unless (or (eq major-mode 'fundamental-mode)
                (bound-and-true-p global-whitespace-mode)
                (null buffer-file-name)
                buffer-read-only)
      (setq-local whitespace-style
                  (if indent-tabs-mode
                      '(face indentation)
                    '(face tabs tab-mark)))
      (whitespace-mode)))
  :init
  (add-hook 'after-change-major-mode-hook
            'my/whitespace-highlight-incorrect-indentation))

;;;;;;;;;;;;;;;;;;;;;;;
;; External packages ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Theme setup. auto-dark is used to automatically toggle between a light theme
;; and a dark theme depending on whether dark mode is enabled in the system.
(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((modus-vivendi-tinted) (modus-operandi-tinted)))
  (auto-dark-allow-powershell t)
  :init (auto-dark-mode))

;; Dumb Jump provides "jump to definition" functionality for many programming
;; languages. It is a zero-configuration alternative to stored indexes (TAGS)
;; and background processes (LSP) that only relies on ripgrep or grep.
(use-package dumb-jump
  :ensure t
  :init
  ;; Use `dumb-jump' as an `xref' backend.
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; Use `completion-read' instead of a separate buffer with candidates.
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;; Corfu enhances in-buffer completion. Candidates are shown in a popup below or
;; above the point, and can be selected by moving up and down.
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  ;; Auto completion.
  (corfu-auto t)
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 2)
  ;; Show docs sooner.
  (corfu-popupinfo-delay '(0.5 . 0.5))
  :bind (:map corfu-map
              ("RET" . nil)           ; Do not hijack my Enter key!
              ("TAB" . corfu-insert)) ; TAB autocompletes
  :init
  ;; Use corfu everywhere.
  (global-corfu-mode)
  ;; Save the completion history for better sorting.
  (corfu-history-mode)
  ;; Show documentation next to the first candidate.
  (corfu-popupinfo-mode))

;; Cape provides extra completions or capfs (`completion-at-point-functions').
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Vertico displays the minibuffer in a vertical layout. It achieves full
;; compatibility with built-in Emacs features, while being very performant and
;; minimalistic.
(use-package vertico
  :ensure t
  :custom
  (vertico-count 14)
  (vertico-resize nil)
  (vertico-cycle t)
  :init
  (vertico-mode)
  ;; Allow configuring Vertico per command or per completion category.
  (vertico-multiform-mode)
  ;; Display Embark keybind completions in a grid similar to `which-key'.
  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))))

;; Vertico directory extension. Provides commands for Ido-like directory
;; navigation (like backspace deleting the directory name).
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Marginalia adds colorful annotations placed at the margin of the minibuffer
;; for the completion candidates.
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;; The `orderless' completion style divides the pattern into space-separated
;; components, and matches candidates that match all of the components in any
;; order. This is similar to fuzzy finders like fzf.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides nil)
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

;; The `consult' package provides lots of commands that are enhanced variants of
;; built-in functionality. The commands improve the minibuffer by adding
;; advanced capabilities for asynchronous input, filtering, narrowing, and
;; previewing of the current candidate's context.
(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ;; imenu is very useful so it deserves its own short keybind
         ("M-i" . consult-imenu)
         ("M-I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s f" . consult-find)
         ("M-s d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :init
  ;; Use Consult to select xref locations.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Embark provides a keyboard-based version of a right-click contextual menu.
;; The `embark-act' command offers relevant actions to use on a target near
;; point, e.g. in the minibuffer the target is the top completion candidate.
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-," . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; Embark as a nice alternative to which-key.
  (setq prefix-help-command #'embark-prefix-help-command))

;; Better integration between Consult and Embark.
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; `transient' is needed by Magit. For some reason Elpaca cannot pull it as a
;; dependency so it must be installed explicitly.
(use-package transient
  :ensure t)

;; It's Magit!
(use-package magit
  :ensure t
  :after transient
  :bind ("C-x g" . magit-status)
  :custom
  ;; Show more granular diffs.
  (magit-diff-refine-hunk t)
  ;; Use auth-source for passwords.
  (magit-process-find-password-functions '(magit-process-password-auth-source)))

;; Source control gutter indicators.
(use-package diff-hl
  :ensure t
  :config
  ;; Refresh before and after Magit operations.
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  :custom
  ;; Slightly faster algorithm for diffing.
  (vc-git-diff-switches '("--histogram"))
  ;; Disable on remote buffers to improve performance.
  (diff-hl-disable-on-remote t)
  ;; Don't block Emacs when updating the gutter.
  (diff-hl-update-async t)
  ;; No borders. Solid colors are prettier.
  (diff-hl-draw-borders nil)
  :init
  (global-diff-hl-mode))

;; Colorful delimiters for programming modes.
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO, FIXME, NOTE, etc. in programming modes.
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

;; Smarter whitespace trimming. Only edited lines get meaningless whitespace
;; trimmed (e.g. end-of-line). Trimming only happens when saving.
(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

;; Guess the indentation offset originally used for creating source code files
;; and transparently adjust the corresponding settings, making it more
;; convenient to edit foreign files.
(use-package dtrt-indent
  :ensure t
  :init (dtrt-indent-global-mode))

;; Better mode line.
(use-package mood-line
  :ensure t
  :init (mood-line-mode))

;; Disable the mouse. I've used Emacs for a while now and I've acquired some
;; nasty habits during that time. Hopefully I can retrain myself.
(use-package disable-mouse
  :ensure t
  :init (global-disable-mouse-mode))

(use-package eglot
  :ensure nil
  :custom
  ;; Disable some annoying LSP features.
  (eglot-ignored-server-capabilities
   '(:semanticTokensProvider)))

(use-package treesit
  :ensure nil
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.23.4"))
               (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  :config
  (mp-setup-install-grammars)
  ;; Remap to tree-sitter major modes.
  (dolist (mapping
           '((c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (python-mode . python-ts-mode)
             (conf-toml-mode . toml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

;;;;;;;;;;;;;;;
;; Languages ;;
;;;;;;;;;;;;;;;

;; Markdown mode.
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do))
  :custom
  ;; Do syntax highlighting inside code blocks.
  (markdown-fontify-code-blocks-natively t))

;; C/C++ stuff (Tree-sitter).
(use-package c-ts-mode
  :ensure nil
  :preface
  (defun my/c-ts-indent-style ()
    "My custom C/C++ style based on K&R."
    `(;; Do not indent namespace children.
      ((n-p-gp nil nil "namespace_definition") grand-parent 0)
      ;; Do not indent preprocessor statements.
      ((node-is "preproc") column-0 0)
      ;; Indent the requires clause under the template line.
      ((parent-is "template_declaration") parent-bol c-ts-mode-indent-offset)
      ;; Align function arguments to the start of the first one, offset if standalone.
      ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
      ((parent-is "argument_list") (nth-sibling 1) 0)
      ;; Same for parameters.
      ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
      ((parent-is "parameter_list") (nth-sibling 1) 0)
      ;; Use k&r as base.
      ,@(alist-get 'k&r (c-ts-mode--indent-styles 'cpp))))
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style #'my/c-ts-indent-style))

;; CMake mode.
(use-package cmake-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;
;; Load custom lisp ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Load custom files.
(add-to-list 'load-path (expand-file-name "user-lisp" user-emacs-directory))
(require 'nasal-mode)

;; Load configuration specific to this Emacs installation, only if it exists.
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load-file local-file)))

(provide 'init)
