;;; package setup
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-install-upgrade-built-in t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Force to get the newest version of org
(package-install 'org)

(require 'use-package)

(setq use-package-always-ensure t)
(setq use-package-expand-minimally t)

;; theme, see themes with M-x load-theme
(use-package modus-themes
  :ensure t
  :demand t
  :init
  (modus-themes-include-derivatives-mode 1)
  :config
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-to-rotate modus-themes-items
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))
  (setq modus-themes-common-palette-overrides nil)
  (modus-themes-load-theme 'modus-vivendi-tinted))

;; ------ Display --------
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t
      initial-scratch-message ""
      visible-bell t
      x-stretch-cursor t)

(setq-default frame-title-format "%b - Chris' emacs")

(delete-selection-mode 1)
(global-auto-revert-mode 1)
(setq auto-revert-remote-files t)
;; (setq split-height-threshold nil)

;; ------ Key bindings --------
(global-set-key (kbd "C-x <up>") 'windmove-up) ;; moving around
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(global-set-key "\M-g" 'goto-line) ;; goto line
(global-set-key "\C-c\C-v" 'uncomment-region) ;; comment region
(global-set-key "\C-x\C-b" 'buffer-menu) ;; I think this buffer menu (not really)

;; ------ Other --------
(delete-selection-mode 1) ;; Replaces highlighted text
(setq ring-bell-function 'ignore) ;; Stop the bell
(global-auto-revert-mode 1) ;; Global auto revert
(setq auto-revert-remote-files t) ;; Revert remote files as well
(setq ring-bell-function 'ignore) ;; Stop the bell
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; set line #'s only for prog mod
(put 'erase-buffer 'disabled nil) ;; Don't ask warning for clear buffer (cause i dont use it enough)
(setq-default enable-remote-dir-locals t)
(setq completion-auto-help 'always)
(add-hook 'c++-mode-hook 'electric-pair-mode) ;; auto closing parens for c++ and python
;; (add-hook 'python-mode-hook 'electric-pair-mode) ;; dont do it for pyton for now

;; ------- diff-hl -------
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1))

;;------------------------------------------------------------------------------
;; Avy
(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-word-or-subword-1)
         ("C-," . avy-pop-mark)
         ("C-j" . avy-goto-char-timer)))

;;------------------------------------------------------------------------------
;; visual-regexp-steroids: better regexp searching
(use-package visual-regexp-steroids
  :ensure t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-r" . vr/isearch-backward)
         ("C-s" . vr/isearch-forward)))

;; use case-insensitive searching in general
(setq case-fold-search t)

;; ------- which-key mode --------
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))
 
;; ----- super-save -----
;; Basically onlty use this when you compile
;; lsp has some weird window changing behavior that
;; triggers this
(use-package super-save
  :ensure t
  :config
  (setq super-save-triggers nil)
  (setq super-save-when-buffer-switched nil)
  (setq super-save-silent nil)
  (add-to-list 'super-save-hook-triggers 'compile)
  (add-to-list 'super-save-hook-triggers 'projectile-compile-project)
  (super-save-mode +1))

;; ----- vertico -------
(use-package vertico
  :ensure t
  :config
  (setq vertico-count 30)
  :init (vertico-mode))

;; This next stuff is more for lsp
(setq xref-show-xrefs-function #'xref-show-definitions-completing-read
      xref-show-definitions-function #'xref-show-definitions-completing-read)
;; places results in another buffer
;; (setq xref-show-xrefs-function #'xref--show-xref-buffer)

;; ----- consult ------
(use-package consult
  ;; ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ;; ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g r" . consult-grep-match)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
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
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                ;; orig. previous-matching-history-element
	 )

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

;; ----- marginalia ------
(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; ----- embark -----
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t) ; only need to install it, embark loads it after consult if found

(use-package imenu-list
  :ensure t
  :bind (("C-'" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil))


;; ----- direnv setup ------
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; ------ yasnippet ------
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (yas-global-mode 1))

;; ------ magit --------
(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (add-hook 'magit-post-refresh-hook
            #'diff-hl-magit-post-refresh))

;; ---- treemacs -----
(use-package treemacs
  :bind ([f9] . treemacs)
  :config (setq treemacs-space-between-root-nodes t))

;; ------ projectile config --------
(use-package projectile
  :bind (([f8] . projectile-compile-project)
         ([f7] . projectile-test-project)
         ([f6] . projectile-configure-project))
  :custom
  (projectile-enable-cmake-presets t)
  (projectile-enable-caching 'persistent)
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map
              (kbd "C-x p")
              'projectile-command-map))

;; ------ lsp configs --------
(use-package lsp-mode
  :hook ((c++-mode . lsp)
         (python-mode . lsp-deferred))
  :custom
  (lsp-idle-delay 0.1)
  (lsp-auto-guess-root t)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  :config
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-clients-clangd-executable "/usr/bin/clangd")
  (setq lsp-clangd-binary-path "/usr/bin/clangd")
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (add-hook 'lsp-mode-hook
            #'lsp-enable-which-key-integration))

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;   (require 'dap-gdb)
;;   (require 'dap-python)
;;   (setq lsp-semantic-tokens-enable t))

;; Idk why this is outside the config
(setq lsp-format-buffer-on-save t)

;; ------ c++ configs --------
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode)) ;; add ipp files to cpp mode

;; clang-format files are YAML
(add-to-list 'auto-mode-alist '("\\.clang-format\\'" . yaml-mode))

;; ------ dap mode ------
;; To get windows to work properly, when you open dap-debug
;; run M-x dap-ui-many-windows-mode
;; it turns it off...
(use-package dap-mode
  :after lsp-mode
  :custom
  (dap-auto-configure-features
   '(sessions locals breakpoints expressions controls tooltip))
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-gdb)
  (require 'dap-python))

;; ------ dape ------
(use-package transient
  :config
  (transient-define-prefix my/dape-transient ()
    "Dape transient."
    [["Breakpoints"
      ("b" "Toggle breakpoint"
       dape-breakpoint-toggle)]]))

(use-package dape
  :bind
  (:map dape-mode-map
        ("C-c d" . my/dape-transient)
        :map dape-repl-mode-map
        ("C-c d" . my/dape-transient))
  :custom
  (dape-breakpoint-global-mode +1)
  (dape-info-hide-mode-line nil)
  (dape-cwd-function #'projectile-project-root)
  :config
  (add-hook 'dape-display-source-hook
            #'pulse-momentary-highlight-one-line))

(use-package emacs
  :custom
  (window-sides-vertical t))

;; ------ rmsbolt -----
(use-package rmsbolt
  :ensure t
  :config
  (setq rmsbolt-compile-delay 10)
  (setq rmsbolt-automatic-recompile nil))

;; Note this will only make rmsbolt work for cpp
(defun my/rmsbolt-after-cpp (&rest _args)
  (when (string-match-p "\\.cpp\\'" (buffer-name (current-buffer)))
    (sleep-for 0.1)
    (compile (concat rmsbolt-command
                     " rmsbolt.cpp -o a.out && ./a.out"))))
(advice-add 'rmsbolt-compile :after #'my/rmsbolt-after-cpp)
;; This is for python
(defun my/rmsbolt-after-python (&rest _args)
  (when (string-match-p "\\.py\\'" (buffer-name (current-buffer)))
    (sleep-for 0.1)
    (compile (concat rmsbolt-command
                     " rmsbolt.py"))))
(advice-add 'rmsbolt-compile :after #'my/rmsbolt-after-python)
(add-to-list 'safe-local-variable-values
             '(rmsbolt-disassemble . "objdump -d -M intel"))

(advice-add 'rmsbolt-compile
            :before
            (lambda (&rest _)
              (save-buffer)))

;; ------ clang format --------
;; trying lsp for now
;; (use-package clang-format
;;   :ensure t
;;   :config
;;   ;; (setq-default clang-format-on-save-mode t)
;;   ;; (setq-default clang-format-fallback-style "llvm") ;; sets fallback clang-format
;;   (setq-default clang-format-style "file:/home/chris/.emacs.d/cpp-tools/.clang-format"))

;; (use-package clang-format+
;;   :ensure t
;;   :hook ((c-mode . clang-format+-mode)
;;          (c++-mode . clang-format+-mode)))

(setq lsp-clients-clangd-args '("--header-insertion=iwyu" "--header-insertion-decorators" "-j=4" "--background-index" "--clang-tidy" "--fallback-style=GNU")) ;; set some clang args

;; ------ python lsp ------
;; For a new system, run
;; uv tool install ruff@latest
;; uv tool install pyright
;; For dape, youll need debugpy in the venv unfortunately
;; for global lsp stuff

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright"))

(use-package ruff-format
  :ensure t
  :hook (python-mode . ruff-format-on-save-mode))

;; ------ emacs pet --------
;; this might be slow
(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; ------ tramp --------
(use-package tramp
  :config
  (tramp-cleanup-all-connections))

;; ------ org mode --------
(use-package org
  :ensure t
  :config
  (with-eval-after-load 'org (keymap-unset org-mode-map "C-'")) ;; this is to get my imenu-list
  :pin gnu)

(setq org-default-notes-file "~/org/notes.org")
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-capture-templates
      '(("l" "Log" entry (file+datetree "~/org/notes.org")
         "* %? \n %a")
      ("r" "Random Notes" entry (file+datetree "/home/chris/blog/org/daily-notes/daily-notes.org")
       "* %?")
      ("c" "C++ Notes" entry (file "/home/chris/blog/org/cpp-notes/cpp-notes.org")
         "* %?")))
(add-hook 'org-mode-hook 'org-indent-mode) ;; Make the indentation look nicer
(setq org-log-done 'time) ;; When a TODO is set to a done state, record a timestamp
(setq org-export-with-sub-superscripts '{})
(setq org-export-backends '(ascii beamer html latex md odt))
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t) ;; syntax highlighting in org
(setq org-src-tab-acts-natively nil) ;; syntax highlighting in org
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (python . t)
   (emacs-lisp . t)
   (shell . t)
   (latex . t)
   (gnuplot . t)))
;; add additional languages with (language . t)))

;; When in an org file, use the scratch venv
(setq org-babel-python-command "~/.venv/bin/python")

;; Spell checking in org mode
(add-hook 'org-mode-hook 'flyspell-mode)

;; Org structure templates: C-c C-,
(add-to-list 'org-structure-template-alist
             '("g" . "src C++ :includes iostream :flags -std=c++23\nstd::cout << \"hello world\\n\";"))
(add-to-list 'org-structure-template-alist
             '("p" . "src python  :results output\nprint(\"hello world\")"))

;; save src block
(setq org-edit-src-auto-save-idle-delay 0.5)
(setq org-edit-src-turn-on-auto-save t)

;; blog publishing
(defun publish-all ()
  "Publish entire blog to html"
  (interactive)
  (load "~/blog/publish.el"))

(defalias 'clangformatbabelblock
   (kmacro "C-c ' M-x c l a n g - f o r m a t - b u f f e r <return> C-c '"))
(global-set-key (kbd "C-c f") 'clangformatbabelblock)

;; This lets src block publishing to look like your emacs
(use-package engrave-faces
  :ensure t
  :after org
  :config
  (setq org-latex-src-block-backend 'engraved)
  )


;; ------ compilation buffer --------
(defun my-compilation-mode-font-setup ()
  (face-remap-add-relative 'default :height 100)) ;; Adjust here
(add-hook 'compilation-mode-hook #'my-compilation-mode-font-setup)
(add-to-list
 'display-buffer-alist
 '("\\*compilation\\*"
   (display-buffer-in-side-window)
   (side . bottom)
   (slot . 0)
   (window-height . 0.3)))

(setq compilation-scroll-output t)
(setq compilation-auto-jump-to-first-error nil)
(setq compilation-max-output-line-length nil)
;; Other alist commands:
;;(display-buffer-at-bottom)
;;(display-buffer-same-window)
;;(display-buffer-below-selected)
;;(display-buffer-in-side-window)

(defun ar/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'ar/colorize-compilation-buffer)

;; ------ rgrep ------
(global-set-key [f10] 'rgrep)
(add-to-list
 'display-buffer-alist
 '("\\*grep\\*"
   (display-buffer-in-side-window)
   (side . left)
   (slot . 0)
   (window-width . 0.3)))

(setq compilation-always-kill t)

(defun my/close-compilation-buffer ()
  (interactive)
  (let ((buf (get-buffer "*compilation*")))
    (when buf
      (delete-windows-on buf)
      (kill-buffer buf))))

(defun my/close-compilation-window-on-success (buf str)
  "Close compilation window after successful non-RMSBolt compilations."
  (with-current-buffer buf
    (unless (or (string-match-p "exited abnormally" str)
                (derived-mode-p 'grep-mode)
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "/tmp/rmsbolt-" nil t)))

      (run-at-time
       "1 sec" nil
       (lambda (buffer)
         (when (buffer-live-p buffer)
           (delete-windows-on buffer)))
       buf)

      (message "No compilation errors!"))))

(add-hook 'compilation-finish-functions
          #'my/close-compilation-window-on-success)

(defun my/close-grep-buffer ()
  (interactive)
  (let ((buf (get-buffer "*grep*")))
    (when buf
      (delete-windows-on buf)
      (kill-buffer buf))))

(global-set-key (kbd "C-<f8>") #'my/close-compilation-buffer)
(global-set-key (kbd "C-<f10>") #'my/close-grep-buffer)

;; ------ GDB ------
(setq gdb-many-windows 't)
(setq gdb-default-window-configuration-file "~/.emacs.d/.default-gdb-layout.config")
(setq gdb-debuginfod-enable-setting 't)

;; ------ JSON ------
(use-package json-mode
  :hook (json-mode . flymake-json-load))

;; ------ epub reader -------
(add-to-list 'auto-mode-alist
             '("\\.epub\\'" . nov-mode))
