;; ------ Packages --------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Setting custom package directory
;; (setq package-user-dir "~/.emacs.d/elpa") ;; as an example
(package-initialize)

;; package thingy
;; Add packages here to autmatically install them
(setq
 package-selected-packages
 '(fireplace
   async
   magit
   projectile
   treemacs
   treemacs-projectile
   treemacs-tab-bar
   lsp-mode
   lsp-ui
   lsp-treemacs
   hydra
   flycheck
   company
   avy
   which-key
   yasnippet
   clang-format
   clang-format+
   tramp
   xclip
   ox-clip
   org
   cmake-mode
   dockerfile-mode
   json-mode
   flymake-json
   diff-hl
   rmsbolt
   gnuplot
   gnuplot-mode
   nov
   pyvenv
   dap-mode
   modus-themes
   yasnippet-snippets
   yaml-mode
   envrc
   ibuffer-projectile
   dape
   python-pytest
   lsp-pyright
   ruff-format
   vertico
   consult
   marginalia
   embark
   embark-consult
   super-save))
(package-install-selected-packages 1)
(package-autoremove)

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
(setq-default frame-title-format "%b - Chris' emacs") ;; Set frame title of emacs
(tool-bar-mode -1) ;; tool bar
(menu-bar-mode -1) ;; No menu bar
(scroll-bar-mode -1) ;; No scroll bar
(setq inhibit-startup-screen t) ;; No Startup screen
;; (set-face-attribute 'default nil :height 95) ;; Set default font sz
(setq split-height-threshold nil)

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
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; set line #'s only for prog mode
(put 'erase-buffer 'disabled nil) ;; Don't ask warning for clear buffer (cause i dont use it enough)
(which-key-mode 1) ;; which key mode is nice
(global-diff-hl-mode) ;; highlights changes
(setq-default enable-remote-dir-locals t)
(setq completion-auto-help 'always)

;; ----- super-save -----
(use-package super-save
  :ensure t
  :config
  (setq super-save-triggers
        '(consult-buffer
          switch-to-buffer
          other-window
          windmove-left
          windmove-down
          windmove-up
          windmove-right))
  (super-save-mode +1))

;; ----- vertico -------
(use-package vertico :init (vertico-mode))
(setq vertico-count 30)
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
         ("C-c m" . consult-man)
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

;; ----- direnv setup ------
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; ------ yasnippet ------
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(yas-global-mode)

;; ------ magit --------
(setq magit-diff-refine-hunk 'all)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; ---- treemacs -----
(global-set-key [f9] 'treemacs) ;; Projectile compile to f8

;; ------ projectile config --------
(use-package projectile
  :init
  (setq projectile-enable-cmake-presets t)
  (setq projectile-enable-caching t)
  (setq projectile-enable-caching 'persistent)
  (global-set-key [f8] 'projectile-compile-project) ;; Projectile compile to f8
  (global-set-key [f7] 'projectile-test-project)
  (global-set-key [f6] 'projectile-configure-project)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

;; ------ lsp configs --------
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes t
      company-idle-delay 0.5
      company-minimum-prefix-length 1
      lsp-idle-delay 0.5
      lsp-auto-guess-root t)



(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-gdb)
  (require 'dap-python)
  (setq lsp-semantic-tokens-enable t))

;; (setq lsp-format-buffer-on-save t)

;; ------ c++ configs --------
(add-hook 'c++-mode-hook 'lsp)
;; (add-hook 'c-mode-hook 'lsp)
(setq lsp-clients-clangd-executable "/usr/bin/clangd")
(setq lsp-clangd-binary-path "/usr/bin/clangd")

(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode)) ;; add ipp files to cpp mode

;; ------ dap mode ------
;; To get windows to work properly, when you open dap-debug
;; run M-x dap-ui-many-windows-mode
;; it turns it off...
(setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip))
(setq dap-python-debugger 'debugpy)

;; ------ dape ------
(set-fringe-mode 12)

(require 'transient)

(transient-define-prefix my/dape-transient ()
  "Dape transient."
  [["Breakpoints"
    ("b" "Toggle breakpoint" dape-breakpoint-toggle)]])

(defun my/dape-keys ()
  (local-set-key (kbd "C-c d") #'my/dape-transient))


(use-package dape
  :hook
  ((dape-mode . my/dape-keys)
   (dape-repl-mode . my/dape-keys))
  :custom
  (dape-breakpoint-global-mode +1)
  (dape-info-hide-mode-line nil)
  (dape-cwd-function #'projectile-project-root)
  :config
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  )
;; (use-package repeat
;;   :custom
;;   (repeat-mode +1))
(use-package emacs
  :custom
  (window-sides-vertical t))

;; ------ rmsbolt -----
(defun my/rmsbolt-after (&rest _args)
  (compile (concat rmsbolt-command " rmsbolt.cpp -o a.out && ./a.out")))
(advice-add 'rmsbolt-compile :after #'my/rmsbolt-after)
(add-to-list 'safe-local-variable-values
             '(rmsbolt-disassemble . "objdump -d -M intel"))

;; ------ clang format --------
(require 'clang-format)
(add-hook 'c++-mode-hook 'clang-format+-mode)
(add-hook 'c-mode-hook 'clang-format+-mode)
(setq-default clang-format-style "file:/home/chris/.emacs.d/cpp-tools/.clang-format")
;; (setq-default clang-format-on-save-mode t)
;; (setq-default clang-format-fallback-style "llvm") ;; sets fallback clang-format
;; (setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "-j=4" "-background-index" "--header-insertion=iwyu" "--limit-references=0" "--limit-results=0")) ;; set some clang args
(setq lsp-clients-clangd-args '("--header-insertion=never" "--header-insertion-decorators=0" "-j=4" "-background-index" "--clang-tidy")) ;; set some clang args

;; ------ python lsp ------
;; For a new system, run
;; uv tool install ruff@latest
;; uv tool install pyright
;; For dape, youll need debugpy in the venv unfortunately
;; for global lsp stuff

(add-hook 'python-mode-hook 'lsp-deferred)

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred

(require 'ruff-format)
(add-hook 'python-mode-hook 'ruff-format-on-save-mode)

;; ------ emacs pet --------
;; this is way too slow, use direnv or something
;; (use-package pet
;;   :config
;;   (add-hook 'python-base-mode-hook 'pet-mode -10))

;; ------ tramp --------
(require 'tramp)
(tramp-cleanup-all-connections)

;; ------ org mode --------
(setq org-default-notes-file "~/org/notes.org")
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-capture-templates
      '(("l" "Log" entry (file+datetree "~/org/notes.org")
         "* %? \n %a")
      ("r" "Random Notes" entry (file+datetree "/home/chris/org/test-blog/org/daily-notes/daily-notes.org")
       "* %?")
      ("c" "C++ Notes" entry (file "/home/chris/org/test-blog/org/cpp-notes/cpp-notes.org")
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

;; Org structure templates: C-c C-,
(add-to-list 'org-structure-template-alist
             '("g" . "src C++ :includes iostream :flags -std=c++23\nstd::cout << \"hello world\\n\";"))
(add-to-list 'org-structure-template-alist
             '("p" . "src python  :results output\nprint(\"hello world\")"))

;; save src block
(setq org-edit-src-auto-save-idle-delay 0.5)
(setq org-edit-src-turn-on-auto-save t)

(defalias 'clangformatbabelblock
   (kmacro "C-c ' M-x c l a n g - f o r m a t - b u f f e r <return> C-c '"))
(global-set-key (kbd "C-c f") 'clangformatbabelblock)

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

;; ------ GDB ------
(setq gdb-many-windows 't)
(setq gdb-default-window-configuration-file "~/.emacs.d/.default-gdb-layout.config")
(setq gdb-debuginfod-enable-setting 't)

;; ------ JSON ------
(add-hook 'json-mode-hook 'flymake-json-load)

;; ------ epub reader -------
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
