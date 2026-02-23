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
   helm-xref
   helm-lsp
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
   pet
   python-pytest
   lsp-pyright
   ruff-format))
(package-install-selected-packages 1)
(package-autoremove)

;; ------ Display --------
(setq-default frame-title-format "%b - Chris' emacs") ;; Set frame title of emacs
(tool-bar-mode -1) ;; tool bar
(menu-bar-mode -1) ;; No menu bar
(scroll-bar-mode -1) ;; No scroll bar
(setq inhibit-startup-screen t) ;; No Startup screen
;; (set-face-attribute 'default nil :height 95) ;; Set default font sz

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
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.05)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-gdb)
  (require 'dap-python)
  (setq lsp-semantic-tokens-enable t))

;; (setq lsp-format-buffer-on-save t)

;; ------ c++ configs --------
(add-hook 'c++-mode-hook 'lsp)
;; (add-hook 'c-mode-hook 'lsp)
;; (setq lsp-clients-clangd-executable "/usr/bin/clangd")
;; (setq lsp-clangd-binary-path "/usr/bin/clangd")

(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode)) ;; add ipp files to cpp mode

;; ------ dap mode ------
;; To get windows to work properly, when you open dap-debug
;; run M-x dap-ui-many-windows-mode
;; it turns it off...
(setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip))
(setq dap-python-debugger 'debugpy)

;; ------ dape ------
(set-fringe-mode 12)
(use-package dape
  :custom
  (dape-breakpoint-global-mode +1)
  (dape-info-hide-mode-line nil)
  (dape-cwd-function #'projectile-project-root)
  :config
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  )
(use-package repeat
  :custom
  (repeat-mode +1))
(use-package emacs
  :custom
  (window-sides-vertical t))

;; ------ clang format --------
(require 'clang-format)
(add-hook 'c++-mode-hook 'clang-format+-mode)
(add-hook 'c-mode-hook 'clang-format+-mode)
(setq-default clang-format-style "file:/home/chris/.emacs.d/cpp-tools/.clang-format")
(setq-default clang-format-on-save-mode t)
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
(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; ------ tramp --------
(require 'tramp)
(tramp-cleanup-all-connections)

;; ------ org mode --------
(setq org-default-notes-file "~/org/notes.org")
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-capture-templates
      '(("l" "Log" entry (file+datetree "~/org/notes.org")
         "* %? \n %a")))
(add-hook 'org-mode-hook 'org-indent-mode) ;; Make the indentation look nicer
(setq org-log-done 'time) ;; When a TODO is set to a done state, record a timestamp
(setq org-export-with-sub-superscripts '{})
(setq org-export-backends '(ascii beamer html latex md odt))
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t) ;; syntax highlighting in org
(setq org-src-tab-acts-natively t) ;; syntax highlighting in org
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
   (display-buffer-at-bottom)
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
(add-to-list
 'display-buffer-alist
 '("\\*grep\\*"
   (display-buffer-below-selected)
   (window-height . 0.3)))

;; ------ GDB ------
(setq gdb-many-windows 't)
(setq gdb-default-window-configuration-file "~/.emacs.d/.default-gdb-layout.config")
(setq gdb-debuginfod-enable-setting 't)

;; ------ JSON ------
(add-hook 'json-mode-hook 'flymake-json-load)

;; ------ epub reader -------
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
