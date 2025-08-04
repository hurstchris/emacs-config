;; theme, see themes with M-x load-theme
(load-theme 'leuven-dark)

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
   helm-lsp))
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
;; (global-display-line-numbers-mode) ;; Set line #'s
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; set line #'s only for prog mode
(put 'erase-buffer 'disabled nil) ;; Don't ask warning for clear buffer (cause i dont use it enough)
(which-key-mode 1) ;; which key mode is nice
;; (advice-add 'list-buffers :after
;;   (lambda (&rest _) (other-window 1))) ;; buffer switching nonsense
(setq-default vc-handled-backends nil) ;; Dont allow git to do anything

;; ------ magit --------
(setq magit-diff-refine-hunk 'all)

;; ------ projectile config --------
(use-package projectile
  :init
  (setq projectile-enable-caching t)
  (setq projectile-enable-cmake-presets t)
  (setq projectile-enable-caching 'persistent)
  (global-set-key [f8] "\C-x\p\c\c") ;; Projectile compile to f8
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; (setq projectile-enable-caching t)
;; (setq projectile-enable-cmake-presets t)
;; (setq projectile-enable-caching 'persistent)
;; (global-set-key [f8] "\C-x\p\c\c") ;; Projectile compile to f8

;; ------ c++ configs --------
;; (add-hook 'c-mode-hook 'lsp)
;; (setq lsp-clients-clangd-executable "/usr/bin/clangd")
;; (setq lsp-clangd-binary-path "/usr/bin/clangd")
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.05)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))

(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode)) ;; add ipp files to cpp mode

;; ------ clang format --------
(require 'clang-format)
(add-hook 'c++-mode-hook 'clang-format+-mode)
(add-hook 'c-mode-hook 'clang-format+-mode)
(setq-default clang-format-fallback-style "llvm") ;; sets fallback clang-format
(setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "-j=4" "-background-index" "--header-insertion=iwyu" "--limit-references=0" "--limit-results=0")) ;; set some clang args
(setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "-j=4" "-background-index" "--clang-tidy")) ;; set some clang args

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
;; (helm-mode)
;; (require 'helm-xref)
;; (define-key global-map [remap find-file] #'helm-find-files)
;; (define-key global-map [remap execute-extended-command] #'helm-M-x)
;; (define-key global-map [remap switch-to-buffer] #'helm-mini)

;; ------ cmake lsp ------
(add-hook 'cmake-mode-hook 'lsp)

;; ------ python lsp ------
(add-hook 'python-mode-hook 'lsp)

;; ------ tramp --------
(require 'tramp)
(tramp-cleanup-all-connections)

;; ------ org mode --------
(setq org-default-notes-file (concat org-directory "~/org/notes.org"))
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-capture-templates
      '(("l" "Log" entry (file+datetree "~/org/notes.org")
         "* %? \n %a")))
(add-hook 'org-mode-hook 'org-indent-mode) ;; Make the indentation look nicer
(setq org-log-done 'time) ;; When a TODO is set to a done state, record a timestamp
(setq org-export-with-sub-superscripts '{})
(setq org-export-backends '(ascii beamer html latex md odt))
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (python . t)
   (latex . t)))

;; Org structure templates: C-c C-,
;; Autmatically add cpp code block with C-c b c
;; (defun my/org-insert-cpp-block ()
;;   (interactive)
;;   (insert "#+BEGIN_SRC C++ :results output :flags -std=c++23\n")
;;   (insert "#include <iostream>\n\n")
;;   (insert "int main() {\n")
;;   (insert "    return 0;\n")
;;   (insert "}\n")
;;   (insert "#+END_SRC\n"))
;; (define-key org-mode-map (kbd "C-c b c") #'my/org-insert-cpp-b
;;	    lock) ;; Bind to C-c b c
(add-to-list 'org-structure-template-alist '("p" . "src C++"))

(defun save-clang-format-org ()
  (interactive)
  (org-edit-src-code)
  (clang-format-buffer)
  (save-buffer)
  (org-edit-src-exit))

;; ------ compilation buffer font size --------
(defun my-compilation-mode-font-setup ()
  (face-remap-add-relative 'default :height 85)) ;; Adjust here
(add-hook 'compilation-mode-hook #'my-compilation-mode-font-setup)

;; ------ GDB ------
(setq gdb-many-windows 't)
(setq gdb-default-window-configuration-file "~/.emacs.d/.default-gdb-layout.config")
(setq gdb-debuginfod-enable-setting 't)

;; ------ JSON ------
(add-hook 'json-mode-hook 'flymake-json-load)

;; ------ diff colors -------

(defun update-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute 'diff-refine-added nil
                      :foreground "white" :background "darkgreen")
  (set-face-attribute 'diff-refine-removed nil
                      :foreground "white" :background "darkred")
  (set-face-attribute 'diff-removed nil
                      :foreground "#eecccc" :background "#663333")
  (set-face-attribute 'diff-indicator-removed nil
                      :foreground "#eecccc" :background "#663333")
  (set-face-attribute 'diff-added nil
                      :foreground "#cceecc" :background "#336633")
  (set-face-attribute 'diff-indicator-added nil
                      :foreground "#cceecc" :background "#336633"))
(eval-after-load "diff-mode"
  '(update-diff-colors))
