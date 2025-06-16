;; theme, see themes with M-x load-theme
(load-theme 'leuven-dark)

;; ------ Packages --------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; package thingy
;; Add packages here to autmatically install them
(setq
 package-selected-packages
 '(magit projectile lsp-mode lsp-treemacs hydra flycheck company which-key yasnippet clang-format))
(package-install-selected-packages 1)
(package-autoremove)

;; Stop the bell
(setq ring-bell-function 'ignore)

;; No Startup screen
(setq inhibit-startup-screen t)

;; Window moving
(keymap-global-set "C-x <up>" 'windmove-up)
(keymap-global-set "C-x <down>" 'windmove-down)
(keymap-global-set "C-x <left>" 'windmove-left)
(keymap-global-set "C-x <right>" 'windmove-right)

;; Replaces highlighted text
(delete-selection-mode 1)

;; Global auto revert
(global-auto-revert-mode 1)

;; projectile config
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; which key mode is nice
(which-key-mode 1)

;; c++ stuff
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))
