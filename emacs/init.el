;; Set the custom file
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;; Load local configs
(setq dotdir (file-name-directory load-file-name))
(dolist (f '("packages.el" "ftplugin.el"))
         (load-file (concat dotdir f)))
(load-file (concat dotdir (if (display-graphic-p) "gui.el" "tui.el")))

;; Configs
(global-visual-line-mode)
(global-linum-mode t)
(show-paren-mode)

(setq-default indent-tabs-mode nil)
(setq-default fill-column 120)
(setq-default auto-fill-function 'do-auto-fill)
(global-display-fill-column-indicator-mode t)

(use-package powerline)
(powerline-default-theme)

;;(meow-mode)
(require 'meow)
(meow-setup)
(meow-global-mode 1)

;; switch-window
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

(global-company-mode)
