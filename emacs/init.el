;; Set the custom file
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;; Load local configs
(setq dotdir (file-name-directory load-file-name))
(load-file (concat dotdir "packages.el"))
(load-file (concat dotdir (if (display-graphic-p) "gui.el" "tui.el")))

;; Configs
(global-visual-line-mode)
(global-linum-mode t)
(show-paren-mode)

(use-package powerline)
(powerline-default-theme)

(evil-mode)
(global-company-mode)

