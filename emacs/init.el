;; Set the custom file
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;; Load local configs
(let ((dotdir (file-name-directory load-file-name)))
  (load-file (concat dotdir "packages.el"))
  (mapc (lambda (f) (load-file f))
        (directory-files (concat dotdir "pkgconf") t "\.el$" t))
  (load-file (concat dotdir (if (display-graphic-p) "gui.el" "tui.el")))
  '())

(global-visual-line-mode)
(global-linum-mode t)
(show-paren-mode)

(use-package powerline)
(powerline-default-theme)

