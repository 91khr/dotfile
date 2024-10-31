;; The mirror of custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:jupyter-server-use-subcommand "server")
 '(ein:output-area-inlined-images t)
 '(evil-undo-system 'undo-redo)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(markdown-enable-highlighting-syntax t)
 '(markdown-enable-math t)
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-header-scaling t)
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(ein jupyter cape slime-company ement rime smartparens slime marginalia corfu orderless vertico telega switch-window magit-section lsp-mode flycheck f dash powerline neotree use-package markdown-mode solarized-theme evil racket-mode company meow))
 '(smtpmail-smtp-server "localhost")
 '(smtpmail-smtp-service 1025)
 '(telega-server-libs-prefix "/usr")
 '(truncate-lines nil)
 '(truncate-partial-width-windows 30))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit normal))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2)))))
