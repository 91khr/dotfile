;; Init packages
(require 'package)
(setq package-archives
      '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(if (not (boundp 'package-list))
    (setq package-list
      '(powerline neotree use-package markdown-mode solarized-theme evil)))
(let ((package-install-list '())
      (ensure-package
       (lambda (name) (unless (package-installed-p name)
     (setq package-install-list (cons name package-install-list))))))
  (cl-loop for p in package-list do (funcall ensure-package p))
  (unless (eq nil package-install-list)
    (package-refresh-contents)
    (cl-loop for p in package-install-list
             do (package-install p))))

;; Custom
(custom-set-variables
 ;; Dont show the startup screen
 '(inhibit-startup-screen t)
 ;; Markdown settings
 '(markdown-enable-math t)
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-header-scaling t)
 ;; Packages
 '(package-selected-packages
   (quote package-list))
 ;; No backup files
 '(make-backup-files nil)
 ;; Continue lines
 '(truncate-lines nil)
 '(truncate-partial-width-windows 30))
(custom-set-faces
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2)))))
;; Prevent emacs from pouring trash into init file
(setq custom-file null-device)



(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (progn
          (setq markdown-command "pandoc")))

;; Appearance settings
(setq face-font-rescale-list '(("WenQuanYi Zen Hei" . 1.2)))
(setq font-use-system-font t)
(add-to-list 'default-frame-alist
             '(font . "monaco-10"))
;(set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Zen Hei")

(if (display-graphic-p)
  (load-theme 'solarized-light t)
  (load-theme 'solarized-zenburn t))
(global-linum-mode t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(show-paren-mode)
(global-visual-line-mode)

(use-package powerline)
(powerline-default-theme)

;; Evil settings
(evil-mode)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
