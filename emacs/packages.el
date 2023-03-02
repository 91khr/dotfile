;; Init packages
(require 'package)
(setq package-archives
      '(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("elpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
        ;;("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(when (not (boundp 'package-list))
  (setq package-list
        '(powerline neotree use-package markdown-mode solarized-theme racket-mode company meow
                    switch-window polymode poly-markdown)))

;; Check for uninstalled packages and install them
(let ((package-install-list (seq-remove #'package-installed-p package-list)))
  (unless (eq nil package-install-list)
    (package-refresh-contents)
    (dolist (p package-install-list) (package-install p))))

(require 'use-package)

;; Individual package config
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (load-file (concat dotdir "pkgconf/markdown.el")))

(use-package meow
  :config (load-file (concat dotdir "pkgconf/meow.el")))

(use-package racket-mode
  :defer t
  :mode (("\\.rkt\\'" . racket-mode))
  :config (load-file (concat dotdir "pkgconf/racket.el")))

