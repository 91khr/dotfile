;; Init packages
(require 'package)
(setq package-archives
      '(("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
        ("elpa" . "http://mirrors.ustc.edu.cn/elpa/gnu/")))
        ;;("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(setq package-list
      '(powerline neotree use-package markdown-mode solarized-theme racket-mode company meow switch-window
                  org-fragtog vertico orderless corfu marginalia slime smartparens rime))

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

(use-package racket-mode
  :defer t
  :mode (("\\.rkt\\'" . racket-mode))
  :config (lambda ()
            (load-file (concat dotdir "pkgconf/racket.el"))
            (add-hook racket-mode-hook (lambda ()
                                         (setq eldoc-documentation-function #'racket-xp-eldoc-function)))))

(use-package rime
  :defer t
  :hook (markdown-mode org-mode telega-load)
  :config
  (add-hook 'telega-chat-mode-hook
            (lambda () (setq-local seni-meow-last-imstate "rime"))))

(use-package vertico
  :init (vertico-mode))
(use-package marginalia
  :init (marginalia-mode))

(use-package savehist
  :init (savehist-mode))
(use-package corfu
  :init (progn
          (global-corfu-mode))
  :custom (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-popupinfo-mode t)
  :bind (:map corfu-map ("C-[" . #'corfu-quit)))

(use-package telega
  :defer t
  :config
  (define-advice telega-chars-xheight
      (:around (orig n &optional face) k)
    (+ (funcall orig n face) (* n 10)))
  (telega-notifications-mode)
  (add-hook 'telega-chat-mode-hook (lambda ()
                                     (auto-fill-mode -1))))

(use-package slime
  :defer t
  :config
  (setq slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))))
  (setq inferior-lisp-program "sbcl"))

(use-package smartparens
  :hook (emacs-lisp-mode lisp-mode scheme-mode slime-repl-mode slime-mrepl-mode)
  :config (load-file (concat dotdir "pkgconf/smartparens.el")))

(use-package meow
  :config (load-file (concat dotdir "pkgconf/meow.el")))

(add-hook 'org-mode-hook 'org-fragtog-mode)

