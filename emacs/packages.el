;; Init packages
(require 'package)
(setq package-archives
      '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(when (not (boundp 'package-list))
  (setq package-list
        '(powerline neotree use-package markdown-mode solarized-theme evil)))

;; Check for uninstalled packages and install them
(let ((package-install-list '())
      (ensure-package
        (lambda (name)
          (unless (package-installed-p name)
            (setq package-install-list (cons name package-install-list))))))
  (cl-loop for p in package-list do (funcall ensure-package p))
  (unless (eq nil package-install-list)
    (package-refresh-contents)
    (cl-loop for p in package-install-list do (package-install p))))

(require 'use-package)

