(require 'smartparens-config)
(smartparens-global-strict-mode)

(setq meow-paren-keymap (define-keymap :parent meow-normal-state-keymap))
(meow-define-state paren
  "Meow state for parens editing"
  :lighter "[NP]"
  :keymap meow-paren-keymap)

(defun cfg-meow-paren-kill ()
  (interactive)
  (if (region-active-p) (meow-kill) (sp-kill-sexp)))

(meow-define-keys 'paren
  '("]" . sp-down-sexp)
  '("[" . sp-up-sexp)
  '("{" . meow-beginning-of-thing)
  '("}" . meow-end-of-thing)
  '("e" . sp-beginning-of-next-sexp)
  '("b" . sp-beginning-of-next-sexp)
  '("<" . sp-beginning-of-sexp)
  '(">" . sp-end-of-sexp)
  '("s" . cfg-meow-paren-kill))

(sp-with-modes sp-lisp-modes
  (sp-local-pair "[" "]" :unless '(sp-in-string-p))
  (sp-local-pair "{" "}" :unless '(sp-in-string-p sp-in-comment-p))
  (sp-local-pair "(" ")" :unless '(sp-in-string-p sp-in-comment-p)))

(add-hook 'smartparens-mode-hook
          (lambda () (meow-paren-mode) (smartparens-strict-mode)))

