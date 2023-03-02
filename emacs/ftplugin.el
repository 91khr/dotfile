;; Agda mode
(add-to-list 'auto-mode-alist '("\\.lagda\\.md\\'" . agda2-mode))

(define-derived-mode lagda-md-mode markdown-mode "Literate Agda"
  "Major mode for literate Agda"
  (let ((vars (buffer-local-variables)))
    (agda2-mode)
    (dolist (p vars)
      (let ((var (car p)) (val (cdr p)))
        (ignore-error setting-constant
          (set var val))))))

