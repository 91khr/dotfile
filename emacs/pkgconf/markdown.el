(setq markdown-command "pandoc")
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (company-mode -1)))

