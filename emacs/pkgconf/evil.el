;; gj and gk
(evil-define-key 'normal 'global (kbd "j") 'evil-next-visual-line)
(evil-define-key 'normal 'global (kbd "k") 'evil-previous-visual-line)
(evil-define-key 'normal 'global (kbd "gj") 'evil-next-line)
(evil-define-key 'normal 'global (kbd "gk") 'evil-previous-line)

;; Neotree
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;; Info
(define-key Info-mode-map (kbd "C-n") 'Info-forward-node)
(define-key Info-mode-map (kbd "C-p") 'Info-backward-node)

(setq evil-undo-system 'undo-redo)

