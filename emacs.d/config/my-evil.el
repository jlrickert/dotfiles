;;; my-evil.el --- Loads evil packages
;;; Commentary:

;;; Code:
(message "Loading my-evil")
(use-package evil
  :config
  (evil-mode 1))
(use-package evil-indent-textobject :after evil)
(use-package evil-leader
  :after evil
  :config
  (evil-leader/set-leader "SPC")
  (global-evil-leader-mode))
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))
(use-package evil-nerd-commenter
  :after (evil evil-leader)
  :commands evilnc-comment-or-uncomment-lines
  :config
  (evil-leader/set-key "/" 'evilnc-comment-or-uncomment-lines)
  )
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(use-package key-chord
  :config
  (key-chord-mode t))


(provide 'my-evil)
;;; my-evil.el ends here
