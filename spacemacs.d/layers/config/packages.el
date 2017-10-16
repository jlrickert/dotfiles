;;; Config Layer
;;;; Evil

(setq config-packages
      '(
        evil
        helm
        projectile
        smartparens
        yasnippet
        ))

(defun config/post-init-evil ()
  (evil-global-set-key 'normal ";" 'evil-ex)
  (evil-global-set-key 'visual ";" 'evil-ex)

  (evil-global-set-key 'motion "e" 'evil-inner-word)
  (evil-global-set-key 'motion "E" 'evil-inner-WORD)
  (evil-global-set-key 'motion "w" 'evil-inner-word)
  (evil-global-set-key 'motion "W" 'evil-inner-WORD)
  (evil-global-set-key 'motion "e" 'evil-a-word)
  (evil-global-set-key 'motion "E" 'evil-a-word)
  (evil-global-set-key 'motion "w" 'evil-a-word)
  (evil-global-set-key 'motion "W" 'evil-a-word)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "e" 'forward-word)
  (evil-global-set-key 'motion "E" 'forward-symbol)
  (evil-global-set-key 'motion "w" 'backward-word)
  (evil-global-set-key 'motion "W" 'backward-symbol)
  (evil-global-set-key 'motion "b" 'evil-jump-item)
  (evil-global-set-key 'motion "C-h" 'evil-window-left)
  (evil-global-set-key 'motion "C-l" 'evil-window-right)
  (evil-global-set-key 'motion "C-k" 'evil-window-up)
  (evil-global-set-key 'motion "C-j" 'evil-window-down)
  )

;;;; Helm

(defun config/post-init-helm ()
  (define-key helm-find-files-map (kbd "C-n") 'next-history-element)
  (define-key helm-find-files-map (kbd "C-p") 'previous-history-element)
  (define-key helm-find-files-map (kbd "M-n") 'helm-next-line)
  (define-key helm-find-files-map (kbd "M-p") 'helm-previous-line)
  (define-key evil-normal-state-map (kbd "M-d") 'sp-splice-sexp)

  (define-key helm-map (kbd "C-n") 'next-history-element)
  (define-key helm-map (kbd "C-p") 'previous-history-element)
  (define-key helm-map (kbd "M-n") 'helm-next-line)
  (define-key helm-map (kbd "M-p") 'helm-previous-line)
  )

;;;; Projectile

(defun config/post-init-projectil ()
  (setq projectil-indexing-method 'native))
