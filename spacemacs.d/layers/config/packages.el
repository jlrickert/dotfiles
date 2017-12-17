;;; Config Layer
;;;; Evil

(setq config-packages
      '(evil
        helm
        projectile
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
  (evil-global-set-key 'motion (kbd "C-h") 'evil-window-left)
  (evil-global-set-key 'motion (kbd "C-l") 'evil-window-right)
  (evil-global-set-key 'motion (kbd "C-k") 'evil-window-up)
  (evil-global-set-key 'motion (kbd "C-j") 'evil-window-down)

  (evil-global-set-key 'normal (kbd "M-d") 'sp-splice-sexp)
  )

;;;; Helm

(defun config/post-init-helm ()
  (with-eval-after-load 'helm
    (define-key helm-find-files-map (kbd "C-n") 'next-history-element)
    (define-key helm-find-files-map (kbd "C-p") 'previous-history-element)
    (define-key helm-find-files-map (kbd "M-n") 'helm-next-line)
    (define-key helm-find-files-map (kbd "M-p") 'helm-previous-line)

    (define-key helm-map (kbd "C-n") 'next-history-element)
    (define-key helm-map (kbd "C-p") 'previous-history-element)
    (define-key helm-map (kbd "M-n") 'helm-next-line)
    (define-key helm-map (kbd "M-p") 'helm-previous-line)))

;;;; Projectile

(defun config/post-init-projectile ()
  (setq projectile-indexing-method 'alien)
  ;; (setq projectile-indexing-method 'native)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  (setq projectile-globally-ignored-directories
        (-distinct (append
                    '("**/node_modules/"
                      "**/__pycache__/"
                      "**/dist/"
                      "**/target/"
                      )
                    projectile-globally-ignored-directories)))
  (setq projectile-globally-ignored-files
        (-distinct (append
                    '("*.pyc"
                      "*.tar.gz"
                      "*.tgz"
                      "*.zip"
                      "*.jar"
                      "*.class"
                      )
                    projectile-globally-ignored-files))))
