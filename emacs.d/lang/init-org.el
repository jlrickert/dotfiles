;;; init-org.el --- My customized org mode
;;; Commentary:

;;; Code:
(use-package org
  :ensure t
  :diminish " ⓔ"
  :mode ("\\.org$" . org-mode)
  :commands (org-capture org-clock-out org-occur-in-agenda-files org-agenda-files)
  :config
  (require 'org-indent)
  (use-package evil-org
    :ensure t
    :config
    (require 'evil-org))
  ;; (require 'org)
  ;; (require 'ox)
  ;; (require 'ox-beamer)
  ;; (require 'ox-latex)
  ;; (require 'cl)

  (setq org-capture-templates '(("a" "My TODO task format."
                                 entry
                                 (file "todo.org")
                                 "* ☛ TODO %?\nSCHEDULED: %t")

                                ("n" "A (work-related) note."
                                 entry
                                 (file+headline "notes.org" "Work")
                                 "* %?\n%u\n\n"
                                 :jump-to-captured t))
        org-clock-persist-file (expand-file-name "org-clock-save.el" user-emacs-directory)
        org-id-locations-file (expand-file-name ".org-id-locations" user-emacs-directory)
        org-log-done t
        org-startup-with-inline-images t
        org-src-fontify-natively t
        org-startup-indented t
        org-agenda-text-search-extra-files '(agenda-archives)
        org-agenda-files '("~/Documents/org/")
        org-todo-keywords '((sequence
                             "☛ TODO"
                             "○ IN-PROGRESS"
                             "⚑ WAITING"
                             "|"
                             "✓ DONE"
                             "✗ CANCELED"))
        org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . t))
        org-default-notes-file "~/Documents/org/todo.org"
        org-directory "~/Documents/org"
        org-enforce-todo-dependencies t
        org-log-done (quote time)
        org-log-redeadline (quote time)
        org-log-reschedule (quote time)
        org-agenda-skip-scheduled-if-done t
        org-insert-heading-respect-content t
        ;; org-ellipsis " …"
        org-ellipsis "⤵")

  (font-lock-add-keywords
   'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                (1 font-lock-comment-face prepend)
                (2 font-lock-function-name-face)
                (3 font-lock-comment-face prepend))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-h")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-l")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-k")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-j")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-H")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-L")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-K")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-J")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1))))
  ;; (set-face-attribute 'org-upcoming-deadline nil :foreground "gold1")
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'org-mode-hook 'evil-org-mode)
  )

(use-package org-agenda
  :config
  (setq org-agenda-restore-windows-after-quit t)
  (evil-define-key 'normal
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    (kbd "M-j") 'org-agenda-next-item
    (kbd "M-k") 'org-agenda-previous-item
    (kbd "M-h") 'org-agenda-earlier
    (kbd "M-l") 'org-agenda-later
    (kbd "gd") 'org-agenda-toggle-time-grid
    (kbd "gr") 'org-agenda-redo)
  )

(use-package org-mime
  :ensure t
  :commands (org-mime-htmlize org-mime-org-buffer-htmlize)
  :config
  (evil-leader/set-key-for-mode 'message-mode
    "M" 'org-mime-htmlize)
  (evil-leader/set-key-for-mode 'org-mode
    "m" 'org-mime-org-buffer-htmlize))

(use-package org-pomodoro
  :ensure t
  :config
  (setq org-pomodoro-audio-player "/usr/bin/afplay")
  (evil-leader/set-key-for-mode 'org-mode
    "p" 'org-pomodoro))

(use-package org-present
  :ensure t
  :config
  (defun my-org-present-start ()
    "Initiate `org-present' mode"
    (org-present-big)
    (org-display-inline-images)
    (org-present-hide-cursor)
    (org-present-read-only)
    (evil-evilified-state))
  (defun my-org-present-end ()
    "Terminate `org-present' mode"
    (org-present-small)
    (org-remove-inline-images)
    (org-present-show-cursor)
    (org-present-read-write)
    (evil-normal-state))
  (add-hook 'org-present-mode-hook 'my-org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my-org-present-end)
  (evil-define-key 'normal 'org-present-mode
    "h" 'org-present-prev
    "l" 'org-present-next
    "q" 'org-present-quit)
  )

(use-package toc-org
  :ensure t
  :config
  (setq toc-org-max-depth 10)
  (add-hook 'org-mode-hook 'toc-org-enable))

(use-package gnuplot
  :ensure t
  :defer t
  :config
  (evil-leader/set-key-for-mode 'org-mode
    "tp" 'org-plot/gnuplot))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿")))

(use-package htmlize
  :ensure t
  )

(provide 'init-org)
;;; init-org.el ends here
