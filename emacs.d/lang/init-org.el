;;; init-org.el --- ${2:Package description}
;;; Commentary:

;;; Code:

(use-package evil-org
  :config
  (require 'evil-org)
  (after-load 'evil-org
    (evil-define-key 'normal evil-org-mode-map
      "O" 'evil-open-above
      "o" 'evil-open-below)
    (evil-leader/set-key-for-mode 'org-mode
      "o" 'make-frame))
  )

(use-package org
  :diminish " ⓔ"
  :mode ("\\.org$" . org-mode)
  :commands (org-capture org-clock-out org-occur-in-agenda-files org-agenda-files)
  :config
  (require 'org-indent)
  (require 'ox)
  (require 'ox-beamer)
  (use-package ox-gfm)
  (use-package ox-pandoc)
  (use-package ox-rst)

  (setq org-ellipsis "⤵"
        ;; org-capture-templates '(("a" "My TODO task format."
        ;;                          entry
        ;;                          (file "todo.org")
        ;;                          "* ☛ TODO %?\nSCHEDULED: %t")

        ;;                         ("n" "A (work-related) note."
        ;;                          entry
        ;;                          (file+headline "notes.org" "Work")
        ;;                          "* %?\n%u\n\n"
        ;;                          :jump-to-captured t))
        ;; org-clock-persist-file (expand-file-name "org-clock-save.el" user-emacs-directory)
        ;; org-id-locations-file (expand-file-name ".org-id-locations" user-emacs-directory)
        ;; org-log-done t
        ;; org-startup-with-inline-images t
        ;; org-src-fontify-natively t
        ;; org-startup-indented t
        ;; org-agenda-text-search-extra-files '(agenda-archives)
        ;; org-agenda-files '("~/Documents/org/")
        ;; org-blank-before-new-entry '((heading . t)
        ;;                              (plain-list-item . t))
        ;; org-default-notes-file "~/Documents/org/todo.org"
        ;; org-directory "~/Documents/org"
        ;; org-enforce-todo-dependencies t
        ;; org-log-done (quote time)
        ;; org-log-redeadline (quote time)
        ;; org-log-reschedule (quote time)
        ;; org-agenda-skip-scheduled-if-done t
        ;; org-insert-heading-respect-content t
        ;; org-ellipsis " …"
        )

  ;; (set-face-attribute 'org-upcoming-deadline nil :foreground "gold1")
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'org-mode-hook 'evil-org-mode)
  )

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿")
        org-todo-keywords '((sequence
                             "☛ TODO"
                             "○ IN-PROGRESS"
                             "⚑ WAITING"
                             "|"
                             "✓ DONE"
                             "✗ CANCELED"))))

(font-lock-add-keywords
 'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
              (1 font-lock-comment-face prepend)
              (2 font-lock-function-name-face)
              (3 font-lock-comment-face prepend))))


(provide 'init-org)
;;; init-org.el ends here
