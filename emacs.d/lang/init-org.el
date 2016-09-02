;;; init-org.el --- My customized org mode
;;; Commentary:

;;; Code:
(use-package org
  :ensure t
  :diminish " ⓔ"
  :mode ("\\.org$" . org-mode)
  :defer t
  :commands (org-capture org-clock-out org-occur-in-agenda-files org-agenda-files)
  :init
  (setq org-clock-persist-file (expand-file-name "org-clock-save.el" user-emacs-directory)
        org-id-locations-file (expand-file-name ".org-id-locations" user-emacs-directory)
        org-log-done t
        org-startup-with-inline-images t
        org-src-fontify-natively t
        org-startup-indented t
        )

  ;; (require 'org)
  ;; (require 'ox)
  ;; (require 'ox-beamer)
  ;; (require 'ox-latex)
  ;; (require 'cl)
  (defun my-org-insert-keybinding (key)
    "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
    (interactive "kType key sequence: ")
    (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
      (if (null (equal key "\r"))
          (insert
           (format tag (help-key-description key nil)))
        (insert (format tag ""))
        (forward-char -8))))

  (defmacro org-emphasize-char (fname char)
    "Make function for setting the emphasis in org mode"
    `(defun ,fname () (interactive)
            (org-emphasize ,char)))

  (evil-leader/set-key-for-mode 'org-mode
    "'" 'org-edit-special
    "c" 'org-capture
    "d" 'org-deadline
    "D" 'org-insert-drawer
    "e" 'org-export-dispatch
    "fe" 'org-set-effort
    "P" 'org-set-property
    ":" 'org-set-tags

    "a" 'org-agenda
    "b" 'org-tree-to-indirect-buffer
    "A" 'org-archive-subtree
    "l" 'org-open-at-point
    "T" 'org-show-todo-tree

    "." 'org-time-stamp
    "!" 'org-time-stamp-inactive

    ;; headings
    "hi" 'org-insert-heading-after-current
    "hI" 'org-insert-heading

    ;; More cycling options (timestamps, headlines, items, properties)
    "L" 'org-shiftright
    "H" 'org-shiftleft
    "J" 'org-shiftdown
    "K" 'org-shiftup

    ;; Change between TODO sets
    "C-S-l" 'org-shiftcontrolright
    "C-S-h" 'org-shiftcontrolleft
    "C-S-j" 'org-shiftcontroldown
    "C-S-k" 'org-shiftcontrolup

    ;; Subtree editing
    "Sl" 'org-demote-subtree
    "Sh" 'org-promote-subtree
    "Sj" 'org-move-subtree-down
    "Sk" 'org-move-subtree-up

    ;; tables
    "ta" 'org-table-align
    "tb" 'org-table-blank-field
    "tc" 'org-table-convert
    "tdc" 'org-table-delete-column
    "tdr" 'org-table-kill-row
    "te" 'org-table-eval-formula
    "tE" 'org-table-export
    "th" 'org-table-previous-field
    "tH" 'org-table-move-column-left
    "tic" 'org-table-insert-column
    "tih" 'org-table-insert-hline
    "tiH" 'org-table-hline-and-move
    "tir" 'org-table-insert-row
    "tI" 'org-table-import
    "tj" 'org-table-next-row
    "tJ" 'org-table-move-row-down
    "tK" 'org-table-move-row-up
    "tl" 'org-table-next-field
    "tL" 'org-table-move-column-right
    "tn" 'org-table-create
    "tN" 'org-table-create-with-table.el
    "tr" 'org-table-recalculate
    "ts" 'org-table-sort-lines
    "ttf" 'org-table-toggle-formula-debugger
    "tto" 'org-table-toggle-coordinate-overlays
    "tw" 'org-table-wrap-region

    ;; Multi-purpose keys
    "," 'org-ctrl-c-ctrl-c
    "*" 'org-ctrl-c-star
    "RET" 'org-ctrl-c-ret
    "-" 'org-ctrl-c-minus
    "^" 'org-sort
    "/" 'org-sparse-tree

    "I" 'org-clock-in
    "n" 'org-narrow-to-subtree
    "N" 'widen
    "O" 'org-clock-out
    "q" 'org-clock-cancel
    "R" 'org-refile
    "s" 'org-schedule

    ;; insertion of common elements
    "il" 'org-insert-link
    "if" 'org-footnote-new
    "ik" 'spacemacs/insert-keybinding-org

    ;; images and other link types have no commands in org mode-line
    ;; could be inserted using yasnippet?
    ;; region manipulation
    "xb" (org-emphasize-char org-emphasize-bold ?*)
    "xc" (org-emphasize-char org-emphasize-code ?~)
    "xi" (org-emphasize-char org-emphasize-italic ?/)
    "xr" (org-emphasize-char org-emphasize-clear ? )
    "xs" (org-emphasize-char org-emphasize-strike-through ?+)
    "xu" (org-emphasize-char org-emphasize-underline ?_)
    "xv" (org-emphasize-char org-emphasize-verbose ?=))
  (evil-leader/set-key
    ;; org-agenda
    "ao#" 'org-agenda-list-stuck-projects
    "ao/" 'org-occur-in-agenda-files
    "aoa" 'org-agenda-list
    "aoe" 'org-store-agenda-views
    "aom" 'org-tags-view
    "aoo" 'org-agenda
    "aos" 'org-search-view
    "aot" 'org-todo-list
    ;; other
    "aoO" 'org-clock-out
    "aoc" 'org-capture
    "aol" 'org-store-link
    )
  :config
  (require 'org-indent)
  (setq org-agenda-text-search-extra-files '(agenda-archives)
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
        org-capture-templates '(("a" "My TODO task format."
                                 entry
                                 (file "todo.org")
                                 "* ☛ TODO %?\nSCHEDULED: %t")

                                ("n" "A (work-related) note."
                                 entry
                                 (file+headline "notes.org" "Work")
                                 "* %?\n%u\n\n"
                                 :jump-to-captured t))
        org-default-notes-file "~/Documents/org/todo.org"
        org-directory "~/Documents/org"
        org-enforce-todo-dependencies t
        org-log-done (quote time)
        org-log-redeadline (quote time)
        org-log-reschedule (quote time)
        org-agenda-skip-scheduled-if-done t
        org-insert-heading-respect-content t
        ;; org-ellipsis " …"
        org-ellipsis "⤵"
        )
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
  )

(use-package org-agenda
  :defer t
  :init (setq org-agenda-restore-windows-after-quit t)
  :config
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
  :defer t
  :commands (org-mime-htmlize org-mime-org-buffer-htmlize)
  :init
  (evil-leader/set-key-for-mode 'message-mode
    "M" 'org-mime-htmlize)
  (evil-leader/set-key-for-mode 'org-mode
    "m" 'org-mime-org-buffer-htmlize))

(use-package org-pomodoro
  :defer t
  :init
  (setq org-pomodoro-audio-player "/usr/bin/afplay")
  (evil-leader/set-key-for-mode 'org-mode
    "p" 'org-pomodoro))

(use-package org-present
  :defer t
  :init
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
  :config
  (evil-define-key 'normal 'org-present-mode
    "h" 'org-present-prev
    "l" 'org-present-next
    "q" 'org-present-quit)
  )

(use-package toc-org
  :defer t
  :init
  (setq toc-org-max-depth 10)
  (add-hook 'org-mode-hook 'toc-org-enable))

(use-package evil-org
  :commands evil-org-mode
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  :config
  (evil-leader/set-key-for-mode 'org-mode
    "o" 'make-frame
    "C" 'evil-org-recompute-clocks
    )
  (evil-define-key 'normal evil-org-mode-map
    "O" 'evil-open-above)
  )

(use-package gnuplot
  :defer t
  :init
  (evil-leader/set-key-for-mode 'org-mode
    "tp" 'org-plot/gnuplot))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿")))

(use-package htmlize
  :ensure t
  :defer t)

(provide 'init-org)
;;; init-org.el ends here
