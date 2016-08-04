;;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*-

;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lang" user-emacs-directory))

(require 'init-benchmarking)

;; Also add all directories within "lisp"
;; I use this for packages I'm actively working on, mostly.
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")
; (add-to-list 'exec-path ".local/bin"')
(require 'init-utils)
(require 'init-elpa)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Essential settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
(setq-default ring-bell-function 'ignore)
(setq-default scroll-preserve-screen-position 'always)
(setq-default truncate-lines t)  ; disables line wrapping
(setq-default truncate-partial-width-windows nil)
(setq-default global-visual-line-mode t)  ; if it does wrap at least make it look nice
(setq-default hscroll-step 5)  ; make horrizontal scrolling less jumpy
(setq-default scroll-step 1)
(setq-default scroll-conservatively 10000)
(eval-after-load "vc" '(setq vc-handled-backends nil))
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(put 'narrow-to-region 'disabled nil)

(defun my-minibuffer-setup-hook ()
  "Increase GC cons threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Set GC cons threshold to its default value."
  (setq gc-cons-threshold 20000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;;; File type overrides.
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))

;;; My own configurations, which are bundled in my dotfiles.
(require 'init-global-functions)

(require 'diminish)
(require 'init-fonts)
(require 'init-gtags)
(require 'init-evil)
(require 'init-maps)
(require 'init-w3m)
(require 'init-powerline)
(require 'init-flycheck)
(require 'init-tmux)
(require 'i3)
(require 'i3-integration)
(require 'init-org)

;;; Load my programming languages
(require 'init-python)
(require 'init-lisp)
(require 'init-common-lisp)
(require 'init-php)

(i3-one-window-per-frame-mode-on)

;; Elisp Utilities
(use-package s :ensure t)
(use-package dash :ensure t)

;; Org prerequisites
(use-package visual-fill-column :ensure t)

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (defadvice wgrep-change-to-wgrep-mode (after wgrep-set-normal-state)
    (if (fboundp 'evil-normal-state)
        (evil-normal-state)))
  (ad-activate 'wgrep-change-to-wgrep-mode)

  (defadvice wgrep-finish-edit (after wgrep-set-motion-state)
    (if (fboundp 'evil-motion-state)
        (evil-motion-state)))
  (ad-activate 'wgrep-finish-edit)

  (use-package wgrep-ag
    :ensure t
    :commands (wgrep-ag-setup)))

(use-package ag
  :ensure t
  :defer t
  :config
  (add-hook 'ag-mode-hook
            (lambda ()
              (wgrep-ag-setup)
              (define-key ag-mode-map (kbd "n") 'evil-search-next)
              (define-key ag-mode-map (kbd "N") 'evil-search-previous)))
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (exec-path-from-shell-initialize)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq completion-cycling 5)
  (setq company-selection-wrap-around t)

  (define-key evil-insert-state-map (kbd "M-n") 'company-indent-or-complete-common)
  (define-key company-active-map (kbd "M-n") 'company-select-next)
  (define-key company-active-map (kbd "M-p") 'company-select-previous)
  )

(use-package swiper
  :ensure t
  :commands swiper
  :bind ("C-s" . swiper)
  :config
  (setq ivy-height 20))

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                           "~/.emacs.d/remote-snippets"))
  (setq tab-always-indent 'complete)
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-ido-prompt
                               yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (key-chord-define-global (kbd "jk") 'yas-expand)
  (key-chord-define yas-keymap (kbd "jk") 'yas-next-field-or-maybe-expand)
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(use-package which-key
  :ensure t
  :diminish ""
  :config
  (which-key-mode t))

(use-package projectile
  :ensure t
  :defer t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))


(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode)
  )

(use-package evil-smartparens
  :ensure t
  :after evil
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

;;; global completions and quick navigation
(use-package ido
  :ensure t
  :after smex
  :config
  (require 'uniquify)
  (use-package ido-ubiquitous :ensure t)
  (use-package flx-ido :ensure t :config)
  (use-package ido-vertical-mode :ensure t)

  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (ido-ubiquitous-mode 1)
  (ido-vertical-mode 1)
  (setq ido-enable-prefix nil
        ido-use-virtual-buffers t
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-show-dot-for-dired t
        ido-confirm-unique-completion nil
        ido-enable-last-directory-history nil
        ido-use-filename-at-point nil
        ido-save-directory (expand-file-name ".ido.last" user-emacs-directory)
        ido-auto-merge-work-directories-length 0)

  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")

  (setq ido-use-faces t)
  (set-face-attribute 'ido-vertical-first-match-face nil
                      :background "#e5b7c0")
  (set-face-attribute 'ido-vertical-only-match-face nil
                      :background "#e52b50"
                      :foreground "white")
  (set-face-attribute 'ido-vertical-match-face nil
                      :foreground "#b00000")
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)

  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-file-dir-completion-map "~" 'ido-jump-to-home)
              (define-key ido-file-dir-completion-map (kbd "RET") 'exit-minibuffer)
              (define-key ido-file-dir-completion-map [tab] 'ido-complete)
              (define-key ido-file-dir-completion-map (kbd "M-Y") 'ido-select-text)
              (define-key ido-file-dir-completion-map (kbd "M-n") 'ido-next-match)
              (define-key ido-file-dir-completion-map (kbd "M-p") 'ido-prev-match)
              (define-key ido-file-dir-completion-map (kbd "M-y") 'ido-select-text)

              (define-key ido-common-completion-map (kbd "RET") 'exit-minibuffer)
              (define-key ido-common-completion-map (kbd "TAB") 'ido-complete)
              (define-key ido-common-completion-map (kbd "M-n") 'ido-next-match)
              (define-key ido-common-completion-map (kbd "M-p") 'ido-prev-match)
              (define-key ido-common-completion-map (kbd "M-y") 'ido-select-text)))

  (defadvice smex (around space-inserts-hyphen activate compile)
             (let ((ido-cannot-complete-command
                     `(lambda()
                        (interactive)
                        (if (string= " " (this-command-keys))
                          (insert ?-)
                          (funcall ,ido-cannot-complete-command)))))
               ad-do-it))
  )

(use-package smex
  :ensure t
  :config
  (defun smex-update-after-load (unused)
    (when (boundp 'smex-cache)
      (smex-update)))

  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (smex-initialize)

  (add-hook 'after-load-functions 'smex-update-after-load)

  (global-set-key (kbd "M-x") 'smex))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode t))

(use-package fzf
  :ensure t
  :after (projectile init-evil)
  :config
  (require 'projectile)
  (require 'init-evil)
  (defun air-fzf-projectile-project-root ()
    "Start `fzf' from the root of a known Projectile project."
    (interactive)
    (fzf-directory (completing-read "Select a project: "
                                    (if (projectile-project-p)
                                        (cons (abbreviate-file-name (projectile-project-root))
                                              (projectile-relevant-known-projects))
                                      projectile-known-projects))))

  (define-evil-or-global-key (kbd "C-c C-p") 'fzf)
  (define-evil-or-global-key (kbd "C-c C-S-p") 'air-fzf-projectile-project-root))

(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 1.0))

(use-package indent-guide
  :ensure t
  :config
  (indent-guide-global-mode))

(use-package guide-key
  :ensure t
  :config
  (setq guide-key/guide-key-sequence '("C-x" "M-x" "SPC"))
  (setq guide-key/popup-window-position 'bottom)
  (setq guide-key/idle-delay .2)
  (setq guide-key/recursive-key-sequence-flag t)
  (guide-key-mode t))

(use-package dictionary :ensure t)
(use-package emmet-mode :ensure t)
(use-package flycheck :ensure t)
(use-package yaml-mode :ensure t :defer t)
(use-package php-extras :ensure t :defer t)

(use-package tiny-menu
  :ensure t
  :config
  (setq tiny-menu-items
        '(("org-things"   ("Things"
                           ((?t "Tag" org-tags-view)
                            (?i "ID" air-org-goto-custom-id)
                            (?k "Keyword" org-search-view))))
          ("org-links"    ("Links"
                           ((?c "Capture" org-store-link)
                            (?l "Insert" org-insert-link)
                            (?i "Custom ID" air-org-insert-custom-id-link))))
          ("org-files"    ("Files"
                           ((?t "TODO" (lambda () (air-pop-to-org-todo nil)))
                            (?n "Notes" (lambda () (air-pop-to-org-notes nil)))
                            (?v "Vault" (lambda () (air-pop-to-org-vault nil))))))
          ("org-captures" ("Captures"
                           ((?c "TODO" air-org-task-capture)
                            (?n "Note" (lambda () (interactive) (org-capture nil "n"))))))))

  (evil-define-key 'normal global-map (kbd "\\ \\") 'tiny-menu)
  (evil-define-key 'normal global-map (kbd "\\ f") (tiny-menu-run-item "org-files"))
  (evil-define-key 'normal global-map (kbd "\\ t") (tiny-menu-run-item "org-things"))
  (evil-define-key 'normal global-map (kbd "\\ c") (tiny-menu-run-item "org-captures"))
  (evil-define-key 'normal global-map (kbd "\\ l") (tiny-menu-run-item "org-links")))

(use-package mmm-mode
  :ensure t
  :defer t
  :config
  (setq mmm-global-mode 'maybe)
  (mmm-add-classes
   '((markdown-cl
      :submode emacs-lisp-mode
      :face mmm-declaration-submode-face
      :front "^~~~cl[\n\r]+"
      :back "^~~~$")
     (markdown-php
      :submode php-mode
      :face mmm-declaration-submode-face
      :front "^```php[\n\r]+"
      :back "^```$")))
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-cl)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-php))

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "pandoc --from markdown_github-hard_line_breaks --to html")
  (define-key markdown-mode-map (kbd "C-\\")  'markdown-insert-list-item)
  (define-key markdown-mode-map (kbd "C-c 1") 'markdown-insert-header-atx-1)
  (define-key markdown-mode-map (kbd "C-c 2") 'markdown-insert-header-atx-2)
  (define-key markdown-mode-map (kbd "C-c 3") 'markdown-insert-header-atx-3)
  (define-key markdown-mode-map (kbd "C-c 4") 'markdown-insert-header-atx-4)
  (define-key markdown-mode-map (kbd "C-c 5") 'markdown-insert-header-atx-5)
  (define-key markdown-mode-map (kbd "C-c 6") 'markdown-insert-header-atx-6))

(use-package markdown-preview-mode
  :ensure t
  :after (markdown-mode)
  :config
  (add-hook 'markdown-preview-mode-hook
            (lambda ()
              (setq markdown-preview-template
                    (expand-file-name
                      "~/.emacs.d/markdown-preview.html"
                      user-emacs-directory))
              (setq markdown-preview-style
                    "http://aaronbieber.com/assets/styles/github-markdown.css"))))

(use-package groovy-mode
  :ensure t
  :config
  (c-set-offset 'label 4))

(use-package rainbow-mode :ensure t)

(use-package css-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook (lambda ()
                             (rainbow-mode))))

(use-package web-mode
  :ensure t
  :defer t
  :config
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-sql-indent-offset 2))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent"))

;;; Helpers for GNUPG, which I use for encrypting/decrypting secrets.
(require 'epa-file)
(epa-file-enable)
(setq-default epa-file-cache-passphrase-for-symmetric-encryption t)

(defvar show-paren-delay 0
  "Delay (in seconds) before matching paren is highlighted.")

;;; Flycheck mode:
(add-hook 'flycheck-mode-hook
          (lambda ()
            (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
            (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

;;; Lisp interaction mode & Emacs Lisp mode:
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;; The Emacs Shell
(defun company-eshell-history (command &optional arg &rest ignored)
  "Complete from shell history when starting a new line.

Provide COMMAND and ARG in keeping with the Company Mode backend spec.
The IGNORED argument is... Ignored."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell-history))
    (prefix (and (eq major-mode 'eshell-mode)
                 (let ((word (company-grab-word)))
                   (save-excursion
                     (eshell-bol)
                     (and (looking-at-p (s-concat word "$")) word)))))
    (candidates (remove-duplicates
                 (->> (ring-elements eshell-history-ring)
                      (remove-if-not (lambda (item) (s-prefix-p arg item)))
                      (mapcar 's-trim))
                 :test 'string=))
    (sorted t)))

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Kill term buffer when term is ended."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(add-hook 'eshell-mode-hook
          (lambda ()
            (set (make-local-variable 'pcomplete-ignore-case) t)
            (set (make-local-variable 'company-backends)
                 '((company-shell company-eshell-history)))))

;;; Magit mode (which does not open in evil-mode):
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))

;;; Git Commit Mode (a Magit minor mode):
(add-hook 'git-commit-mode-hook 'evil-insert-state)

;;; Emmet mode:
(add-hook 'emmet-mode-hook
          (lambda ()
            (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-l") 'emmet-next-edit-point)
            (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-h") 'emmet-prev-edit-point)))

;;; Web mode:
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-style-padding 2)
            (yas-minor-mode t)
            (emmet-mode)
            (flycheck-add-mode 'html-tidy 'web-mode)
            (flycheck-mode)))

(setq web-mode-ac-sources-alist
      '(("php" . (ac-source-php-extras ac-source-yasnippet ac-source-gtags ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
        ("css" . (ac-source-css-property ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

;;; Emacs Lisp mode:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (yas-minor-mode t)
            (eldoc-mode)
            (highlight-symbol-mode)
            (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;; SH mode:
(add-hook 'sh-mode-hook (lambda ()
                          (setq sh-basic-offset 2)
                          (setq sh-indentation 2)))

;;; Javascript mode:
(add-hook 'javascript-mode-hook (lambda ()
                                  (set-fill-column 120)
                                  (turn-on-auto-fill)
                                  (setq js-indent-level 2)))

;;; Markdown mode:
(add-hook 'markdown-mode-hook (lambda ()
                                (set-fill-column 80)
                                (turn-on-auto-fill)
                                (flyspell-mode)))

;;; HTML mode:
(add-hook 'html-mode-hook (lambda ()
                            (setq sgml-basic-offset 2)
                            (setq indent-tabs-mode nil)))

(defun find-php-functions-in-current-buffer ()
  "Find lines that appear to be PHP functions in the buffer.

This function performs a regexp forward search from the top
\(point-min) of the buffer to the end, looking for lines that
appear to be PHP function declarations.

The return value of this function is a list of cons in which
the car of each cons is the bare function name and the cdr
is the buffer location at which the function was found."
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (save-match-data
        (while (re-search-forward  "^ *\\(public \\|private \\|protected \\|static \\)*?function \\([^{]+\\)" nil t)
          (let* ((fn-name (save-match-data (match-string-no-properties 2)))
                 (fn-location (save-match-data (match-beginning 0))))
            (setq res
                  (append res
                          (list `(,fn-name . ,fn-location)))))))
      res)))

(put 'narrow-to-region 'disabled nil)
(diminish 'undo-tree-mode)

;;; Updates title of eamcs
(cl-dolist (hook '(buffer-list-update-hook
                   change-major-mode-hook
                   find-file-hook))
(add-hook hook 'update-emacs-title))


;;; Cannot kill scratch buffer
(add-hook 'kill-buffer-query-functions
          (lambda ()
            (if (member (buffer-name (current-buffer)) '("*scratch*" "*Messages*"))
              (progn
                (bury-buffer)
                nil)
              t)))
(plist-put minibuffer-prompt-properties 'point 'minibuffer-avoid-prompt)

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 80
                    :weight 'normal
                    :width 'normal)

(global-font-lock-mode)
(add-hook 'prog-mode-hook 'functional-prettification)

(defcustom sanityinc/force-default-font-for-symbols nil
  "When non-nil, force Emacs to use your default font for symbols."
  :type 'boolean)

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark)
  )

;; Allow access to emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; Print out the load time
(add-hook
 'after-init-hook
 (lambda ()
   (message "init completed in %.2fms"
            (sanityinc/time-subtract-millis
             after-init-time
before-init-time))))

(provide 'init)
;;; init.el ends here
