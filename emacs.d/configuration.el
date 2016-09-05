
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lang" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "resources" user-emacs-directory))

(setq user-full-name "Jared L. Rickert"
      user-mail-address "jaredrickert52@gmail.com"
      calendar-latitude 45.6
      calendar-longitude 94.1
      calendar-location-name "St Cloud, MN")

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;;; Pin some packages to specific repositories.
(setq package-pinned-packages '((gtags . "marmalade")
                                (php-extras . "marmalade")))

(require 'my-packages)
(package-initialize)

(defvar evil-packages
'(
  evil
  evil-leader
  evil-surround
  evil-indent-textobject
  evil-nerd-commenter
  evil-matchit
))
(jlr/ensure-packages evil-packages)

(evil-mode 1)
(global-evil-surround-mode)
(global-evil-leader-mode)
(global-evil-matchit-mode 1)
