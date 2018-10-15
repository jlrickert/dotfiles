;;; packages.el --- i3wm layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jared Rickert <jaredrickert52@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `i3wm-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `i3wm/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `i3wm/pre-init-PACKAGE' and/or
;;   `i3wm/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst i3wm-packages
  '(
    (i3 :location local)
    (i3-integration :location local)
    )
  "The list of Lisp packages required by the i3wm layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun i3wm/init-i3 ()
  (use-package i3))

(defun i3wm/init-i3-integration ()
  (use-package i3-integration
    :config
    (i3-advise-visible-frame-list-on)
    (when i3wm-one-window-per-frame
      (i3-one-window-per-frame-mode-on))
    (when i3wm-one-project-per-workspace
      (i3wm/turn-on-one-project-per-workspace))))

;;; packages.el ends here
