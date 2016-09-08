;;; my-gui.el --- my gui config
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)
(show-paren-mode 1)
(setq-default scroll-preserve-screen-position 'always)
(setq-default hscroll-step 5)  ; make horrizontal scrolling less jumpy
(setq-default scroll-step 1)
(setq-default scroll-conservatively 10000)

(use-package indent-guide
  :config
  (indent-guide-global-mode))

(use-package highlight-symbol
  :defer t
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 1.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frame Title
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun update-emacs-title ()
  "Update the Emacs title based on the current buffer.
If the current buffer is associated with a filename, that filename will be
used to tile the window.  Otherwise, the window will be titled based upon the
name of the buffer."
  (if (buffer-file-name (current-buffer))
      (setq frame-title-format "Emacs - %f")
    (setq frame-title-format "Emacs - %b")))

(cl-dolist (hook '(buffer-list-update-hook
                   change-major-mode-hook
                   find-file-hook))
  (add-hook hook 'update-emacs-title))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package solarized-theme
  :ensure t
  :config
  (setq custom-safe-themes t)
  (load-theme 'solarized-dark))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 80
                    :weight 'normal
                    :width 'normal)

(defun functional-prettification ()
  (setq prettify-symbols-alist
        (list '("function" . ?ƒ)
              '("func" . ?ƒ)
              '("fn" . ?λ)
              '("lambda" . ?λ)
              '("!=" . ?≠)
              '(">=" . ?≥)
              '("<=" . ?≤)
              '("&&" . ?∧)
              '("||" . ?∨)
              '("PI" . ?π)
              '("Math.PI" . ?π)
              '("math.PI" . ?π)
              ))
  (prettify-symbols-mode t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode Line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface my-pl-segment1-active
  '((t (:foreground "#000000" :background "#E1B61A")))
  "Powerline first segment active face.")
(defface my-pl-segment1-inactive
  '((t (:foreground "#CEBFF3" :background "#3A2E58")))
  "Powerline first segment inactive face.")
(defface my-pl-segment2-active
  '((t (:foreground "#F5E39F" :background "#8A7119")))
  "Powerline second segment active face.")
(defface my-pl-segment2-inactive
  '((t (:foreground "#CEBFF3" :background "#3A2E58")))
  "Powerline second segment inactive face.")
(defface my-pl-segment3-active
  '((t (:foreground "#CEBFF3" :background "#3A2E58")))
  "Powerline third segment active face.")
(defface my-pl-segment3-inactive
  '((t (:foreground "#CEBFF3" :background "#3A2E58")))
  "Powerline third segment inactive face.")

(defun jlr/powerline-default-theme ()
  "Set up my custom Powerline with Evil indicators."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (seg1 (if active 'my-pl-segment1-active 'my-pl-segment1-inactive))
                          (seg2 (if active 'my-pl-segment2-active 'my-pl-segment2-inactive))
                          (seg3 (if active 'my-pl-segment3-active 'my-pl-segment3-inactive))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode
                                           (powerline-raw (powerline-evil-tag) evil-face)
                                         ))
                                     (if evil-mode
                                         (funcall separator-left (powerline-evil-face) seg1))
                                     (powerline-buffer-id seg1 'l)
                                     (powerline-raw "[%*]" seg1 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format seg1 'l))
                                     (powerline-raw " " seg1)
                                     (funcall separator-left seg1 seg2)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object seg2 'l))
                                     (powerline-major-mode seg2 'l)
                                     (powerline-process seg2)
                                     (powerline-minor-modes seg2 'l)
                                     (powerline-narrow seg2 'l)
                                     (powerline-raw " " seg2)
                                     (funcall separator-left seg2 seg3)
                                     (powerline-vc seg3 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) seg3 'l))))
                          (rhs (list (powerline-raw global-mode-string seg3 'r)
                                     (funcall separator-right seg3 seg2)
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) seg2 'l))
                                     (powerline-raw "%4l" seg2 'l)
                                     (powerline-raw ":" seg2 'l)
                                     (powerline-raw "%3c" seg2 'r)
                                     (funcall separator-right seg2 seg1)
                                     (powerline-raw " " seg1)
                                     (powerline-raw "%6p" seg1 'r)
                                     (when powerline-display-hud
                                       (powerline-hud seg1 seg3)))))
                     (concat (powerline-render lhs)
                             (powerline-fill seg3 (powerline-width rhs))
                             (powerline-render rhs)))))))

(use-package powerline
  :config
  (use-package powerline-evil)

  (setq powerline-default-separator (if (display-graphic-p) 'arrow nil))
  (jlr/powerline-default-theme)

  (diminish 'undo-tree-mode)
  )

(provide 'my-gui)
;;; my-gui.el ends here
