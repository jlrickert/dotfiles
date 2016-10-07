;;; my-defun.el --- all my custom global defuns
;;; Commentary:

;;; Code:
(message "Loading my-defun")

(use-package cl)

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defun backward-symbol (&optional arg)
  "Move backward until encountering the beginning of a symbol.
With ARG, do this that many times."
  (interactive "p")
  (forward-symbol (- (or arg 1))))

(defun is-line-empty ()
  "Return t if current line is empty, else nil."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun electric-lisp-comment ()
  "Autocomment things for Lisp."
  (interactive)
  (if (is-line-empty)
      (insert ";; ")
    (if (bound-and-true-p smartparens-mode)
        (sp-comment)
      (insert ";"))))

(defun quick-find-file ()
  "Find a file with either projectile or ido depending on the context."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (ido-find-file)))

(defun jlr/indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun jlr/foldr (fn x seq)
  (if seq
      (let* ((head (car seq))
             (tail (cdr seq)))
        (funcall fn x (jlr/foldr fn head tail)))
    x))

(defun jlr/foldl (fn x seq)
  (if seq
      (let* ((head (car seq))
             (tail (cdr seq)))
        (funcall fn (jlr/foldl fn head tail) x))
    x))

(defun jlr/reduce (fn seq &optional identity)
  "Use FN to combine SEQ an option IDENTITY."
  (if identity
      (jlr/foldr fn identity seq)
    (jlr/foldr fn (car seq) (cdr seq)))
  )

(defun jlr/path-join (&rest paths)
  "Join PATHS together, inserting '/' as needed.
An empty last part will result in a path that ends with a
separator."
  (let* ((sep (if (string= system-type "windows-nt") "\\" "/")))
    (jlr/reduce (lambda (a b) (concat a sep b)) paths)))

(defun jlr/ag (string)
  "Search using ag n the current project for a given literal
search STRING, with STRING defaulting to the symbol under point.

If called with a prefix, prompts for flags to pass to ag. "
  (interactive (list (ag/read-from-minibuffer "Search string")))
  (if (projectile-project-p)
      (ag/search string (cdr (project-current)))
    (ag)
    ))

(provide 'my-defun)
;;; my-defun.el ends here
