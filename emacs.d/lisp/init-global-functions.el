;;; init-global-functions.el --- Global functions mostly used by mappings.
;;; Commentary:
;;; Code:
(require 'htmlfontify)
(defun fontify-and-browse ()
  "Fontify the current buffer into HTML, write it to a temp file, and open it in a browser."
  (interactive)
  (let* ((fontified-buffer (hfy-fontify-buffer))
         (temp-file-name (make-temp-file "ff" nil ".html")))
    (with-current-buffer fontified-buffer
      (write-region (point-min) (point-max) temp-file-name))
    (browse-url (concat "file://" temp-file-name))))

(defun func-region (func start end)
  "Run FUNC over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun hex-region (start end)
  "Hexify (URL encod) the region between START and END in current buffer."
  (interactive "r")
  (func-region #'url-hexify-string start end))

(defun unhex-region (start end)
  "Unhex (URL decode) the region between START and END in current buffer."
  (interactive "r")
  (func-region #'url-unhex-string start end))

(defun occur-last-search ()
  "Run `occur` with the last evil search term."
  (interactive)
  ;; Use the appropriate search term based on regexp setting.
  (let ((term (if evil-regexp-search
                  (car-safe regexp-search-ring)
                (car-safe search-ring))))
    ;; If a search term exists, execute `occur` on it.
    (if (> (length term) 0)
        (occur term)
      (message "No term to search for."))))

(defun show-first-occurrence ()
  "Display the location of the word at point's first occurrence in the buffer."
  (interactive)
  (save-excursion
    (let ((search-word (thing-at-point 'symbol t)))
      (goto-char 1)
      (re-search-forward search-word)
      (message (concat
                "L" (number-to-string (line-number-at-pos)) ": "
                (replace-regexp-in-string
                 "[ \t\n]*\\'"
                 ""
                 (thing-at-point 'line t)
                 ))))))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my-align-single-equals ()
  "Align on the first single equal sign."
  (interactive)
  (align-regexp
   (region-beginning) (region-end)
   "\\(\\s-*\\)=" 1 1 nil))

;;; From http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html
(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia."
  (interactive
   (let ((term (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (word-at-point))))
     (list (read-string (format "Wikipedia (%s): " term) nil nil term))))
  (w3m-browse-url (concat
                   "http://en.m.wikipedia.org/w/index.php?search="
                   search-term)))

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

(defun backward-symbol (&optional arg)
  "Move backward until encountering the beginning of a symbol.
With ARG, do this that many times."
  (interactive "p")
  (forward-symbol (- (or arg 1))))

(defun update-emacs-title ()
  "Update the Emacs title based on the current buffer.
If the current buffer is associated with a filename, that filename will be
used to tile the window.  Otherwise, the window will be titled based upon the
name of the buffer."
  (if (buffer-file-name (current-buffer))
      (setq frame-title-format "Emacs - %f")
      (setq frame-title-format "Emacs - %b")))

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

(provide 'init-global-functions)
;;; init-global-functions.el ends here
