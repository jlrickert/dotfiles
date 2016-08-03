;;; init-global-functions.el --- Global functions mostly used by mappings.
;;; Commentary:
;;; Code:
(defun air--pop-to-file (file &optional split)
  "Visit a FILE, either in the current window or a SPLIT."
  (if split
      (find-file-other-window file)
    (find-file file)))

(require 'htmlfontify)
(defun fontify-and-browse ()
  "Fontify the current buffer into HTML, write it to a temp file, and open it in a browser."
  (interactive)
  (let* ((fontified-buffer (hfy-fontify-buffer))
         (temp-file-name (make-temp-file "ff" nil ".html")))
    (with-current-buffer fontified-buffer
      (write-region (point-min) (point-max) temp-file-name))
    (browse-url (concat "file://" temp-file-name))))

(defun chrome-reload (&optional focus)
  "Use osascript to tell Google Chrome to reload.

If optional argument FOCUS is non-nil, give Chrome the focus as well."
  (interactive "P")
  (let ((cmd (concat "osascript -e 'tell application \"Google Chrome\" "
                     "to (reload (active tab of (window 1)))"
                     (if focus " & activate" "")
                     "'")))
    (shell-command cmd "*Reload Chrome")))

(defun load-only-theme (theme)
  "Disable all themes and then load THEME interactively."
  (interactive
   (list
    (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes)))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme (intern theme) nil nil)
  (when (fboundp 'powerline-reset)
    (powerline-reset)))

(defun air-cycle-theme (&optional reverse)
  "Load the next (or previous if REVERSE is true) available theme."
  (interactive)
  (if (> (length custom-enabled-themes) 1)
      (message "You cannot cycle themes with more than one theme enabled")
    (let* ((current-theme (car custom-enabled-themes))
           (all-themes (if reverse
                           (reverse (custom-available-themes))
                         (custom-available-themes)))
           (first-theme (car all-themes))
           (go (lambda (theme)
                 (message "Loading %s." (symbol-name theme))
                 (disable-theme current-theme)
                 (load-theme theme)))
           theme)
      (if (catch 'done
            (while (setq theme (pop all-themes))
              (if (and (eq theme current-theme)
                       (setq theme (pop all-themes)))
                  (progn
                    (funcall go theme)
                    (throw 'done nil))))
            t)
          (funcall go first-theme)))))

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

(defun cycle-powerline-separators (&optional reverse)
  "Set Powerline separators in turn.  If REVERSE is not nil, go backwards."
  (interactive)
  (let* ((fn (if reverse 'reverse 'identity))
         (separators (funcall fn '("arrow" "arrow-fade" "slant"
                                   "chamfer" "wave" "brace" "roundstub" "zigzag"
                                   "butt" "rounded" "contour" "curve")))
         (found nil))
    (while (not found)
      (progn (setq separators (append (cdr separators) (list (car separators))))
             (when (string= (car separators) powerline-default-separator)
               (progn (setq powerline-default-separator (cadr separators))
                      (setq found t)
                      (redraw-display)))))))

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

(defun selective-display-increase ()
  (interactive)
  (set-selective-display
   (if selective-display (+ selective-display 1) 1)))

(defun selective-display-decrease ()
  (interactive)
  (when selective-display
    (set-selective-display
     (if (< (- selective-display 1) 1)
         nil
       (- selective-display 1)))))

(defun my-align-single-equals ()
  "Align on the first single equal sign."
  (interactive)
  (align-regexp
   (region-beginning) (region-end)
   "\\(\\s-*\\)=" 1 1 nil))

;;; Helpers for narrowing.
(defun narrow-and-set-normal ()
  "Narrow to the region and, if in a visual mode, set normal mode."
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (if (string= evil-state "visual")
      (progn (evil-normal-state nil)
             (evil-goto-first-line))))

(defun narrow-to-region-or-subtree ()
  "Narrow to a region, if set, otherwise to an Org subtree, if present."
  (interactive)
  (if (and mark-active
           (not (= (region-beginning) (region-end))))
      (narrow-and-set-normal)
    (if (derived-mode-p 'org-mode)
        (org-narrow-to-subtree))))

(defun air-narrow-dwim ()
  "Narrow to a thing or widen based on context.

Attempts to follow the Do What I Mean philosophy."
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (narrow-to-region-or-subtree)))

(defun open-current-line-in-codebase-search ()
  "Go to the current file's current line on the codebase site."
  (interactive)
  (let* ((line-num (number-to-string (line-number-at-pos)))
         (file-path (replace-regexp-in-string
                     (expand-file-name (vc-find-root (buffer-file-name) ".git"))
                     ""
                     (buffer-file-name)))
         (args (concat "http://dox.wayfair.com/source/xref/php/" file-path "#" line-num)))
    (call-process "open" nil nil nil args)))

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
  "Find a file with either or ido depending on the context."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (ido-find-file)))

(defun indent-buffer ()
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
