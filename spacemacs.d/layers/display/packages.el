(setq display-packages
      '(
        (prettify-utils :location (recipe :fetcher github
                                          :repo "jlrickert/prettify-utils.el"))
        (pretty-fonts :location local)
        (pretty-code :location local)
        ))

;;; Prettify-utils

(defun display/init-prettify-utils ()
  (use-package prettify-utils))

;;; Pretty-Fonts

(defun display/init-pretty-fonts ()
  (use-package pretty-fonts
    :config
    (pretty-fonts-set-kwds
     '(;; Fira Code Ligatures
       (pretty-fonts-fira-font prog-mode-hook org-mode-hook)))

    (pretty-fonts-set-fontsets
     '(("Symbola"
        ;; 𝕊    ⨂      ∅      ⟻    ⟼     ⊙      𝕋       𝔽
        #x1d54a #x2a02 #x2205 #x27fb #x27fc #x2299 #x1d54b #x1d53d
        ;; 𝔹    𝔇       𝔗
        #x1d539 #x1d507 #x1d517)))
    ))

;;; Pretty-Code

(defun display/init-pretty-code ()
  (use-package pretty-code
    :config
    (setq python-pretty-pairs
          (pretty-code-get-pairs
           '(:lambda "lambda"
                     :def "def"
                     :null "None"
                     :true "True"
                     :false "False"
                     :int "int"
                     :float "float"
                     :str "str"
                     :bool "bool"
                     :not "not"
                     :for "for"
                     :in "in"
                     :not-in "not in"
                     :return "return"
                     :yield "yield"
                     :and "and"
                     :or "or"
                     :tuple "Tuple"
                     :pipe "tz-pipe"
                     )
           ))

    (setq rust-pretty-pairs
          (pretty-code-get-pairs
           '(:def "fn"
                  :true "true"
                  :false "false"
                  :for "for"
                  :in "in"
                  :bool "bool"
                  :return "return"
                  :yield "yield"
                  )
           ))

    (setq emacs-pretty-pairs
          (pretty-code-get-pairs
           '(:lambda "lambda" :def "defun"
                     :true "t" :false "nil")))

    (pretty-code-set-pairs
     `((python-mode-hook ,python-pretty-pairs)
       (emacs-lisp-mode-hook ,emacs-pretty-pairs)
       (elisp-mode-hook ,emacs-pretty-pairs)
       (rust-mode-hook ,rust-pretty-pairs)
       ))

    (add-hook 'prog-mode-hook 'prettify-symbols-mode)
    (setq prettify-symbols-unprettify-at-point t)
    (global-prettify-symbols-mode +1)
    )
  )
