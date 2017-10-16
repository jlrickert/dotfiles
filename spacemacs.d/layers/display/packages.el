(setq display-packages
      '(
        (pretty-fonts :location local)
        (pretty-code :location local)
        ))

;;; Pretty-Fonts

(defun display/init-pretty-fonts ()
  (use-package pretty-fonts
    :config
    (pretty-fonts-set-kwds
     '(;; Fira Code Ligatures
       (pretty-fonts-fira-font prog-mode-hook org-mode-hook)))

    (pretty-fonts-set-fontsets
     '(("fontawesome"
        ;;                         
        #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

       ("all-the-icons"
        ;;    
        #xe907 #xe928)

       ("github-octicons"
        ;;                          
        #xf091 #xf059 #xf076 #xf075 #xe192  #xf016)

       ("material icons"
        ;;        
        #xe871 #xe918 #xe3e7
        ;;
        #xe3d0 #xe3d1 #xe3d2 #xe3d4)

       ("Symbola"
        ;; 𝕊    ⨂      ∅      ⟻    ⟼     ⊙      𝕋       𝔽
        #x1d54a #x2a02 #x2205 #x27fb #x27fc #x2299 #x1d54b #x1d53d
        ;; 𝔹    𝔇       𝔗
        #x1d539 #x1d507 #x1d517)))
    ))

;;; Pretty-Code

(defun display/init-pretty-code ()
  (use-package pretty-code
    :config
    (global-prettify-symbols-mode 1)
    (setq python-pretty-pairs
          (pretty-code-get-pairs
           '(:lambda "lambda" :def "def"
                     :null "None" :true "True" :false "False"
                     :int "int" :float "float" :str "str" :bool "bool"
                     :not "not" :for "for" :in "in" :not-in "not in"
                     :return "return" :yield "yield"
                     :and "and" :or "or"
                     :tuple "Tuple"
                     :pipe "tz-pipe"
                     )
           ))

    (pretty-code-set-pairs `((python-mode-hook ,python-pretty-pairs)))
    )
  )
