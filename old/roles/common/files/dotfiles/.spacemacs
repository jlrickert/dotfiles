;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Layer configuration: This function should only modify
configuration layer settings."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(ansible
     windows-scripts
     helm
     (auto-completion :variables
                      auto-completion-enable-help-tooltop t
                      auto-completion-enable-snippets-in-popup 'nil
                      auto-completion-return-key-behavior 'nil
                      auto-completion-tab-key-behavior 'complete
                      ;; company-mode-completion-cancel-keywords
                      ;; (concatenate 'list
                      ;;              company-mode-completion-cancel-keywords "{")
                      ;; auto-completion-complete-with-key-sequence "jk"
                      auto-completion-enable-sort-by-usage t
											(haskell :variables haskell-completion-backend 'intero)
                      )
     better-defaults
     git
     (syntax-checking :variables syntax-checking-use-original-bitmaps t)
     (spell-checking :variables spell-checking-enable-by-default nil)
     (version-control :variables
                      version-control-global-margin t
                      version-control-diff-tool 'git-gutter+)

     ;; Languages
     (clojure
      :variables
      clojure-enable-fancify-symbols t)
     csv
     elm
     emacs-lisp
     (go :variables go-tab-width 4)
     (haskell
      :variables
      haskell-enable-hindent "johan-tibell"
      )
     julia
     html
     (javascript
      :variables
      js2-basic-offset 2
      js-indent-level 2)
     latex
     markdown
     ansible
     python
     react
     rust
     (scala
			:variables
			scala-auto-insert-asterisk-in-comments t
			scala-use-unicode-arrows t
			scala-auto-start-ensime t)
     shell-scripts
     (sql
      :variables
      sql-capitalize-keywords t
      sql-capitalize-keywords-blacklist '("name"))
     (typescript
      :variables
      typescript-indent-level 2)
     yaml
     org
     )

   dotspacemacs-additional-packages '(prettier-js evil-smartparens graphql-mode)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-use-spacelpa nil
   dotspacemacs-verify-spacelpa-archives t
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory 'emacs-version
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-initial-scratch-message nil
   dotspacemacs-themes '(solarized-dark
                         solarized-light
                         spacemacs-dark)
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-generate-layout-names nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers '(:relative nil
                               :disabled-for-modes pdf-view-mode
                               :size-limit-kb 2000)
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-frame-title-format "%I@%S %t"
   dotspacemacs-icon-title-format nil
   dotspacemacs-whitespace-cleanup nil
   dotspacemacs-zone-out-when-idle nil
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-init ()
  "Initialization for user code: This function is called
immediately after `dotspacemacs/init', before layer
configuration. It is mostly for variables that should be set
before packages are loaded. If you are unsure, try setting them
in `dotspacemacs/user-config' first."
  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer-elpa-archives)
  (push '(ensime . "melpa-stable") package-pinned-packages))

(defun dotspacemacs/user-config ()
  "Configuration for user code: This function is called at the
very end of Spacemacs startup, after layer configuration. Put
your configuration code here, except for variables that should be
set before packages are loaded."
  (setq create-lockfiles nil)
  (setq-default tab-width 2
                indent-tabs-mode 2
                standard-indent 2)

  (custom-keybindings)
  (custom-web-config)
  (custom-css-config)
  (custom-lisp-config)
  (custom-graphql-mode)
  (custom-haskell-config)
  )

(defun custom-keybindings ()
  "Custom keybindings."
  (evil-global-set-key 'normal ";" 'evil-ex)
  (evil-global-set-key 'visual ";" 'evil-ex)
  (evil-global-set-key 'motion "w" 'evil-backward-word-begin)
  (evil-global-set-key 'motion "W" 'evil-backward-WORD-begin)
  (evil-global-set-key 'motion "b" 'evil-jump-item)

  (evil-global-set-key 'normal (kbd "M-d") 'sp-splice-sexp)

  (with-eval-after-load 'helm
    (define-key helm-find-files-map (kbd "C-n") 'next-history-element)
    (define-key helm-find-files-map (kbd "C-p") 'previous-history-element)
    (define-key helm-find-files-map (kbd "M-n") 'helm-next-line)
    (define-key helm-find-files-map (kbd "M-p") 'helm-previous-line)

    (define-key helm-map (kbd "C-n") 'next-history-element)
    (define-key helm-map (kbd "C-p") 'previous-history-element)
    (define-key helm-map (kbd "M-n") 'helm-next-line)
    (define-key helm-map (kbd "M-p") 'helm-previous-line))
  )

(defun custom-web-config ()
  "Typescript customization"
  (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-tsx-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

  (setq-default js-indent-level 2)
  (setq-default js2-basic-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  ;; (setq-default typescript-indent-level 2)
  (setq-default typescript-indent-level 2) ;; some other package is setting default to 4
                                           ;; after this.

  (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
    "=" 'prettier-js)

  (spacemacs/set-leader-keys-for-major-mode 'json-mode
    "=" 'prettier-js)

  (spacemacs/set-leader-keys-for-major-mode 'typescript-tsx-mode
    "=" 'prettier-js)

  (spacemacs/add-to-hooks 'prettier-js-mode '(typescript-mode-hook typescript-tsx-mode-hook))
  )

(defun custom-css-config ()
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

  (spacemacs/set-leader-keys-for-major-mode 'css-mode
    "=" 'prettier-js)

  (spacemacs/set-leader-keys-for-major-mode 'scss-mode
    "=" 'prettier-js)

  (spacemacs/add-to-hooks 'prettier-js-mode
                          '(css-mode-hook
                            scss-mode-hook)))

(defun custom-lisp-config ()
  "Lisp customization"
  (spacemacs/add-to-hooks '(lambda ()
                             (smartparens-strict-mode)
                             (evil-smartparens-mode))
                          '(emacs-lisp-mode-hook clojure-mode-hook)))

(defun custom-haskell-config ()
  "Haskell config customization"
  )

(defun custom-graphql-mode ()
  "Adds graphql mode"
  (add-to-list 'auto-mode-alist '("\\.graphql\\'" . graphql-mode))
  )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-org-rifle yasnippet-snippets yapfify yaml-mode ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package unfill toml-mode toc-org tide tagedit symon string-inflection sqlup-mode sql-indent spaceline-all-the-icons solarized-theme smeargle slim-mode scss-mode sass-mode rjsx-mode restart-emacs rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort pug-mode prettier-js powershell popwin pippel pipenv pip-requirements persp-mode pcre2el password-generator paradox overseer orgit org-projectile org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file noflet neotree nameless mwim mvn move-text mmm-mode meghanada maven-test-mode markdown-toc magit-svn magit-gitflow macrostep lsp-julia lorem-ipsum livid-mode live-py-mode link-hint julia-repl julia-mode json-navigator json-mode js2-refactor js-doc jinja2-mode intero insert-shebang indent-guide importmagic impatient-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets groovy-mode groovy-imports graphql-mode gradle-mode google-translate golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy font-lock+ flyspell-correct-helm flycheck-rust flycheck-pos-tip flycheck-haskell flycheck-elm flycheck-bashate flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-smartparens evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu ensime emmet-mode elm-test-runner elm-mode elisp-slime-nav editorconfig dumb-jump dotenv-mode doom-modeline diminish diff-hl define-word dante cython-mode csv-mode counsel-projectile company-web company-tern company-statistics company-shell company-go company-ghci company-ghc company-emacs-eclim company-cabal company-auctex company-ansible company-anaconda column-enforce-mode cmm-mode clojure-snippets clojure-cheatsheet clj-refactor clean-aindent-mode cider-eval-sexp-fu centered-cursor-mode cargo browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk ansible-doc ansible aggressive-indent ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)