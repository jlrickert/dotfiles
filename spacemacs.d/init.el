;;; Dotspacemacs

;; -- Jared Rickert's Spacemacs Configuration --
;; -- Contact: jaredrickert52@gmail.com --
;; -- MIT License --
;; -- Emacs 25.3.1 --
;; See README for details --
;;
;; All configuration is housed in personal layers

(setq is-linuxp (eq system-type 'gnu/linux))
(defun os-path (x) (if is-linuxp x (expand-file-name x "c:")))

(defun dotspacemacs/init ()
  "Spacemacs core settings."
  (dotspacemacs/init/coding)
  (dotspacemacs/init/display)
  (dotspacemacs/init/evil)
  (dotspacemacs/init/keys)
  (dotspacemacs/init/layout)
  (dotspacemacs/init/misc)
  (dotspacemacs/init/packages)
  (dotspacemacs/init/startup))

(defun dotspacemacs/layers ()
  "Spacemacs layers declarations and package configuration"
  (dotspacemacs/layers/config)
  (dotspacemacs/layers/packages))

(defun dotspacemacs/user-init()
  "Package independent settings to run before `dotspacemacs/user-config'."
  (setq custom-file "./.custom-settings.el"))

(defun dotspacemacs/user-config ()
  "Configuration that cannot be delegate to layers")

;;; Spacemacs/Layers
;;;; Local

(defvar dotspacemacs/layers/local
  '((macros :location local)     ; All local layers inherit these macros
    (config :location local)     ; Org, Avy, Evil, Misc... config
    (display :location local)    ; Pretty code/outlines... pkgs
    ;; (langs :location local)      ; Language config
    ;; (personal :location local)   ; Personal pkgs
    )
  "Local layers housed in '~/.spacemacs.d/layers'.")

;;;; Core

(defvar dotspacemacs/layers/core
  '((auto-completion :variables
                     auto-completion-enable-help-tooltip t
                     auto-completion-enable-snippets-in-popup t
                     auto-completion-return-key-behavior 'nil
                     auto-completion-tab-key-behavior 'complete
                     ;; company-mode-completion-cancel-keywords
                     ;; (concatenate 'list
                     ;;              company-mode-completion-cancel-keywords "{")
                     auto-completion-complete-with-key-sequence "jk"
                     auto-completion-enable-sort-by-usage t
                     )
    better-defaults
    git
    helm
    nlinum
    org
    spell-checking
    syntax-checking
    (version-control :variables
                     version-control-global-margin t
                     version-controll-diff-tool 'git-gutter+)
    )
  "Layers I consider core to Spacemacs")

;;;; Langs

(defvar dotspacemacs/layers/langs
  '(
    asm
    c-c++
    (clojure :variables clojure-enable-fancify-symbols t)
    (elm :variables elm-reacot-port "3000" elm-reactor-address "0.0.0.0" elm-sort-imports-on-save t)
    emacs-lisp
    (haskell :variables haskell-completion-backend 'intero)
    html
    javascript
    latex
    markdown
    (python :variables python-sort-imports-on-save t python-test-runner 'pytest)
    rust
    shaders
    shell-scripts
    sql
    (typescript :variables typescript-fmt-on-save nil)
    yaml
    )
  "Programming and markup language layers")

(defvar dotspacemacs/layers/extra
  '(graphviz
    csv)
  "Miscellaneous layers")

;;;; Layers/config

(defun dotspacemacs/layers/config ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path `(,(os-path "~/.spacemacs.d/layers/"))
   dotspacemacs-configuration-layers (append dotspacemacs/layers/core
                                             dotspacemacs/layers/langs
                                             dotspacemacs/layers/extra
                                             dotspacemacs/layers/local)
   ))

;;;; Layers/packages

(defun dotspacemacs/layers/packages ()
  (setq-default
   dotspacemacs-additional-packages '(solidity-mode)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-but-keep-unused
   ))

;;; Spacemacs/Init
;;;; Coding

(defun dotspacemacs/init/coding ()
  (setq-default
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-smooth-scrolling t
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-line-numbers t
   dotspacemacs-whitespace-cleanup 'trailing
   ))

;;;; Display

(defun dotspacemacs/init/display ()
  (setq-default
   dotspacemacs-themes '(solarized-dark
                         solarized-light
                         )

   dotspacemacs-default-font '("Source Code Pro"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-fullscreen-at-startup (if is-linuxp nil t)
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-zone-out-when-idle nil
   dotspacemacs-frame-title-format "%I@%S"
   dotspacemacs-icon-title-format nil
   ))

;;;; Evil

(defun dotspacemacs/init/evil ()
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   ;; dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   ))

;;;; Keys

(defun dotspacemacs/init/keys ()
  (setq-default
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-distinguish-gui-tab nil
   ))

;;;; Layouts

(defun dotspacemacs/init/layout ()
  (setq-default
   ;; dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-generate-layout-names t
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ))

;;;; Misc

(defun dotspacemacs/init/misc ()
  (setq-default
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-persistent-server nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   ))

;;;; Packages

(defun dotspacemacs/init/packages ()
  (setq-default
   dotspacemacs-default-package-repository nil
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   ))

;;;; Startup

(defun dotspacemacs/init/startup ()
  (setq-default
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '()
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-loading-progress-bar t
   ))

;;; Spacemacs/User-Config
