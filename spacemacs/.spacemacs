;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(tree-sitter
     sql
     dart
     (plantuml :variables
							 plantuml-jar-path "~/bin/plantuml.jar"
							 org-plantuml-jar-path "~/bin/plantuml.jar")
		 (lsp :variables
          ;; Formatting and indentation - use Cider instead
          lsp-enable-on-type-formatting nil
          ;; Set to nil to use CIDER features instead of LSP UI
          lsp-enable-indentation nil
          lsp-enable-snippet t  ;; to test again

          ;; symbol highlighting - `lsp-toggle-symbol-highlight` toggles highlighting
          ;; subtle highlighting for doom-gruvbox-light theme defined in dotspacemacs/user-config
          lsp-enable-symbol-highlighting t

          ;; Show lint error indicator in the mode line
          lsp-modeline-diagnostics-enable t
          ;; lsp-modeline-diagnostics-scope :workspace

          ;; popup documentation boxes
          ;; lsp-ui-doc-enable nil          ;; disable all doc popups
          lsp-ui-doc-show-with-cursor nil   ;; doc popup for cursor
          ;; lsp-ui-doc-show-with-mouse t   ;; doc popup for mouse
          ;; lsp-ui-doc-delay 2                ;; delay in seconds for popup to display
          lsp-ui-doc-include-signature t    ;; include function signature
          ;; lsp-ui-doc-position 'at-point  ;; top bottom at-point
          lsp-ui-doc-alignment 'window      ;; frame window

          ;; code actions and diagnostics text as right-hand side of buffer
          lsp-ui-sideline-enable nil
          lsp-ui-sideline-show-code-actions nil
          ;; lsp-ui-sideline-delay 500

          ;; lsp-ui-sideline-show-diagnostics nil

          ;; reference count for functions (assume their maybe other lenses in future)
          lsp-lens-enable t

          ;; Efficient use of space in treemacs-lsp display
          treemacs-space-between-root-nodes nil

          ;; Optimization for large files
          lsp-file-watch-threshold 10000
          lsp-log-io t)
		 systemd
     react
     asciidoc
     nginx
     swift
     sql
		 terraform
     typescript
     debug
     rust
     go
     python
     windows-scripts
     octave
     ruby
     csv
     protobuf
     javascript
     yaml
     html
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm

     auto-completion
     ;; better-defaults
     emacs-lisp
     (clojure :variables
              clojure-enable-clj-refactor t
              clojure-enable-linters 'clj-kondo)
     git
     markdown
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(logview
                                      git-link
                                      ob-http
                                      multi-vterm
                                      wgrep-ag
                                      password-store
                                      pass
                                      helm-pass
                                      nix-mode
                                      indium
                                      graphql-mode
                                      with-editor)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(exec-path-from-shell)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style '(emacs :variables
                                       hybrid-style-enable-hjkl-bindings t)
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 27
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers '(:absolute t
                                         :disabled-for-modes dired-mode
                                         doc-view-mode
                                         pdf-view-mode
                                         :size-limit-kb 1000)
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun lambdawerk-cleanup-buffer ()
  "clean up buffer"
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defun lambdawerk-indent ()
  (define-clojure-indent
    (defui '(1 nil nil (1)))
    (with 'defun)))

(defun my/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (global-undo-tree-mode nil)
  (setq evil-undo-system 'undo-redo)
  (evil-set-undo-system 'undo-redo)
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "dot" "gnuplot" "ledger" "R" "sass" "screen" "sql" "awk"
            "ditaa" "haskell" "latex" "lisp" "matlab" "org" "perl" "ruby"
            "sqlite" "rust" "scala" "golang")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; (setq treesit-language-source-alist
  ;;  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
  ;;    (cmake "https://github.com/uyha/tree-sitter-cmake")
  ;;    (css "https://github.com/tree-sitter/tree-sitter-css")
  ;;    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
  ;;    (go "https://github.com/tree-sitter/tree-sitter-go")
  ;;    (html "https://github.com/tree-sitter/tree-sitter-html")
  ;;    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
  ;;    (json "https://github.com/tree-sitter/tree-sitter-json")
  ;;    (make "https://github.com/alemuller/tree-sitter-make")
  ;;    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
  ;;    (python "https://github.com/tree-sitter/tree-sitter-python")
  ;;    (toml "https://github.com/tree-sitter/tree-sitter-toml")
  ;;    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
  ;;    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
  ;;    (yaml "https://github.com/ikatyang/tree-sitter-yaml")
  ;;    (clojure "https://github.com/sogaiu/tree-sitter-clojure")))

  ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  (defun sort-words (reverse beg end)
    "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.
  
    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.
  
    See `sort-regexp-fields'."
    (interactive "*P\nr")
    (sort-regexp-fields reverse "\\w+" "\\&" beg end))

  (setq auth-sources '(password-store))
  (auth-source-pass-enable)
  (require 'helm-bookmark)

  (setq lsp-auto-guess-root t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-modeline-diagnostics-scope :workspace)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (put-clojure-indent 'letsubs 1)
              (put-clojure-indent 'create-class 0)
              (put-clojure-indent 'register-handler-db -1)
              (put-clojure-indent 'create-class -1)
              (put-clojure-indent 'register-handler-fx -1)
              (put-clojure-indent 'register-handler -1)
              (put-clojure-indent 'reg-fx -1)
              (put-clojure-indent 'reg-cofx -1)
              (put-clojure-indent 'reg-sub -1)
              (put-clojure-indent 'allowed-keys -1)
              (put-clojure-indent 'list-item 0)
              (put-clojure-indent 'setTimeout 0)
              (put-clojure-indent 'set-timeout 0)
              (put-clojure-indent 'run-test-sync 0)
              (put-clojure-indent 'keep 0)
              (put-clojure-indent 'status/move-to-internal-storage 0)
              (put-clojure-indent 'status/should-move-to-internal-storage? 0)
              (put-clojure-indent 'utils/show-popup 0)
              (put-clojure-indent '.watchPosition 0)
              (put-clojure-indent '.clearWatch 0)
              (put-clojure-indent 'ra/start-figwheel! 0)
              (put-clojure-indent '.getCurrentPosition 0)
              (put-clojure-indent 'crypt/gen-random-bytes 0)
              (put-clojure-indent 'assoc 0)
              (put-clojure-indent 'figwheel/watch-and-reload 0)
              (put-clojure-indent 'leval/eval-in-project 0)
              (lambdawerk-indent)
              (add-hook 'before-save-hook 'lambdawerk-cleanup-buffer t t)))
  (add-hook 'cider-mode-hook
            (lambda ()
              (define-key cider-mode-map (kbd "M-c M-c")  'cider-eval-print-last-sexp)
              (define-key cider-mode-map (kbd "C-c M-r") 'cider-ns-refresh)))
  ;; (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (define-key global-map (kbd "C-<return>") 'multi-vterm)
  (define-key global-map (kbd "M-f") 'helm-projectile-ag)

  (setq nrepl-hide-special-buffers t)
  (setq cider-save-file-on-load t)

  (defun my-helm-projectile-or-find-files ()
    "Toggle between helm-projectile and spacemacs/helm-find-files."
    (interactive)
    (if (bound-and-true-p helm-alive-p)
        (progn (helm-keyboard-quit)
               (spacemacs/helm-find-files))
      (helm-projectile-find-file)))

  (global-set-key (kbd "C-x C-p") 'helm-projectile)

  (add-hook 'lsp-mode-hook
            (lambda ()
              (define-key lsp-mode-map (kbd "M-.")  'lsp-find-definition)
              (define-key lsp-mode-map (kbd "C-M-.")  'cider-find-var)))
  
  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "M-s") 'sp-splice-sexp)

  (global-set-key (kbd "<f1>") (lambda() (interactive)
                                 (find-file "/home/yenda/brian/src/dev/development.clj")
                                 (cider-jack-in-clj&cljs nil)
                                 ;;(find-file (concat "~/org/daily/" (format-time-string "%Y%m%d") ".org"))
                                 ))
  (global-set-key (kbd "<f2>") (lambda() (interactive) (find-file "~/org/todo.org")))
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C--") 'text-scale-decrease)

  ;; ORGMODE

  (setq org-src-fontify-natively t)

  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c s e") 'org-edit-src-code)
    (define-key org-mode-map (kbd "C-c s i") 'my/org-insert-src-block)
    (define-key org-mode-map (kbd "C-c C-x i") 'org-clock-in))


  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (plantuml . t)
     (emacs-lisp . t)
     (shell . t)
     (http . t)))

  (defun org-babel-execute:passthrough (body _params) body)
  (defalias 'org-babel-execute:graphql 'org-babel-execute:passthrough)
  (defalias 'org-babel-execute:json 'org-babel-execute:passthrough)

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (member lang '("plantuml"))))

  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  (defun push-to-mac ()
    (interactive)
    (magit-commit-extend)
    (let ((current-branch (magit-get-current-branch)))
      (magit-git-push current-branch
                      (concat "mac/" current-branch)
                      (list "--force"))))

  ;; fix the C-z hanging and replace it by undo
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo)

  (global-set-key (kbd "C-x C-g") 'magit-status)

  (define-key smartparens-mode-map (kbd "C-d") 'delete-forward-char)
  (global-set-key (kbd "C-x p") 'push-to-mac)

  (add-hook 'makefile-mode-hook
            (lambda ()
              (setq indent-tabs-mode t)
              (setq-default indent-tabs-mode t)
              (setq tab-width 2)))

  (setq package-native-compile t)

  )
          

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
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
 '(cider-eval-result-duration 'change)
 '(cider-inject-dependencies-at-jack-in t)
 '(cider-interactive-eval-output-destination 'repl-buffer)
 '(cider-print-fn 'puget)
 '(cider-redirect-server-output-to-repl t)
 '(cider-show-error-buffer 'only-in-repl)
 '(epg-pinentry-mode 'loopback)
 '(evil-want-Y-yank-to-eol nil)
 '(helm-ag-base-command "ag --nocolor --nogroup")
 '(helm-ag-command-option "")
 '(helm-ff-lynx-style-map t)
 '(magit-diff-refine-ignore-whitespace t)
 '(nrepl-log-messages t)
 '(package-selected-packages
   '(logview yapfify yaml-mode xterm-color ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tide typescript-mode flycheck tagedit spaceline powerline smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rake rainbow-delimiters racer pos-tip pyvenv pytest pyenv-mode py-isort pug-mode powershell popwin pip-requirements persp-mode pcre2el paradox orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-bullets open-junk-file nix-mode neotree multi-term move-text mmm-mode minitest markdown-toc magit-gitflow magit-popup macrostep lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc indent-guide hy-mode dash-functional hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile projectile helm-mode-manager helm-make helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio go-guru go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit git-commit transient evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dumb-jump diminish define-word cython-mode csv-mode company-web web-completion-data company-statistics company-go go-mode company-anaconda company column-enforce-mode coffee-mode clojure-snippets clj-refactor hydra inflections multiple-cursors paredit lv clean-aindent-mode cider-eval-sexp-fu eval-sexp-fu cider sesman spinner queue pkg-info parseedn clojure-mode parseclj a epl chruby cargo markdown-mode rust-mode bundler inf-ruby bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup))
 '(safe-local-variable-values
   '((cider-clojure-cli . "-A:dev:rad-dev -J-Dtrace")
     (eval progn
           (global-display-fill-column-indicator-mode t)
           (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))
     (cider-default-cljs-repl . custom)
     (cider-clojure-cli-aliases . ":env/dev:env/test:enable-server")
     (smerge-lower-re . "^======= \\(.*\\)\12")
     (eval progn
           (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))
     (cider-connect-clj-default-port . 7888)
     (cider-connect-clj-default-host . "localhost")
     (eval setenv "AWS_ACCESS_KEY_ID" "EXO03628b63df1311978b45bc3a")
     (eval setenv "AWS_SECRET_ACCESS_KEY" "OR2am2aDESikb1QSmuOen3a3m039X7J8WDXt5p9xeus")
     (eval setenv "AWS_REGION" "ch-dk-2")
     (cider-clojure-cli-parameters . "-A:dev:rad-dev:reveal -J-Dtrace")
     (cider-ns-refresh-after-fn . "development/go")
     (cider-ns-refresh-before-fn . "development/stop")
     (cider-ns-refresh-after-fn . "development/restart")
     (cider-ns-refresh-after-fn . "dev/restart")
     (cider-ns-refresh-after-fn . "dev/setup-and-start")
     (cider-clojure-cli-parameters . "-A:dev:test:cider-clj")
     (eval setenv "CLASH_STAGE" "local")
     (eval setenv "AWS_LOCAL_SQS" "true")
     (eval setenv "AWS_REGION" "eu-west-2")
     (cider-ns-refresh-after-fn . "dev/start-and-migrate")
     (cljr-injected-middleware-version . "3.0.0-alpha11")
     (cider-required-middleware-version . "0.27.2")
     (eval put-clojure-indent 'when-let-ok 1)
     (eval put-clojure-indent 't/with-system 1)
     (eval put-clojure-indent 'with-system 1)
     (eval put-clojure-indent 'measure/time 2)
     (eval put-clojure-indent 'when-ok 1)
     (cider-ns-refresh-after-fn . "clash.api.dev/start-and-migrate")
     (cider-ns-refresh-after-fn . "clash.api.dev/start-and-instrument")
     (cider-ns-refresh-after-fn . "clash.api.dev/start")
     (cider-ns-refresh-show-log-buffer . t)
     (cider-clojure-cli-global-aliases . "-A:dev:test")
     (cider-path-translations
      ("/root" . "/home/yenda")
      ("/usr/src/app" . "/home/yenda/clash-transcoder"))
     (cider-known-endpoints
      ("localhost" "9676"))
     (cider-print-options
      (("print-length" nil)))
     (cider-print-fn . puget)
     (eval define-clojure-indent
           (codepoint-case 'defun))
     (cider-path-translations
      ("/root" . "/home/yenda")
      ("/usr/src/app" . "/home/yenda/clash-backend"))
     (cider-preferred-build-tool . clojure-cli)
     (cider-clojure-cli-parameters . "-A:dev:test")
     (cider-ns-refresh-before-fn . "com.stuartsierra.component.repl/stop")
     (cider-ns-refresh-after-fn . "com.stuartsierra.component.repl/start")
     (cider-known-endpoints
      ("localhost" "9656"))
     (eval define-clojure-indent
           (animation/interpolate 0)
           (animation/start 0)
           (animation/parallel 0))
     (eval define-clojure-indent
           (animation/start 0)
           (animation/parallel 0))
     (cider-shadow-cljs-default-options . "app")
     (cider-default-cljs-repl . shadow)
     (cider-default-cljs-repl . shadow-select)
     (cider-default-cljs-repl . figwheel-repl)
     (typescript-backend . tide)
     (typescript-backend . lsp)
     (javascript-backend . tern)
     (javascript-backend . lsp)
     (go-backend . go-mode)
     (go-backend . lsp)))
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yapfify yaml-mode xterm-color ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tide typescript-mode flycheck tagedit spaceline powerline smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rake rainbow-delimiters racer pos-tip pyvenv pytest pyenv-mode py-isort pug-mode powershell popwin pip-requirements persp-mode pcre2el paradox orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-bullets open-junk-file nix-mode neotree multi-term move-text mmm-mode minitest markdown-toc magit-gitflow magit-popup macrostep lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc indent-guide hy-mode dash-functional hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile projectile helm-mode-manager helm-make helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio go-guru go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit git-commit transient evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dumb-jump diminish define-word cython-mode csv-mode company-web web-completion-data company-statistics company-go go-mode company-anaconda company column-enforce-mode coffee-mode clojure-snippets clj-refactor hydra inflections multiple-cursors paredit lv clean-aindent-mode cider-eval-sexp-fu eval-sexp-fu cider sesman spinner queue pkg-info parseedn clojure-mode parseclj a epl chruby cargo markdown-mode rust-mode bundler inf-ruby bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
