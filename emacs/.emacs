;; -*- lexical-binding: t; -*-
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(setq use-package-always-ensure t)

;; cleanup UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(blink-cursor-mode 1)
(electric-pair-mode 1)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1)

(defun yenda-cleanup-buffer ()
  "clean up buffer"
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(setq show-trailing-whitespace t)

;; cleanup backups
(defvar autosave-dir
  (concat "/home/" (user-login-name) "/.emacs-autosaves/"))

(make-directory autosave-dir t)

(setq
 backup-by-copying t      ; don't clobber symlinks
 auto-save-file-name-transforms `((".*" ,(expand-file-name "\\2" autosave-dir) t))
 backup-directory-alist `(("." . ,autosave-dir))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 ;; use versioned backups
 version-control t)

(defun force-delete-other-windows ()
  "Forcefully delete all other windows, regardless of their status."
  (interactive)
  (mapc (lambda (window)
          (unless (eq window (selected-window)) ; Keep the current window
            (ignore-errors (delete-window window)))) ; Ignore errors that occur when trying to delete a window
        (window-list)))

(global-set-key (kbd "C-x 1") 'force-delete-other-windows)
(global-set-key (kbd "<f1>") (lambda() (interactive) (find-file "/home/yenda/brian/src/dev/development.clj")))
(global-set-key (kbd "<f2>") (lambda() (interactive) (find-file "~/org/todo.org")))

(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package spacemacs-theme
  :config
  (load-theme 'spacemacs-dark t))


;; my packages
(use-package magit)
(use-package forge :after magit)
(use-package magit-todos :after magit
  :config (magit-todos-mode 1))
(use-package git-link)

(use-package which-key
  :config
  (which-key-mode))

(use-package orderless :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package puni
  :defer t
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  :bind
  (:map puni-mode-map
        ("C-<left>" . puni-barf-forward)
        ("C-<right>" . puni-slurp-forward)
        ("M-s M-s" . 'puni-splice)))

(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(use-package clojure-mode)
(use-package cider
  :config
  ;; don't bind M-s it's used by consult!
  (define-key cider-repl-mode-map (kbd "M-s") nil))

(use-package lsp-mode
  :config
  (add-hook 'cider-mode-hook
            (lambda ()
              (define-key cider-mode-map (kbd "M-c M-c")  'cider-eval-print-last-sexp)
              (define-key cider-mode-map (kbd "C-c M-r") 'cider-ns-refresh)))
  (progn
    (setq lsp-enable-symbol-highlighting t
          lsp-enable-indentation t
          lsp-ui-doc-enable t
          lsp-modeline-diagnostics-enable t
          lsp-ui-doc-show-with-cursor nil
          lsp-ui-doc-include-signature t
          lsp-ui-doc-alignment 'window
          lsp-lens-enable t
          lsp-auto-guess-root t
          lsp-log-io t)

    (setq lsp-headerline-breadcrumb-enable nil
          lsp-modeline-code-actions-enable nil)
    (lsp-modeline-code-actions-mode -1)
    (setq lsp-completion-provider :none)
    (lsp-headerline-breadcrumb-mode -1)
    (add-hook 'clojure-mode-hook #'lsp)
    (add-hook 'clojurescript-mode-hook #'lsp)
    (add-hook 'clojurec-mode-hook #'lsp)
    (add-hook 'scss-mode #'lsp)
    (define-key lsp-mode-map (kbd "M-o") lsp-command-map)
    (add-hook 'lsp-mode-hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda ()
                            (when (derived-mode-p 'prog-mode)
                              (lsp-format-buffer)))
                          nil t)))))

(use-package lsp-ui :after lsp-mode)

(use-package lsp-treemacs :after lsp-mode)

(use-package consult-lsp :after lsp-mode
  :bind
  ("M-s F" . consult-lsp-symbols)
  ("M-s f" . consult-lsp-file-symbols))

(setq vc-follow-symlinks t)

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize nil)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . my/consult-ripgrep-region-or-prompt)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  (defun my/consult-ripgrep-region-or-prompt ()
    "Use `consult-ripgrep' to search for the current region if active, or prompt otherwise, in the project directory."
    (interactive)
    (let ((dir (funcall consult-project-function t)))
      (if (use-region-p)
          (consult-ripgrep dir (buffer-substring-no-properties (region-beginning) (region-end)))
        (consult-ripgrep dir))))

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-find consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key ">")
  (setq consult-widen-key "<")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("M-o" . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package wgrep)

(use-package eat
  :straight (eat :files (:defaults "terminfo"
                                   "integration"))
  :bind (("M-RET" . eat-project)
         :map project-prefix-map
         ("t" . eat-project))
  :hook (eshell-load . eat-eshell-mode)
  :config (progn
            (setq eat-kill-buffer-on-exit t)
            (setq eshell-visual-commands nil)))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Corfu
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (interactive)
                        (indent-buffer)
                        (yenda-cleanup-buffer))
                      t t)))

;; fix the C-z hanging and replace it by undo
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x C-g") 'magit-status)

(defun my/magit-todos-find-todo ()
  "Go to todo item read from `magit-todos' list using `consult--read'."
  (interactive)
  (let* ((candidates (when-let* ((items (funcall magit-todos-scanner :sync t
                                                 :directory (funcall consult-project-function t)
                                                 :depth magit-todos-depth)))
                       (cl-loop for item in items
                                collect (magit-todos-item-cons item))))
         (strings (mapcar #'car candidates))
         (choice (consult--read strings
                                :prompt "TODO: "
                                :state
                                (let ((preview (consult--jump-preview)))
                                  (lambda (action cand)
                                    ;; Only preview simple menu items which are markers,
                                    ;; in order to avoid any bad side effects.
                                    (funcall preview action (and (markerp (cdr cand)) (cdr cand)))))
                                :sort t))  ; Optional: sorts the candidates
         (item (alist-get choice candidates nil nil #'equal)))
    (magit-todos-jump-to-item :item item)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((eval setenv "CLASH_STAGE" "local")
     (eval setenv "AWS_LOCAL_SQS" "true")
     (eval setenv "AWS_REGION" "eu-west-2")
     (cider-print-options
      (("print-length" nil)))
     (cider-print-fn . puget)
     (cider-ns-refresh-before-fn . "com.stuartsierra.component.repl/stop")
     (cider-ns-refresh-after-fn . "dev/setup-and-start")
     (cider-known-endpoints
      ("localhost" "9656"))
     (elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 0)
      (thread-last . 0)
      (transient-define-prefix . defmacro)
      (transient-define-suffix . defmacro))
     (checkdoc-package-keywords-flag)
     (cider-clojure-cli-parameters . "-A:dev:rad-dev:reveal -J-Dtrace")
     (eval setenv "AWS_ACCESS_KEY_ID" "EXO03628b63df1311978b45bc3a")
     (eval setenv "AWS_SECRET_ACCESS_KEY" "OR2am2aDESikb1QSmuOen3a3m039X7J8WDXt5p9xeus")
     (eval setenv "AWS_REGION" "ch-dk-2")
     (toc-org-max-depth . 2)
     (org-list-indent-offset . 1)
     (eval progn
           (global-display-fill-column-indicator-mode t)
           (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))
     (cider-ns-refresh-after-fn . "development/restart")))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "chocolate1" :foreground "#000000" :underline nil)))))
