(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(defvar my-packages '(monokai-theme
		      projectile
		      company
		      aggressive-indent
		      magit
		      helm

		      markdown-mode
		      mustache-mode
		      
		      smartparens
		      rainbow-delimiters
		      clojure-mode
		      cider

		      ruby-mode
		      robe
		      flymake-ruby

		      page-break-lines
		      ;;xquery-mode
		      slime))

(dolist (p my-packages)
  (unless (package-installed-p p)
    ;; TRADEOFF: less init time, more install time
    (package-refresh-contents)
    (package-install p)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)

(load-theme 'monokai t)

(global-set-key (kbd "<f1>") (lambda() (interactive) (find-file (concat "~/org/daily/" (format-time-string "%Y%m%d") ".org"))))
(global-set-key (kbd "<f2>") (lambda() (interactive) (find-file "~/org/todo.org")))
(global-set-key (kbd "C-x C-g") 'magit-status)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; HELM

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(helm-mode 1)

;; LISP

(require 'smartparens-config)
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; COMMON LISP

(setq inferior-lisp-program "/bin/sbcl"
      lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-startup-animation nil)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")

(require 'slime)
(slime-setup '(slime-fancy))

;; CLOJURE

;; (require 'cider-any)
;; (require 'cider-any-uruk)

;; (add-hook 'xquery-mode-hook 'cider-any-mode)

;; (setq cider-any-uruk-uri "xdbc://localhost:8889/"
;;       cider-any-uruk-user "admin"
;;       cider-any-uruk-password "admin")

(add-hook 'clojure-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
;; clojure mode hooks

(defun lambdawerk-cleanup-buffer ()
	"clean up buffer"
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defun lambdawerk-indent ()
	(define-clojure-indent
		(defui '(1 nil nil (1)))
		(with 'defun)))

(add-hook 'clojure-mode-hook
	  (lambda ()
	    (lambdawerk-indent)
	    (add-hook 'before-save-hook 'lambdawerk-cleanup-buffer t t)))

(global-set-key (kbd "C-<right>") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-<left>") 'sp-forward-barf-sexp)

;; RUBY

(add-to-list 'auto-mode-alist
	     '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
	     '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; ORGMODE

(setq org-src-fontify-natively t)
(defun my/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
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

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c s e") 'org-edit-src-code)
  (define-key org-mode-map (kbd "C-c s i") 'my/org-insert-src-block)
  (define-key org-mode-map (kbd "C-c C-x i") 'org-clock-in))

;; Babel

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (ruby . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "ruby")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; CUSTOM

(add-hook 'after-init-hook 'global-company-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (slime cider clojure-mode projectile monokai-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:inherit bold :background "SlateGray4" :underline nil))))
 '(helm-source-header ((t (:background "dark sea green" :foreground "#272822" :underline nil)))))
