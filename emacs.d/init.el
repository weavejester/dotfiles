;;;; Initialize ;;;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(require 'use-package)

;; Add /usr/local/bin to path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setq default-directory (getenv "HOME"))

(server-start)

(use-package better-defaults)

;;;; Editor ;;;;

(when (window-system)
  (set-default-font "Fira Code"))

;; Enable ligatures
(mac-auto-operator-composition-mode)

(setq inhibit-startup-message t)
(blink-cursor-mode 0)
(menu-bar-mode 1)

;; Disable annoying visible bell on OSX
(setq visible-bell nil)

;; Actually, why not disable the annoying audible bell as well
(setq ring-bell-function 'ignore)

;; Mac Emacs settings
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq fast-but-imprecise-scrolling t)

;; Buffer settings
(setq default-indicate-empty-lines t)
(setq require-final-newline t)
(setq show-trailing-whitespace t)

;; Themes

(use-package neotree
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-window-fixed-size nil)
    (evil-leader/set-key
      "tt" 'neotree-toggle
      "tp" 'neotree-projectile-action)))

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  :config
  (progn
    (doom-themes-neotree-config)
    (setq doom-neotree-line-spacing 0)
    (doom-themes-org-config)))

(use-package solaire-mode
  :init
  (solaire-mode)
  :config
  (progn
    (add-hook 'after-change-major-mode-hook 'turn-on-solaire-mode))
    (solaire-mode-swap-bg))

(use-package powerline)

(use-package airline-themes
  :init
  (progn
    (require 'airline-themes)
    (load-theme 'airline-doom-one t))
  :config
  (progn
    (set-face-attribute 'mode-line          nil :font "Fira Mono for Powerline")
    (set-face-attribute 'mode-line-inactive nil :font "Fira Mono for Powerline")
    (setq powerline-utf-8-separator-left        #xe0b0
          powerline-utf-8-separator-right       #xe0b2
          airline-utf-glyph-separator-left      #xe0b0
          airline-utf-glyph-separator-right     #xe0b2
          airline-utf-glyph-subseparator-left   #xe0b1
          airline-utf-glyph-subseparator-right  #xe0b3
          airline-utf-glyph-branch              #xe0a0
          airline-utf-glyph-readonly            #xe0a2
          airline-utf-glyph-linenumber          #xe0a1)))

(setq eshell-prompt-regexp "^ [^$]*[$] "
      eshell-prompt-function
      (lambda ()
        (concat
         (propertize
          (concat " " (airline-shorten-directory (eshell/pwd) 16) " ")
          'face `(:foreground ,(face-foreground 'airline-normal-outer)
                  :background ,(face-background 'airline-normal-outer)))

         (propertize
          (char-to-string airline-utf-glyph-separator-left)
          'face `(:foreground ,(face-background 'airline-normal-outer)
                  :background ,(face-background 'airline-normal-inner)))

         (let ((git-branch (airline-curr-dir-git-branch-string (eshell/pwd))))
           (if (not (or (null git-branch) (string= "" git-branch)))
               (concat
                (propertize
                 (concat " " (char-to-string airline-utf-glyph-branch) " " git-branch " ")
                 'face `(:foreground ,(face-background 'airline-insert-outer)
                         :background ,(face-background 'airline-insert-inner))))))

         (propertize
          (char-to-string airline-utf-glyph-subseparator-left)
          'face `(:foreground ,(face-background 'airline-insert-outer)
                  :background ,(face-background 'airline-insert-inner)))

         (propertize "$" 'invisible t)
         (propertize " " 'face `()))))

(use-package diminish
  :config
  (progn
    (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
    (eval-after-load "simple" '(diminish 'auto-fill-function))
    (eval-after-load "eldoc" '(diminish 'eldoc-mode))
    (eval-after-load "guide-key" '(diminish 'guide-key-mode))
    (eval-after-load "highlight-parentheses" '(diminish 'highlight-parentheses-mode))
    (eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode " sln"))
    (eval-after-load "projectile" '(diminish 'projectile-mode " prj"))
    (eval-after-load "paredit" '(diminish 'paredit-mode " par"))
    (eval-after-load "company" '(diminish 'company-mode " cmp"))
    (eval-after-load "cider" '(diminish 'cider-mode " cid"))
    (eval-after-load "typed-clojure-mode" '(diminish 'typed-clojure-mode " typ"))
    (eval-after-load "org-indent" '(diminish 'org-indent-mode))
    (eval-after-load "evil-org" '(diminish 'evil-org-mode))
    (eval-after-load "evil-cleverparens" '(diminish 'evil-cleverparens-mode))
    (eval-after-load "autorevert" '(diminish 'auto-revert-mode))
    (eval-after-load "helm" '(diminish 'helm-mode))))

;; No slow stupid flyspell. Die!
(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

;;;; Global keybindings ;;;;

(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)

(global-set-key (kbd "s-z") 'undo)

(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

;;;; Modes ;;;;

(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)

(use-package paredit
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojurescript-mode-hook 'paredit-mode)
    (add-hook 'clojurec-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)))

(use-package paren-face
  :init
  (global-paren-face-mode)
  :config
  (add-hook 'clojure-mode-hook (lambda () (setq paren-face-regexp "#?[](){}[]"))))

(use-package company
  :init (global-company-mode)
  :config
  (progn
    (defun indent-or-complete ()
      (interactive)
      (if (looking-at "\\_>")
          (company-complete-common)
        (indent-according-to-mode)))

    (global-set-key "\t" 'indent-or-complete)))

(use-package magit)

(use-package evil
  :init
  (progn
    (evil-mode 1)
    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader "SPC")
        (evil-leader/set-key "wd" 'delete-window)
        (evil-leader/set-key "wo" 'delete-other-windows)
        (evil-leader/set-key "ws" 'split-window-below)
        (evil-leader/set-key "wh" 'split-window-horizontally)
        (evil-leader/set-key "wv" 'split-window-vertically)
        (evil-leader/set-key "ww" 'other-window)
        (evil-leader/set-key "wk" 'kill-buffer-and-window)))
    (use-package evil-magit
      :config
      (progn
        (evil-leader/set-key "gs" 'magit-status)))
    (use-package evil-org
      :init (add-hook 'org-mode-hook 'evil-org-mode))
    (use-package evil-cleverparens
      :init   (add-hook 'paredit-mode-hook 'evil-cleverparens-mode)
      :config (setq evil-cleverparens-swap-move-by-word-and-symbol t))
    (use-package evil-surround
      :config
      (progn
        (global-evil-surround-mode 1)
        (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))
        (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete)))))
  :config
  (progn
    (setq evil-cross-lines t)
    (setq evil-move-cursor-back nil)))

(defun eshell-here ()
  (interactive)
  (let* ((parent (projectile-project-root))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "cd " parent))
    (eshell-send-input)))

(evil-leader/set-key "sh" 'eshell-here)

(use-package guide-key
  :init (guide-key-mode 1)
  :config
  (progn
    (setq guide-key/idle-delay 1)
    (setq guide-key/recursive-key-sequence-flag t)
    (setq guide-key/popup-window-position 'bottom)
    (setq guide-key/guide-key-sequence
          `("C-h" "C-x" "C-c" ,evil-leader/leader "g" "z"))))

(use-package ido
  :config
  (progn
    (evil-leader/set-key "bs" 'ido-switch-buffer)
    (evil-leader/set-key "bk" 'ido-kill-buffer)
    (evil-leader/set-key "ff" 'ido-find-file)))

(use-package flx-ido
  :init (flx-ido-mode 1)
  :config (setq ido-use-faces nil))

(use-package ido-vertical-mode
  :init (ido-vertical-mode 1))

(use-package helm
  :config
  (progn
    (helm-mode 1)
    (setq helm-autoresize-mode t)
    (setq helm-buffer-max-length 40)
    (diminish 'helm-mode)
    (global-set-key (kbd "M-x") 'helm-M-x)))

(use-package helm-projectile)

(use-package fancy-narrow
  :config
  (evil-leader/set-key
    "nr" 'fancy-narrow-to-region
    "np" 'fancy-narrow-to-page
    "nf" 'fancy-narrow-to-defun
    "nw" 'fancy-widen))

(use-package projectile
  :init (projectile-global-mode)
  :config
  (progn
    (evil-leader/set-key "pf" 'helm-projectile)
    (evil-leader/set-key "pa" 'helm-projectile-ag)
    (evil-leader/set-key "pk" 'projectile-kill-buffers)))

(use-package yasnippet
  :init
  (progn
    (yas-global-mode 1)
    (use-package clojure-snippets)))

(use-package org
  :config
  (progn
    (setq org-tags-column 0)))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode))
  :init (add-hook 'markdown-mode-hook 'auto-fill-mode))

(use-package glsl-mode
  :config (setq c-basic-offset 4))

(use-package elisp-mode
  :config
  (progn
    (evil-leader/set-key "ee" 'eval-last-sexp)))

(use-package clojure-mode
  :mode (("\\.edn$" . clojure-mode))
  :config
  (progn
    (setq clojure-align-forms-automatically t)

    (define-clojure-indent
      (defroutes 'defun)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (context 2)
      (let-routes 1))

    (define-clojure-indent
      (form-to 1))

    (define-clojure-indent
      (match 1)
      (are 2)
      (checking 2)
      (async 1))

    (define-clojure-indent
      (select 1)
      (insert 1)
      (update 1)
      (delete 1))

    (define-clojure-indent
      (run* 1)
      (fresh 1))

    (define-clojure-indent
      (extend-freeze 2)
      (extend-thaw 1))

    (define-clojure-indent
      (go-loop 1))

    (define-clojure-indent
      (this-as 1)
      (specify 1)
      (specify! 1))

    (define-clojure-indent
      (s/fdef 1))

    (setq clojure--prettify-symbols-alist
          '(("fn" . ?λ)))

    (defun toggle-nrepl-buffer ()
      "Toggle the nREPL REPL on and off"
      (interactive)
      (if (string-match "cider-repl" (buffer-name (current-buffer)))
          (delete-window)
        (cider-switch-to-repl-buffer)))

    (defun cider-save-and-refresh ()
      (interactive)
      (save-buffer)
      (call-interactively 'cider-refresh))

    (defun cider-eval-last-sexp-and-append ()
      (interactive)
      (cider-eval-last-sexp '(1)))

    (evil-leader/set-key "eb" 'cider-eval-buffer)
    (evil-leader/set-key "ee" 'cider-eval-last-sexp)
    (evil-leader/set-key "er" 'cider-eval-region)
    (evil-leader/set-key "ef" 'cider-eval-defun-at-point)
    (evil-leader/set-key "ea" 'cider-eval-last-sexp-and-append)
    (evil-leader/set-key "ec" 'cider-eval-last-sexp-and-replace)
    (evil-leader/set-key "ef" 'cider-eval-sexp-at-point)

    (evil-leader/set-key "cd" 'cider-doc)
    (evil-leader/set-key "cc" 'cider-connect)
    (evil-leader/set-key "cj" 'cider-jack-in)
    (evil-leader/set-key "cJ" 'cider-jack-in-clojurescript)
    (evil-leader/set-key "ct" 'cider-test-run-ns-tests)
    (evil-leader/set-key "cT" 'cider-test-run-project-tests)
    (evil-leader/set-key "cn" 'cider-repl-set-ns)
    (evil-leader/set-key "cr" 'toggle-nrepl-buffer)
    (evil-leader/set-key "cf" 'cider-save-and-refresh)

    (global-set-key (kbd "s-r") 'cider-save-and-refresh)))

(use-package cider
  :config
  (progn
    (setq nrepl-hide-special-buffers t)
    (setq cider-popup-stacktraces-in-repl t)
    (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-auto-select-error-buffer nil)
    (setq cider-prompt-save-file-on-load nil)
    (setq cider-repl-display-help-banner nil)
    (setq cider-repl-use-pretty-printing t)
    (setq cider-refresh-before-fn "reloaded.repl/suspend")
    (setq cider-refresh-after-fn "reloaded.repl/resume")
    (setq cider-cljs-lein-repl "(do (reloaded.repl/go) (user/cljs-repl))")
    (setq cider-prompt-for-symbol nil)

    (evil-define-key '(insert normal) cider-mode-map
      (kbd "M-.") 'cider-find-var
      (kbd "M-,") 'cider-pop-back)))

(use-package typed-clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'typed-clojure-mode)
  :config
  (progn
    (evil-leader/set-key "tc" 'typed-clojure-check-ns)))

(use-package clj-refactor
  :init
  (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
  :config
  (progn
    (evil-leader/set-key "rai" 'cljr-add-import-to-ns)
    (evil-leader/set-key "rar" 'cljr-add-require-to-ns)
    (evil-leader/set-key "rau" 'cljr-add-use-to-ns)
    (evil-leader/set-key "rrr" 'cljr-remove-unused-requires)
    (evil-leader/set-key "rcn" 'cljr-clean-ns)
    (evil-leader/set-key "rtf" 'cljr-thread-first-all)
    (evil-leader/set-key "rtl" 'cljr-thread-last-all)
    (evil-leader/set-key "rcc" 'cljr-cycle-coll)
    (evil-leader/set-key "rcp" 'cljr-cycle-privacy)
    (evil-leader/set-key "rcs" 'clojure-toggle-keyword-string)
    (evil-leader/set-key "rfe" 'cljr-create-fn-from-example)
    (evil-leader/set-key "rrs" 'cljr-rename-symbol)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" default)))
 '(package-selected-packages
   (quote
    (nginx-mode fill-column-indicator helm-ag haskell-mode clj-refactor yaml-mode wrap-region use-package typed-clojure-mode smex slamhound rainbow-mode projectile powerline paren-face multiple-cursors markdown-mode linum-relative ido-vertical-mode guide-key glsl-mode flx-ido fancy-narrow expand-region evil-surround evil-org evil-magit evil-cleverparens drag-stuff company clojure-snippets cask better-defaults aggressive-indent ag)))
 '(safe-local-variable-values
   (quote
    ((cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")
     (cider-cljs-lein-repl . "(do (dev) (go) (cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
