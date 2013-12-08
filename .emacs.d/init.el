;;;; Initialize ;;;;

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(add-to-list 'load-path "~/.emacs.d")
(require 'use-package)

;; Add /usr/local/bin to path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setq default-directory (getenv "HOME"))

(server-start)


;;;; Visual ;;;;

;; Disable annoying visible bell on OSX
(setq visible-bell nil)

;; Actually, why not disable the annoying audible bell as well
(setq ring-bell-function 'ignore)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ; scroll window under mouse
(setq scroll-step 1)                                ; keyboard scroll one line at a time

;; Color theme
(load-theme 'weft t)

;; Custom mode-line
(require 'powerline)
(require 'weft-powerline)
(powerline-weft-theme)


;;;; Editor ;;;;

;; Custom shortcuts
(global-set-key (kbd "s-b") 'ido-switch-buffer)
(global-set-key (kbd "s-o") 'ido-find-file)

;; No slow stupid flyspell. Die!
(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

(use-package evil
  :init
  (progn
    (evil-mode 1)
    (use-package evil-paredit
      :init (add-hook 'paredit-mode-hook 'evil-paredit-mode))
    (use-package surround
      :init
      (progn
        (global-surround-mode 1)
        (add-to-list 'surround-operator-alist '(evil-paredit-change . change))
        (add-to-list 'surround-operator-alist '(evil-paredit-delete . delete)))))
  :config
  (progn
    (setq evil-cross-lines t)
    (setq evil-move-cursor-back nil)

    (evil-define-motion evil-forward-sexp (count)
      (if (paredit-in-string-p)
          (evil-forward-word-end count)
          (paredit-forward count)))

    (evil-define-motion evil-backward-sexp (count)
      (if (paredit-in-string-p)
          (evil-backward-word-begin)
          (paredit-backward count)))

    (evil-define-motion evil-forward-sexp-word (count)
      (if (paredit-in-string-p)
          (evil-forward-word-begin count)
          (progn (paredit-forward count)
                 (skip-chars-forward "[:space:]"))))

    (define-key evil-motion-state-map "w" 'evil-forward-sexp-word)
    (define-key evil-motion-state-map "e" 'evil-forward-sexp)
    (define-key evil-motion-state-map "b" 'evil-backward-sexp)))


;;;; Modes ;;;;

(use-package flx-ido
  :init (flx-ido-mode 1)
  :config (setq ido-use-faces nil))

(use-package ido-vertical-mode
  :init (ido-vertical-mode 1))

(use-package projectile
  :init (projectile-global-mode)
  :config
  (progn
    (evil-ex-define-cmd "pf[iles]" 'projectile-find-file)))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))

(use-package glsl-mode)

(use-package clojure-mode
  :mode ("\\.edn$" . clojure-mode)
  :init
  (progn
    (use-package cider
      :init
      (progn
        (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
        (add-hook 'cider-repl-mode-hook 'subword-mode))
      :config
      (progn
        (setq nrepl-hide-special-buffers t)
        (setq cider-popup-stacktraces-in-repl t)
        (setq cider-repl-history-file "~/.emacs.d/nrepl-history"))))
  :config
  (progn
    (use-package auto-complete-config
      :init (ac-config-default))

    (use-package ac-nrepl
      :init
      (progn
        (add-hook 'cider-mode-hook 'ac-nrepl-setup)
        (eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))))

    (define-clojure-indent
      (defroutes 'defun)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (context 2))

    (define-clojure-indent
      (form-to 1))

    (define-clojure-indent
      (match 1)
      (are 2))

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

    (defun toggle-nrepl-buffer ()
      "Toggle the nREPL REPL on and off"
      (interactive)
      (if (string= (buffer-name (current-buffer)) "*cider*")
          (delete-window)
        (cider-switch-to-repl-buffer nil)))

    (global-set-key (kbd "s-r") 'toggle-nrepl-buffer)))
