;; Initiate package management with Marmalade repository
(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Install standard packages if not already installed
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(starter-kit starter-kit-bindings starter-kit-lisp
    undo-tree smart-tab evil evil-leader
    clojure-mode clojure-test-mode clojure-project-mode
    nrepl nrepl-ritz ac-nrepl
    smartparens rainbow-mode powerline
    markdown-mode yaml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Theme
(load-theme 'weft t)

;; Custom load path outside of elpa
(add-to-list 'load-path "~/.emacs.d")

;; File modes
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Disable annoying visible bell on OSX
(setq visible-bell nil)

;; Actually, why not disable the annoying audible bell as well
(setq ring-bell-function 'ignore)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ; scroll window under mouse
(setq scroll-step 1)                                ; keyboard scroll one line at a time

;; Custom mode-line
(require 'powerline)
(powerline-center-evil-theme)

;; Clojure mode
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

;; Smartparens (an alternative to paredit)
(require 'smartparens-config)
(smartparens-global-mode t)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)

;; Remove starter-kit hooks for paredit
(dolist (mode '(scheme emacs-lisp lisp clojure))
  (remove-hook (intern (concat (symbol-name mode) "-mode-hook"))
               'esk-turn-on-paredit))

;; Custom Clojure indents
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

;; nREPL setup
(require 'nrepl)

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces-in-repl t)
(setq nrepl-history-file "~/.emacs.d/nrepl-history")
 
(add-hook 'nrepl-mode-hook 'subword-mode)

;; Auto complete
(require 'auto-complete-config)
(ac-config-default)

(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))

;; Custom shortcuts

(defun newline-from-middle ()
  (interactive)
  (end-of-line)
  (paredit-newline))

(global-set-key (kbd "s-<return>") 'newline-from-middle)
(global-set-key (kbd "s-b") 'ido-switch-buffer)
(global-set-key (kbd "s-o") 'ido-find-file)

(defun toggle-nrepl-buffer ()
  "Toggle the nREPL REPL on and off"
  (interactive)
  (if (string= (buffer-name (current-buffer)) "*nrepl*")
    (delete-window)
    (nrepl-switch-to-repl-buffer nil)))

(global-set-key (kbd "s-r") 'toggle-nrepl-buffer)

;; Setup Evil (Vi emulation)
(require 'evil)
(evil-mode 1)
(setq evil-cross-lines t)
(setq evil-move-cursor-back nil)

;; Evil smart-parens
(require 'evil-sp)

;; See: https://bitbucket.org/lyro/evil/issue/113
(ido-ubiquitous-disable-in evil-ex)

;; Org mode
(require 'evil-org)
(setq org-cycle-separator-lines 1)

;; No slow stupid flyspell. Die!
(eval-after-load "flyspell"
  '(defun flyspell-mode (&optional arg)))

;; Add /usr/local/bin to path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; GLSL mode
(require 'glsl-mode)

;; Start server
(server-start)
