;; Initialize Cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Theme
(load-theme 'weft t)

;; Custom load path outside of elpa
(add-to-list 'load-path "~/.emacs.d")

(require 'use-package)

;; File modes
(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))

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
(require 'weft-powerline)
(powerline-weft-theme)

;; Clojure mode
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

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

;; Cider setup
(require 'cider)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)

(setq nrepl-hide-special-buffers t)
(setq cider-popup-stacktraces-in-repl t)
(setq cider-repl-history-file "~/.emacs.d/nrepl-history")
 

;; Auto complete
(require 'auto-complete-config)
(ac-config-default)

(require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))

;; Custom shortcuts

(global-set-key (kbd "s-b") 'ido-switch-buffer)
(global-set-key (kbd "s-o") 'ido-find-file)

(defun toggle-nrepl-buffer ()
  "Toggle the nREPL REPL on and off"
  (interactive)
  (if (string= (buffer-name (current-buffer)) "*cider*")
    (delete-window)
    (cider-switch-to-repl-buffer nil)))

(global-set-key (kbd "s-r") 'toggle-nrepl-buffer)

;; Setup Evil (Vi emulation)
(require 'evil)
(evil-mode 1)
(setq evil-cross-lines t)
(setq evil-move-cursor-back nil)

;; Evil surround
(require 'surround)
(global-surround-mode 1)

;; See: https://bitbucket.org/lyro/evil/issue/113
;; (ido-ubiquitous-disable-in evil-ex)

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
