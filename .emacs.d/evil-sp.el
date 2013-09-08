(require 'evil)
(require 'smartparens)

;; Evil smartparens motions

(evil-define-motion evil-sp-forward-sexp (count)
  (sp-forward-sexp count))

(evil-define-motion evil-sp-backward-sexp (count)
  (sp-backward-sexp count))

(evil-define-motion evil-sp-up-sexp (count)
  (sp-up-sexp count))

(evil-define-motion evil-sp-backward-up-sexp (count)
  (sp-backward-up-sexp count))

(evil-define-motion evil-sp-down-sexp (count)
  (sp-down-sexp count))

(define-key evil-motion-state-map "e" 'evil-sp-forward-sexp)
(define-key evil-motion-state-map "E" 'evil-sp-backward-sexp)

;; Evil smartparens text objects

(evil-define-text-object evil-a-sexp (count &optional beg end type)
  (evil-an-object-range count beg end #'sp-forward-sexp #'sp-backward-sexp))

(evil-define-text-object evil-inner-sexp (count &optional beg end type)
  (evil-inner-object-range count beg end #'sp-forward-sexp #'sp-backward-sexp))

(define-key evil-outer-text-objects-map "e" 'evil-a-sexp)
(define-key evil-inner-text-objects-map "e" 'evil-a-sexp)

(provide 'evil-sp)
