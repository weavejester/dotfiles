;;; weft-theme.el --- Weft color theme for Emacs.

(unless (>= 24 emacs-major-version)
  (error "weft-theme requires Emacs 24 or later."))

(deftheme weft
  "Weft color theme")

(let ((weft-background "#212120")
      (weft-current-line "#2d2d2c")
      (weft-highlight "#575650")
      (weft-comment "#777777")
      (weft-blue-light "#89BDFF")
      (weft-gray "#595959")
      (weft-gray-darker "#983830")
      (weft-gray-darkest "#141411")
      (weft-gray-lightest "#595959")
      (weft-gray-light "#E6E6E6")
      (weft-green "#A6E22A")
      (weft-green-light "#A6E22E")
      (weft-magenta "#F92672")
      (weft-purple "#AE81FF")
      (weft-purple-light "#FD5FF1")
      (weft-yellow "#E6DB74")
      (weft-yellow-dark "#75715E")
      (weft-yellow-light "#F8F8F2"))
  (custom-theme-set-faces
   'weft
   ;; Frame
   `(default ((t (:foreground ,weft-yellow-light :background ,weft-background))))
   `(cursor ((t (:foreground ,weft-magenta))))
   `(hl-line ((t (:background ,weft-current-line))))
   `(region ((t (:background ,weft-highlight))))
   `(minibuffer-prompt ((t (:foreground ,weft-yellow-dark))))
   `(mode-line ((t (:background "#666666" :foreground "#ffffff"))))
   `(mode-line-buffer-id ((t (:foreground "#ffffff"))))
   `(show-paren-match-face ((t (:background ,weft-gray-lightest))))
   ;; Main
   `(font-lock-builtin-face ((t (:foreground ,weft-green))))
   `(font-lock-comment-face ((t (:foreground ,weft-yellow-dark))))
   `(font-lock-constant-face ((t (:foreground ,weft-purple))))
   `(font-lock-doc-string-face ((t (:foreground ,weft-yellow))))
   `(font-lock-function-name-face ((t (:foreground ,weft-green))))
   `(font-lock-keyword-face ((t (:foreground ,weft-magenta))))
   `(font-lock-string-face ((t (:foreground ,weft-yellow))))
   `(font-lock-type-face ((t (:foreground ,weft-blue-light))))
   `(font-lock-variable-name-face ((t (:foreground ,weft-magenta))))
   `(font-lock-warning-face ((t (:bold t :foreground ,weft-purple-light))))
   ;; Parenface
   `(parenface-paren-face ((t (:foreground "#999999"))))
   `(parenface-bracket-face ((t (:foreground "#cccccc"))))
   `(parenface-curly-face ((t (:foreground "#cccccc"))))
   ;; Org-Mode
   `(org-block ((t (:foreground ,weft-comment))))
   `(org-block-begin-line ((t (:foreground ,weft-comment))))
   `(org-ellipsis ((t (:foreground ,weft-comment))))
   `(org-level-1 ((t (:foreground ,weft-green))))
   `(org-level-2 ((t (:foreground ,weft-magenta))))
   `(org-level-3 ((t (:foreground ,weft-blue-light))))
   ;; CUA
   `(cua-rectangle ((t (:background ,weft-gray-darkest))))
   ;; IDO
   `(ido-first-match ((t (:foreground ,weft-purple))))
   `(ido-only-match ((t (:foreground ,weft-green))))
   `(ido-subdir ((t (:foreground ,weft-blue-light))))
   ;; Powerline
   `(powerline-active1 ((t (:background "#444444" :foreground "#ffffff"))))
   `(powerline-active2 ((t (:background "#333333" :foreground "#ffffff"))))
   `(powerline-inactive1 ((t (:background "#666666" :foreground "#ffffff"))))
   `(powerline-inactive2 ((t (:background "#444444" :foreground "#ffffff"))))
   `(powerline-evil-normal ((t (:background "#559900" :foreground "#002200"))))
   `(powerline-evil-insert ((t (:background "#dd3311" :foreground "#220000"))))
   `(powerline-evil-visual ((t (:background "#dd9900" :foreground "#222200"))))
   ;; Company
   `(company-preview ((t (:background "#ffff99" :foreground "#000000"))))
   `(company-tooltip ((t (:background "#aaaaaa" :foreground "#000000"))))
   `(company-scrollbar-bg ((t (:background "#999999"))))
   `(company-scrollbar-fg ((t (:background "#555555"))))
   `(company-tooltip-selection ((t (:background "#eeeeee" :foreground "#000000"))))
   `(company-tooltip-common    ((t (:background "#cccccc" :foreground "#000000"))))))

(custom-theme-set-variables
 'weft
 `(hl-paren-colors '("#ffffff")))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'weft)

;;; weft-theme.el ends here
