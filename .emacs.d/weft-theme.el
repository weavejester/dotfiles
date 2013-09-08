;;; weft-theme.el --- Weft color theme for Emacs.

(unless (>= 24 emacs-major-version)
  (error "weft-theme requires Emacs 24 or later."))

(deftheme weft
  "Weft color theme")

(let ((weft-background "#212120")
      (weft-current-line "#2d2d2c")
      (weft-highlight "#575650")
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
   `(modeline ((t (:background ,weft-gray-lightest :foreground ,weft-gray-light))))
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
   ;; CUA
   `(cua-rectangle ((t (:background ,weft-gray-darkest))))
   ;; IDO
   `(ido-first-match ((t (:foreground ,weft-purple))))
   `(ido-only-match ((t (:foreground ,weft-green))))
   `(ido-subdir ((t (:foreground ,weft-blue-light))))
   ;; Powerline
   `(powerline-active1 ((t (:background "#3a3a3a" :foreground "#ffffff"))))
   `(powerline-active2 ((t (:background "#222222" :foreground "#ffdd44"))))
   `(powerline-inactive1 ((t (:background "#777777" :foreground "#ffffff"))))
   `(powerline-inactive2 ((t (:background "#444444" :foreground "#ffffff"))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'weft)

;;; weft-theme.el ends here
