;;; weft-theme.el --- Weft color theme for Emacs.

(unless (>= 24 emacs-major-version)
  (error "weft-theme requires Emacs 24 or later."))

(deftheme weft
  "Weft color theme")

(custom-theme-set-faces
 'weft
 ;; Frame
 `(default ((t (:foreground "#F8F8F2" :background "#212120"))))
 `(cursor ((t (:foreground "#f92672"))))
 `(hl-line ((t (:background "#2d2d2c"))))
 `(region ((t (:background "#575650"))))
 `(minibuffer-prompt ((t (:foreground "#75715e"))))
 `(mode-line ((t (:background "#666666" :foreground "#ffffff"))))
 `(mode-line-buffer-id ((t (:foreground "#ffffff"))))
 `(show-paren-match-face ((t (:background "#595959"))))
 ;; Main
 `(font-lock-builtin-face ((t (:foreground "#a6e22a"))))
 `(font-lock-comment-face ((t (:foreground "#75715e"))))
 `(font-lock-constant-face ((t (:foreground "#ae81ff"))))
 `(font-lock-doc-string-face ((t (:foreground "#e6db74"))))
 `(font-lock-function-name-face ((t (:foreground "#a6e22a"))))
 `(font-lock-keyword-face ((t (:foreground "#f92672"))))
 `(font-lock-string-face ((t (:foreground "#e6db74"))))
 `(font-lock-type-face ((t (:foreground "#89bdff"))))
 `(font-lock-variable-name-face ((t (:foreground "#a6e22a"))))
 `(font-lock-warning-face ((t (:bold t :foreground "#fd5ff1"))))
 ;; Parenface
 `(parenface-paren-face ((t (:foreground "#999999"))))
 `(parenface-bracket-face ((t (:foreground "#cccccc"))))
 `(parenface-curly-face ((t (:foreground "#cccccc"))))
 ;; Org-Mode
 `(org-block ((t (:foreground "#777777"))))
 `(org-block-begin-line ((t (:foreground "#777777"))))
 `(org-ellipsis ((t (:foreground "#777777"))))
 `(org-level-1 ((t (:foreground "#a6e22a"))))
 `(org-level-2 ((t (:foreground "#f92672"))))
 `(org-level-3 ((t (:foreground "#89bdff"))))
 ;; CUA
 `(cua-rectangle ((t (:background "#141411"))))
 ;; IDO
 `(ido-first-match ((t (:foreground "#ae81ff"))))
 `(ido-only-match ((t (:foreground "#a6e22a"))))
 `(ido-subdir ((t (:foreground "#89bdff"))))
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
   `(company-tooltip-common    ((t (:background "#cccccc" :foreground "#000000")))))

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
