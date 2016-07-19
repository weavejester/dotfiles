;; Custom powerline theme
(require 'powerline)

(defface powerline-evil-base
  '((t (:foreground "white" :inherit mode-line)))
  "Base face for powerline evil faces."
  :group 'powerline)

(defface powerline-evil-normal
  '((t (:background "green" :inherit powerline-evil-base-face)))
  "Powerline face for evil NORMAL state."
  :group 'powerline)

(defface powerline-evil-insert
  '((t (:background "blue" :inherit powerline-evil-base-face)))
  "Powerline face for evil INSERT state."
  :group 'powerline)

(defface powerline-evil-visual
  '((t (:background "orange" :inherit powerline-evil-base-face)))
  "Powerline face for evil VISUAL state."
  :group 'powerline)

(defface powerline-evil-operator
  '((t (:background "purple" :inherit powerline-evil-base-face)))
  "Powerline face for evil OPERATOR state."
  :group 'powerline)

(defun evil-state-face (active)
  (let ((face (intern (concat "powerline-evil-" (symbol-name evil-state)))))
    (cond ((and active (facep face)) face)
          (active 'powerline-active2)
          (t 'powerline-inactive2))))

(defun evil-state-brief ()
  (replace-regexp-in-string "[<> ]" "" (eval (evil-state-property evil-state :tag))))

(defun powerline-weft-theme ()
  "Setup a mode-line with major, evil, and minor modes centered."
  (interactive)
  (setq-default mode-line-format
        '("%e"
          (:eval
           (let* ((active (powerline-selected-window-active))
                  (mode-line (if active 'mode-line 'mode-line-inactive))
                  (evil-face (ignore-errors (evil-state-face active)))
                  (face1 (if active 'powerline-active1 'powerline-inactive1))
                  (face2 (if active 'powerline-active2 'powerline-inactive2))
                  (separator-left (intern (format "powerline-%s-%s"
                                                  powerline-default-separator
                                                  (car powerline-default-separator-dir))))
                  (separator-right (intern (format "powerline-%s-%s"
                                                   powerline-default-separator
                                                   (cdr powerline-default-separator-dir))))
                  (lhs (append
                        (if evil-mode
                            (list (powerline-raw (evil-state-brief) evil-face 'l)
                                  (powerline-raw " " evil-face)
                                  (funcall separator-left evil-face mode-line)))
                        (list (powerline-raw "%*" nil 'l)
                              (powerline-buffer-id nil 'l)
                              (powerline-raw " ")
                              (funcall separator-left mode-line face1)
                              (powerline-narrow face1 'l)
                              (powerline-vc face1))))
                  (rhs (list (powerline-raw global-mode-string face1 'r)
                             (powerline-raw "%4l" face1 'r)
                             (powerline-raw ":" face1)
                             (powerline-raw "%3c" face1 'r)
                             (funcall separator-right face1 mode-line)
                             (powerline-raw " ")
                             (powerline-raw "%6p" nil 'r)
                             (powerline-hud face2 face1)))
                  (center (append (list (powerline-raw " " face1)
                                        (funcall separator-left face1 face2)
                                        (when (boundp 'erc-modified-channels-object)
                                          (powerline-raw erc-modified-channels-object face2 'l))
                                        (powerline-major-mode face2 'l)
                                        (powerline-process face2)
                                        (powerline-raw " " face2))
                                  (if (split-string (format-mode-line minor-mode-alist))
                                      (list (powerline-minor-modes face2 'l)
                                            (powerline-raw " " face2)
                                            (funcall separator-right face2 face1))
                                    (list (powerline-raw evil-mode-line-tag face2)
                                          (funcall separator-right face2 face1))))))
             (concat (powerline-render lhs)
                     (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                     (powerline-render center)
                     (powerline-fill face1 (powerline-width rhs))
                     (powerline-render rhs)))))))

(provide 'weft-powerline)
