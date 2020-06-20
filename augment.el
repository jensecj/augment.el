;;; augment.el. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; URL: https://github.com/jensecj/augment.el/
;; Keywords: reference, overlay, augmentation
;; Package-Requires: ((emacs "28.0.50"))
;; Package-Version: 20200620
;; Version: 0.1

;;; Commentary:

;;; Code:

;;;; settings

;; TODO: customize face per entry?
(defface augment-face '((t (:inherit link)))
  "Face used for augmented sections.")
;;;; predicates

;;;; core

;;;; public

;;;; mode

(defvar augment-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used by augment buttons.")

;;;###autoload
(define-minor-mode augment-mode
  "Toggle augmenting the buffer."
  nil " augment" augment-map
  )

(define-globalized-minor-mode global-augment-mode
  augment-mode
  (lambda () (augment-mode +1)))

;;;###autoload
(define-minor-mode augment-prog-mode
  "Like `augment-mode', but only augment in comments and strings."
  nil " augment-prog" augment-map
  )

(define-globalized-minor-mode global-augment-prog-mode
  augment-prog-mode
  (lambda () (augment-prog-mode +1)))

;; TODO: benchmark -- what is a reasonable number of augmentation for a buffer?
;; TODO: show augment in eldoc?


(provide 'augment)
