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

(defun augment-in-string-p (&optional pos)
  "Return non-nil if POS is inside string syntax."
  (nth 3 (syntax-ppss pos)))

(defun augment-in-comment-p (&optional pos)
  "Return non-nil if POS is inside comment syntax."
  (nth 4 (syntax-ppss pos)))

(defun augment-in-comment-or-string-p (&optional pos)
  "Character address of start of comment or string; nil if not in either."
  (nth 8 (syntax-ppss pos)))

;;;; core


(defun augment-create-overlay (beg end aug)
  "Augment the region between BEG and END, with content AUG."
  (let ((o (make-overlay beg end nil t nil)))
	  (overlay-put o 'category 'augment)
    (overlay-put o 'augmentation aug)))
;;;; public

;;;; mode

(defvar augment-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used by augment buttons.")

(defun augment--set-overlay-properties ()
  "Set properties of augment overlays."
  (put 'augment 'evaporate t)
  (put 'augment 'face 'augment-face)
  (put 'augment 'keymap augment-map)
  (put 'augment 'follow-link t))

(augment--set-overlay-properties)

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
