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

(require 'syntax)
;;;; settings

;; TODO: customize face per entry?
(defface augment-face '((t (:inherit link)))
  "Face used for augmented sections.")

(defvar augment-entries nil
  "List of augmentation entries to apply.

An entry is a plist of (:PREDICATE :ORDER :REGEXP :AUGMENT-FN).

:PREDICATE determines if an entry is applied. It can be either:
* A symbol
* A function, in which case it is called without arguments.

:ORDER the order in which the augmentation should be checked and
applied (from lowest to highest), this acts to prioritize one
entry above another, as only one augment will be applied at any
one point.

:REGEXP is an elisp regular expression.

:AUGMENT-FN is a function which is called with a list of string
matches from REGEXP, the 0th element is the entire match, the 1st
element is the first group match, etc. The function should return
a string which is used as the content of the augmentation.")

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

(defun augment--should-apply-p (beg end)
  "Predicate which determites if an augmentation should be
applied in region BEG END."
  (and
   ;; don't create multiple overlays at the same point
   (not (augment-in-region beg end)) ;; TODO: add priority to entries?
   ;; if `augment-prog-mode' is active, only augment strings and comments.
   (or (not augment-prog-mode) (augment-in-comment-or-string-p))))

(defun augment--predicate-truthy-p (pred)
  "Returns non-nil if PRED is true."
  (cond
   ((functionp pred) (funcall pred))
   ((symbolp pred) pred)))

;;;; public

(defun augment-sort-entries ()
  "Sort augment entries by their `:order' keyword."
  (setq augment-entries
        (sort augment-entries
              (lambda (a b)
                (ignore-errors
                  (< (plist-get a :order) (plist-get b :order)))))))

(defun augment-at-point (&optional p)
  "Return the augment at point P."
  (let ((aug))
    ;; there should only be one `augment' overlay
    (dolist (o (overlays-at (or p (point))))
      (when-let ((a (overlay-get o 'augmentation)))
        (setq aug a)))
    aug))

(defun augment-in-region (beg end)
  "Return a list of all augments in region between BEG and END."
  (let ((augs))
    ;; there should only be one `augment' overlay
    (dolist (o (overlays-in beg end))
      (when-let ((a (overlay-get o 'augmentation)))
        (push a augs)))
    augs))
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
