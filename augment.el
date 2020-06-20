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
(require 'jit-lock)

(require 'dash)
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

(defun augment-matches (match-data)
  "Collect all match groups from MATCH-DATA, as strings.

The 0th element is the entire match, the 1st element is the first
capture group, etc."
  (let* ((data (-partition 2 (-butlast match-data)))
         (matches (mapcar (lambda (d) (apply #'buffer-substring-no-properties d)) data)))
    matches))

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

(defun augment--unfontify (start end)
  "Remove all augment overlays from region."
  (dolist (o (overlays-in start end))
    (when (eq (overlay-get o 'category) 'augment)
      (delete-overlay o))))

(defun augment--fontify (start end)
  "Add applicable augment overlays in region between START and END."
  (save-excursion
    (let ((beg-line (progn (goto-char start) (line-beginning-position)))
	        (end-line (progn (goto-char end) (line-end-position))))
      ;; we re-fontify the entire region, so first remove the old overlays
      (augment--unfontify beg-line end-line)
      (goto-char beg-line)

      (dolist (e augment-entries)
        (when-let ((pred (plist-get e :predicate))
                   (regex (plist-get e :regex))
                   (augment-fn (plist-get e :augment-fn)))
          (when (augment--predicate-truthy-p pred)
            (save-excursion ;; each entry needs to look at the entire region
              (while (and (< (point) end-line)
		                      (re-search-forward regex end-line 'move))
                (let ((beg (match-beginning 0))
                      (end (match-end 0)))
	                (when (augment--should-apply-p beg end)
                    (when-let ((data (augment-matches (match-data t)))
                               (augmentation (funcall augment-fn data)))
                      (augment-create-overlay beg end augmentation))))))))))))

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

(defun augment-browse-at-point ()
  "Treat the augmentation at point as an URL, and browse to it."
  (interactive)
  (when-let ((url (augment-at-point)))
    (browse-url url)))

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
  (if augment-mode
      (progn
        (augment-prog-mode -1)
        (jit-lock-register #'augment--fontify))
    (jit-lock-unregister #'augment--fontify)
    (save-restriction
      (widen)
      (augment--unfontify (point-min) (point-max)))))

(define-globalized-minor-mode global-augment-mode
  augment-mode
  (lambda () (augment-mode +1)))

;;;###autoload
(define-minor-mode augment-prog-mode
  "Like `augment-mode', but only augment in comments and strings."
  nil " augment-prog" augment-map
  (if augment-prog-mode
      (progn
        (augment-mode -1)
        (jit-lock-register #'augment--fontify))
    (jit-lock-unregister #'augment--fontify)
    (save-restriction
      (widen)
      (augment--unfontify (point-min) (point-max)))))

(define-globalized-minor-mode global-augment-prog-mode
  augment-prog-mode
  (lambda () (augment-prog-mode +1)))

;; TODO: benchmark -- what is a reasonable number of augmentation for a buffer?
;; TODO: show augment in eldoc?


(provide 'augment)
