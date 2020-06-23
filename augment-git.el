;;; augment-git.el. --- -*- lexical-binding: t; -*-
(require 'vc)
(require 'project)


(defun augment-git--remotes ()
  "Return a list of remotes as reported by `vc'."
  (split-string
   (s-trim
    (with-output-to-string
      (with-current-buffer standard-output
        (vc-git--out-ok "remote"))))))

(defun augment-git--remote-url (remote)
  "Return the url of REMOTE."
  (s-trim
   (with-output-to-string
		 (with-current-buffer standard-output
		   (vc-git--out-ok "config" (concat "remote." remote ".url"))))))

(defun augment-git--git-repo-p ()
  "Return non-nil if the current buffer is a file belonging to a git-repo."
  (and (buffer-file-name) ;; the buffer should be an existing file
       (eq 'Git (vc-responsible-backend (cdr (project-current))))))

(defun augment-git--github-repo-p ()
  "Return t if the current buffer is a file belonging to a github repo."
  (and (augment-git--git-repo-p)
       (-any
        (lambda (u)
          (or (s-prefix-p "git@github.com" u)
              (s-prefix-p "https://github.com" u)))
        (-map #'augment-git--remote-url (augment-git--remotes)))))

(defun augment-git--augment-github-commit (matchdata)
  "Return the augment for a github commit; a link to the commits
page on github.com."
  (let* ((commit (nth 1 matchdata))
         (remote (car (augment-git--remotes)))
         (remote-url (augment-git--remote-url remote)))
    (concat
     (->> remote-url
          (s-chop-suffix ".git")
          (s-replace "git@github.com:" "https://github.com/"))
     "/commit/"
     commit)))

(defvar augment-entry-github-commits
  (list
   :predicate #'augment-git--github-repo-p
   :order 9
   :regex (rx "@" (group (>= 8 hex)))
   :augment-fn #'augment-git--augment-github-commit)
  "Augment commit-references with a link to the commit on github.com")

(defun augment-git--augment-github-issue (matchdata)
  "Return the augment for a github issue; A link to the issues
page on github.com"
  (let* ((issue (nth 1 matchdata))
         (remote (car (augment-git--remotes)))
         (remote-url (augment-git--remote-url remote)))
    (concat
     (->> remote-url
          (s-chop-suffix ".git")
          (s-replace "git@github.com:" "https://github.com/"))
     "/issues/"
     issue)))

(defvar augment-entry-github-issues
  (list
   :predicate #'augment-git--github-repo-p
   :order 9
   :regex (rx "#" (group (1+ num)))
   :augment-fn #'augment-git--augment-github-issue)
  "Augment issue-references with a link to the issue on github.com")


(provide 'augment-git)
