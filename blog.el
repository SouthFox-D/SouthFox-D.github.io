;;; blog.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 SouthFox
;;
;; Author: SouthFox <master@southfox.me>
;; Maintainer: SouthFox <master@southfox.me>
;; Created: June 08, 2026
;; Modified: June 08, 2026
;; Version: 0.0.1
;; Keywords: hypermedia
;; Homepage: https://git.southfox.me/southfox/blog
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'consult)
(require 'projectile)
(require 'org)

(defvar blog-root
  (let ((current-file load-file-name))
    (if current-file
        (file-name-directory current-file)
      (projectile-project-root)))
  "The root directory of blog.")

(defun blog--get-org-title (file)
  "Extract the title from FILE.
Only reads the first 4KB of the file for efficiency."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (string-trim (org-get-title))))

(defun blog-insert-post-link ()
  "Insert an org link to a selected blog post."
  (interactive)
  (let* ((posts-dir (expand-file-name "posts" blog-root))
         (post-files (directory-files-recursively posts-dir "\\.org\\'"))
         (candidates (mapcar (lambda (path)
                               (cons (blog--get-org-title path) path))
                             post-files))
         (selected-title (consult--read (mapcar #'car candidates)
                                        :prompt "Insert post link: "
                                        :lookup (lambda (selected &rest _)
                                                  (assoc selected candidates))))
         (selected-path (cdr selected-title))
         (slug (format "/%s/" (file-name-sans-extension
                               (file-relative-name selected-path posts-dir))))
         (title (car selected-title)))
    (if selected-path
        (insert (org-link-make-string slug title))
      (user-error "No post selected"))))

(defun blog-create-new-post (file-id)
  "Create a new blog post with FILE-ID in posts/YEAR/MONTH/FILE-ID.org."
  (interactive "sPost ID: ")
  (let* ((now (current-time))
         (year (format-time-string "%Y" now))
         (month (format-time-string "%m" now))
         (target-dir (expand-file-name (format "posts/%s/%s" year month) blog-root))
         (file-path (expand-file-name (concat file-id ".org") target-dir))
         (keywords `(("title" . ,file-id)
                     ("author" . "SouthFox")
                     ("date" . ,(format-time-string "%Y-%m-%d %H:%M:%S" now))
                     ("draft" . "t")
                     ("tags" . ,(if (string-match-p "fox-thinking" file-id)
                                    "FoxThinking"
                                  "")))))
    (make-directory target-dir t)
    (if (file-exists-p file-path)
        (message "%s already exists!" file-path)
      (with-temp-file file-path
        (insert
         (org-element-interpret-data
          (mapcar (lambda (kv)
                    (org-element-create
                     'keyword
                     (list :key (car kv)
                           :value (cdr kv))))
                  keywords))))
      (message "Created new post at: %s" file-path)
      (find-file file-path))))

(provide 'blog)
;;; blog.el ends here
