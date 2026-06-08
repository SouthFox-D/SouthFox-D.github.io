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
(require 'dash)
(require 'projectile)
(require 'org)
(require 'tablist)

(defvar blog-root
  (let ((current-file load-file-name))
    (if current-file
        (file-name-directory current-file)
      (projectile-project-root)))
  "The root directory of blog.")

(defun blog-set-keyword (key value)
  "Set post keyword with give KEY and VALUE."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" key ":\\(.*\\)") nil t)
          (if (string-blank-p value)
              (kill-whole-line)
            (replace-match (concat " " value) t nil nil 1))
        (goto-char (point-min))
        (if (re-search-forward "^#\\+[a-zA-Z_]+:.*$" nil t)
            (progn
              (end-of-line)
              (insert "\n"))
          (goto-char (point-min)))
        (insert "#+" key ": " value)))))

(defun blog--get-posts ()
  "Return a list of plists containing data for all org blog posts.
Only reads the first 4KB of the file for efficiency."
  (let* ((posts-dir (expand-file-name "posts" blog-root))
         (post-files (when (file-directory-p posts-dir)
                       (directory-files-recursively posts-dir "\\.org\\'"))))
    (mapcar
     (lambda (path)
       (with-temp-buffer
         (insert-file-contents path nil 0 4096)
         (org-mode)
         (let* ((keywords (org-collect-keywords '("TITLE" "TAGS" "DATE" "DRAFT")))
                (title-val (car (cdr (assoc "TITLE" keywords))))
                (tags-val  (car (cdr (assoc "TAGS" keywords))))
                (date-val  (car (cdr (assoc "DATE" keywords))))
                (draft-val (car (cdr (assoc "DRAFT" keywords)))))
           (list :title (if (and title-val (not (string-empty-p title-val)))
                            (string-trim title-val)
                          (file-name-base path))
                 :filename path
                 :tags (if tags-val (string-trim tags-val) "")
                 :date (if date-val (string-trim date-val) "N/A")
                 :draft (if draft-val (string-trim draft-val) "nil")))))
     post-files)))

(defun blog-entries ()
  "Format blog posts data for `tabulated-list-entries'."
  (mapcar
   (lambda (post)
     (let ((filename (plist-get post :filename))
           (title (plist-get post :title))
           (tags (plist-get post :tags))
           (date (plist-get post :date))
           (draft (plist-get post :draft)))
       (list filename (vector title tags date draft))))
   (blog--get-posts)))

(defun blog-refresh ()
  "Refresh the entries in the tablist buffer."
  (setq tabulated-list-entries (blog-entries)))

(defun blog--file-add-tag-internal (new-tag)
  "Add NEW-TAG to the #+TAGS keyword in the current buffer."
  (let* ((keywords (org-collect-keywords '("TAGS")))
         (current-tags-str (car (cdr (assoc "TAGS" keywords))))
         (current-tags (if current-tags-str
                           (split-string current-tags-str "[ \t]+" t)
                         nil)))
    (unless (member new-tag current-tags)
      (let ((updated-tags-str (string-join (append current-tags (list new-tag)) ":")))
        (blog-set-keyword "TAGS" updated-tags-str)))))

(defun blog-tablist-add-tag-to-marked (tag)
  "Add TAG to all marked blog posts."
  (interactive "sAdd tag: ")
  (let ((files (mapcar #'car (tablist-get-marked-items))))
    (unless files
      (when-let ((current-id (tabulated-list-get-id)))
        (setq files (list current-id))))
    (if (not files)
        (user-error "No post selected")
      (dolist (file files)
        (when (file-exists-p file)
          (let ((buf (find-buffer-visiting file)))
            (if buf
                (with-current-buffer buf
                  (blog--file-add-tag-internal tag)
                  (save-buffer))
              (with-current-buffer (find-file-noselect file)
                (blog--file-add-tag-internal tag)
                (save-buffer)
                (kill-buffer))))))
      (tablist-revert)
      (blog-refresh))))

(define-derived-mode blog-mode tabulated-list-mode "Blog"
  "Major mode for blog posts."
  (setq tabulated-list-format [
			       ("Title" 40 t nil)
			       ("Tags"  20 t nil)
			       ("Date"  20)
			       ("Draft" 8 nil)
			       ])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'blog-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

;;;###autoload
(defun blog-dashboard ()
  "Bring up the blog management dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*Blog Dashboard*")))
    (with-current-buffer buf
      (blog-mode)
      (tablist-revert))
    (pop-to-buffer buf)))

(defun blog-insert-post-link ()
  "Insert an org link to a selected blog post."
  (interactive)
  (let* ((posts-dir (expand-file-name "posts" blog-root))
         (posts (blog--get-posts))
         (candidates (mapcar (lambda (post)
                               (let* ((title (plist-get post :title))
                                      (tags (plist-get post :tags))
                                      (display-name (if (string-empty-p tags)
                                                        title
                                                      (format "%s (%s)" title tags))))
                                 (cons display-name post)))
                             posts))
         (selected (consult--read (mapcar #'car candidates)
                                  :prompt "Insert post link: "
                                  :lookup (lambda (sel &rest _)
                                            (cdr (assoc sel candidates)))))
         (selected-path (plist-get selected :filename))
         (slug (format "/%s/" (file-name-sans-extension
                               (file-relative-name selected-path posts-dir))))
         (title (plist-get selected :title)))
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
