;;; x-browser.el --- x-browser -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'consult)
(require 'marginalia)

(require-package 'ebuku)

(defvar x/bookmarks bookmark-alist)

(defvar x/buku-bookmarks nil)

(defvar x/bookmark-history nil)

(defun x/update-bookmarks ()
  "Update bookmarks from buku cache."
  (interactive)
  (bookmark-maybe-load-default-file)
  (ebuku-update-bookmarks-cache)
  (setq x/buku-bookmarks
        (mapcar (lambda (bm)
                  `(,(alist-get 'title bm)
                    (url . ,(alist-get 'url bm))
                    (index . ,(alist-get 'index bm))
                    (handler . eww-browse-with-external-browser)))
                ebuku-bookmarks))
  (setq x/bookmarks (append x/buku-bookmarks bookmark-alist)))

(defun marginalia-annotate-bookmark (cand)
  "Annotate bookmark CAND with its file name and front context string."
  (when-let ((bm (assoc cand x/bookmarks)))
    (let ((front (bookmark-get-front-context-string bm)))
      (marginalia--fields
       ((marginalia--bookmark-type bm) :truncate 0.1 :face 'marginalia-type)
       ((or (bookmark-get-filename bm)
            (alist-get 'page bm)
            (alist-get 'url bm))
        :face 'marginalia-file-name)))))

(defun x/bookmark-candidates ()
  "Generate buku bookmarks candidates."
  (mapcar (lambda (cand)
            (propertize (car cand)))
          x/bookmarks))

(defun x/open-bookmark (title)
  (if (assoc title bookmark-alist)
      (bookmark-jump title)
    (let ((url (alist-get 'url (assoc title x/bookmarks))))
      (eww-browse-with-external-browser url))))

(defun x/consult-bookmark (title)
  "Select pattern with buku."
  (x/update-bookmarks)
  (interactive
   (list (consult--read
          (x/bookmark-candidates)
          :prompt "Bookmark: "
          :category 'bookmark
          :history 'x/bookmark-history)))
  (x/open-bookmark title))

(defun x/buku--candidates ()
  (mapcar (lambda (cand)
            (propertize (car cand)))
          x/buku-bookmarks))

(defun x/buku--read (action)
  (x/update-bookmarks)
  (apply action (list (consult--read
                       (x/buku--candidates)
                       :prompt "Bookmark: "
                       :category 'bookmark
                       :history 'x/bookmark-history))))

(defun x/buku-add ()
  "Add a bookmark to the buku database."
  (interactive)
  (let ((url "")
        (index "")
        (title ""))
    (setq url (read-from-minibuffer "Bookmark URL? "))
    (if ebuku-retrieve-url-metadata
        (with-temp-buffer
          (if (ebuku--call-buku `("--add" ,url))
              (progn
                (goto-char (point-min))
                (if (re-search-forward
                     "already exists at index \\([[:digit:]]+\\)" nil t)
                    (user-error (concat
                                 "Bookmark already exists at index "
                                 (match-string 1))))
                (re-search-forward "^\\([[:digit:]]+\\)\\. \\(.+\\)$")
                (setq index (match-string 1))
                (setq title (match-string 2))
                (if (re-search-forward "^\\s-+\\+ \\(.+\\)$" nil t)
                    (setq comment (match-string 3))))
            (error "Failed to add bookmark"))))
    (setq title (read-from-minibuffer "Bookmark title? " title))
    (if ebuku-retrieve-url-metadata
        (with-temp-buffer
          (if (ebuku--call-buku `("--update" ,index
                                  "--title" ,title))
              (progn
                (ebuku-refresh)
                (message "Bookmark added."))
            (error "Failed to modify bookmark metadata")))
      (with-temp-buffer
        (if (ebuku--call-buku `("--add" ,url
                                "--title" ,title))
            (progn
              (ebuku-refresh)
              (message "Bookmark added."))
          (error "Failed to add bookmark"))))))

(defun x/buku--edit (title)
  (if-let ((bm (assoc title x/buku-bookmarks)))
      (let ((index (alist-get 'index bm))
            (title (read-from-minibuffer
                    "Title? "
                    title))
            (url (read-from-minibuffer
                  "URL? "
                  (alist-get 'url bm))))
        (with-temp-buffer
          (if (ebuku--call-buku `("--update" ,index
                                  "--title" ,title
                                  "--url" ,url))
              (progn
                (ebuku-refresh)
                (message "Bookmark updated."))
            (error "Failed to update bookmark"))))))

(defun x/buku-edit ()
  "Edit a bookmark from the buku database."
  (interactive)
  (x/buku--read #'x/buku--edit))

(defun x/buku--delete (title)
  (if-let ((bm (assoc title x/buku-bookmarks)))
      (let ((index (alist-get 'index bm)))
        (if (y-or-n-p (concat "Delete bookmark \"" title "\"? "))
            (progn
              (ebuku--delete-bookmark-helper index)
              (ebuku-refresh))))))

(defun x/buku-delete ()
  "Delete a bookmark from the buku database."
  (interactive)
  (x/buku--read #'x/buku--delete))

(defun x/buku-export-to-download ()
  "Export bookmarks to ~/Downloads/buku.html"
  (interactive)
  (let ((file "~/Downloads/buku.html"))
    (x/async-command
     (concat "rm -rf " file
             "; buku -e " file))))

(provide 'x-browser)
;;; x-browser.el ends here
