;;; x-browser.el --- x-browser -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar x/bookmarks bookmark-alist)

(defvar x/buku-bookmarks nil)

(defvar x/bookmark-history nil)

(defvar x/buku-org (x/expand-note "etc/buku.org"))
(defvar x/buku-html (x/expand-note "etc/buku.html"))

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

(add-hook 'ebuku-mode-hook 'x/update-bookmarks)

(with-eval-after-load 'marginalia
  (defun marginalia-annotate-bookmark (cand)
    "Annotate bookmark CAND with its file name and front context string."
    (when-let ((bm (assoc cand x/bookmarks)))
      (let ((front (bookmark-get-front-context-string bm)))
        (marginalia--fields
         ((marginalia--bookmark-type bm) :truncate 0.1 :face 'marginalia-type)
         ((or (bookmark-get-filename bm)
              (alist-get 'page bm)
              (alist-get 'url bm))
          :face 'marginalia-file-name))))))

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
  (interactive
   (list (let ((narrow (mapcar (pcase-lambda (`(,x ,y ,_)) (cons x y))
                               consult-bookmark-narrow)))
           (consult--read
            (x/bookmark-candidates)
            :prompt "Bookmark: "
            :state (consult--bookmark-preview)
            :category 'bookmark
            :history 'x/bookmark-history
            ;; Add default names to future history.
            ;; Ignore errors such that `consult-bookmark' can be used in
            ;; buffers which are not backed by a file.
            :add-history (ignore-errors (bookmark-prop-get (bookmark-make-record) 'defaults))
            :group (consult--type-group narrow)
            :narrow (consult--type-narrow narrow)))))
  (x/update-bookmarks)
  (x/open-bookmark title))

(x/package-use 'consult-dir)
(x/package-use 'dash-docs)
(x/package-use 'consult-dash)

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

(defun x/buku-export ()
  "Export bookmarks to notes/etc/buku.[org|html]"
  (interactive)
  (shell-command
   (concat "rm -rf " x/buku-org " " x/buku-html
           "; buku --nostdin -e " x/buku-org "; buku --nostdin -e" x/buku-html))
  (find-file x/buku-org))

(defun x/buku-import ()
  "Import bookmarks from notes/etc/buku.org"
  (interactive)
  (shell-command
   (concat "xbuku --nostdin -i " x/buku-org))
  (x/update-bookmarks))

(provide 'x-browser)
;;; x-browser.el ends here
