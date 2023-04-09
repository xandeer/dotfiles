;;; x-org-roam.el --- Settings for org-roam -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-roam-v2-ack t)
(setq org-roam-directory org-directory)
(setq org-roam-database-connector 'sqlite-builtin)
(setq org-roam-db-gc-threshold most-positive-fixnum)
(setq org-roam-db-location (no-littering-expand-var-file-name "roam.db"))
(setq org-roam-dailies-directory "journal/")
(setq org-roam-node-display-template "${title:48} ${tags:36}")
;; (org-roam-node-display-template . "${title:36} ${tags:20} ${backlinkscount:6}")
;; (require 'org-roam-capture)
;; (require 'org-roam-dailies)
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+TITLE: ${title}\n#+CREATED: <%<%Y-%m-%d %a %R>>\n#+filetags: fleeting\n")
         :unnarrowed t)))
(setq org-roam-capture-immediate-template
      '("d" "default" plain "%?"
        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+TITLE: ${title}\n#+CREATED: <%<%Y-%m-%d %a %R>>\n#+filetags: fleeting\n")
        :immediate-finish t
        :unnarrowed t))
(setq org-roam-dailies-capture-templates
      '(("d" "daily" plain
         "*** %(format-time-string \"<%Y-%m-%d %R> \")%?"
         :prepend t
         :jump-to-captured t
         :target (file+head "%<%Y-%m-%d>.org"
                            ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %<%B %m-%d>\n\n\n* %<%B %d>\n\n")
         ;; :file-name "%<%Y-%m-%d>.org"
         ;; :head ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %<%B %m-%d>\n#+STARTUP: content\n\n"
         )))

(with-eval-after-load 'org-roam

  ;; (consult-org-roam-mode 1)

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.25)
                 (window-height . fit-window-to-buffer)))

  (defun x--refresh-roam-buffer ()
    (when (eq (org-roam-buffer--visibility) 'visible)
      (progn
        (select-window (get-buffer-window org-roam-buffer))
        (org-roam-buffer-refresh))))
  (advice-add 'org-roam-buffer-toggle :after #'x--refresh-roam-buffer)

  (define-key org-roam-mode-map (kbd "H-r") 'kill-buffer-and-window))

(defun x--has-roam-tag (tag)
  "Check whether TAG is included in the current file."
  (s-contains? tag (or (cadr (assoc "FILETAGS"
                                    (org-collect-keywords '("filetags"))))
                       "")))

(defun x--enable-valign-when-valign ()
  (when (x--has-roam-tag "valign")
    (valign-mode)))

(defun x--disable-company-when-nocompany ()
  (when (x--has-roam-tag "nocompany")
    ;; (company-mode -1)
    (corfu-mode -1)))

(add-hook 'org-mode-hook #'x--enable-valign-when-valign)
(add-hook 'org-mode-hook #'x--disable-company-when-nocompany)

(defun x/roam-buffer-p ()
  "Whether the current is in roam directory."
  (and (buffer-file-name)
       (s-ends-with? ".org" (buffer-file-name))
       (s-contains? (expand-file-name org-roam-directory) (buffer-file-name))))

(defun x--journal-date (year)
  "Generate a date on today in YEAR."
  (let ((d (split-string (string-remove-suffix ".org" (buffer-name)) "-")))
    (date-to-time (format "20%s-%s-%s +0800" year (nth 1 d) (nth 2 d)))))

(defun x--migrate-journal ()
  "Replace journal's title."
  (interactive)
  (let ((today (x--journal-date "21")))
    (x/replace "^#\\+STARTUP: .*\n" "")
    (x/clear-file-links)
    (x/replace "^\\(#\\+TITLE: \\).*" (format-time-string "\\1%B %m-%d" today))
    (x/replace "^\\* .*" (format-time-string "* %B %d\n** %Y" today))
    (search-forward "** 2021")
    (org-set-tags (format-time-string ":%a:" today))
    (x/replace "^\\*" "**" (point))))

(defun x--is-current-year? (year)
  (= year (string-to-number (format-time-string "%y"))))

(setq x/org-today-tag x/office)
(defun x--insert-journal-in-year (year)
  "Insert a journal heading like: ** YEAR :Mon:."
  (interactive "nYear[< 4: 2x, else: 1x]: ")
  (setq year (+ year (if (< year 4) 20 10)))
  (goto-char (point-min))
  (let ((today (x--journal-date year)))
    (if (x--is-current-year? year) (progn
                                     (search-forward (format-time-string "* %B %d" today))
                                     (newline))
      (goto-char (point-max)))
    (insert (format-time-string "** %Y" today))
    (org-set-tags (concat (format-time-string ":%a:" today)
                          (when (x--is-current-year? year) x/org-today-tag))))
  (end-of-line)
  (newline))

(defun x--clear-file-link-at-point ()
  (save-excursion
    (save-match-data
      (let* ((link (org-element-context))
             (type (org-element-property :type link))
             (path (org-element-property :path link))
             (desc (and (org-element-property :contents-begin link)
                        (org-element-property :contents-end link)
                        (filter-buffer-substring
                         (org-element-property :contents-begin link)
                         (org-element-property :contents-end link)))))
        (goto-char (org-element-property :begin link))
        (when (and (org-in-regexp org-link-any-re 1)
                   (string-equal type "file"))
          (replace-match (or desc path)))))))

(defun x/clear-file-links ()
  "Clear the old file links."
  (interactive)
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (x--clear-file-link-at-point))))

(defun x--remove-links (beg end)
  "Remove Org-mode links between BEG and END in the current buffer.
If BEG and END are not provided, the function operates on the entire buffer."
  (interactive "r")
  ;; Set BEG and END to the buffer boundaries if they are not provided
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  ;; Define the regular expression for Org-mode links using the rx macro
  (let ((org-link (rx (seq "[[" (*? anything) "][" (group (+? anything)) "]]"))))
    ;; Replace Org-mode links with their description using the x/replace function
    (x/replace org-link "\\1" beg end)))

(defun x--remove-links-forward ()
  "Remove links after current point."
  (interactive)
  (x--remove-links (point) (point-max)))

(defun x--remove-links-backward ()
  "Remove links before current point."
  (interactive)
  (x--remove-links (point-min) (point)))

(defun x--remove-links-in-buffer ()
  "Remove links in the current buffer."
  (interactive)
  (x--remove-links (point-min) (point-max)))

(defun x--trim-inline-tags ()
  "Keep only two spaces before tags."
  (interactive)
  (x/replace " \\{2,\\}:" " :"))

(defun x/vocabulary-items ()
  "Make every line to an org item in the current subtree, and sync it to anki."
  (interactive)
  (org-mark-subtree)
  (next-line)
  (beginning-of-line)
  (replace-regexp "^\\(.+\\)$" "- \\1")
  (org-anki-sync-entry))

(autoload #'org-roam-dailies-goto-previous-note "org-roam" nil t)
(autoload #'org-roam-dailies-goto-next-note "org-roam" nil t)
(require 'transient)

(transient-define-prefix x/transient-roam ()
  "Transient for Org Roam."
  [["Roam"
    ("r" "Roam buffer" org-roam-buffer-toggle)
    ("a" "Roam add alias" org-roam-alias-add)
    ("c" "Trim inline tags" x--trim-inline-tags)
    (";" "Roam add tag" org-roam-tag-add)
    ("H-;" "Roam remove tag" org-roam-tag-remove)
    ("s" "Roam DB sync" org-roam-db-sync)]
   ["Dailies"
    ("." "Dailies goto today" org-roam-dailies-goto-today :transient t)
    ("j" "Dailies next" org-roam-dailies-goto-next-note :transient t)
    ("k" "Dailies previous" org-roam-dailies-goto-previous-note :transient t)
    ("3" "Insert journal in 2023" ,(x/interactive-wrapper (x--insert-journal-in-year 3)))
    ("m" "Migrate journal" x--migrate-journal :transient t)]
   ["Anki"
    ("y" "Anki sync entry" org-anki-sync-entry)
    ("Y" "Anki sync all" org-anki-sync-all)
    ("v" "Ankify vocabulary" x/vocabulary-items)]
   ["Misc"
    ("l" "Remove links" x--remove-links)
    ("f" "Fill subtree" x/fill-subtree)
    ("i" "Create ID" org-id-get-create)
    ("n" "Narrow" org-toggle-narrow-to-subtree)]])

(x/define-keys
 org-mode-map
 '(("H-i" org-roam-node-insert)
   ("H-k" x/transient-roam)))

(x/define-keys
 global-map
 `(("H-g" ,(x/interactive-wrapper (org-roam-node-find t)))
   ("H-n" org-roam-node-find)))

;;; tbl mode
(defvar x/tbl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "H-r") #'org-table-recalculate)
    (define-key map (kbd "H-t") (lambda () (interactive)
                                  (org-open-line 1)
                                  (tempel-insert 'day)
                                  (org-cycle)))
    map))

(define-minor-mode x/tbl-mode
  "Minor mode for table."
  :keymap x/tbl-mode-map
  :group 'x/tbl
  :lighter "X/TBL"
  ;; (if x/tbl-mode
  ;;     (progn))
  )

(defun x--tbl-setup ()
  "Enable `x/tbl-mode when it has tag \"tbl\"."
  (when (x--has-roam-tag "tbl")
    (x/tbl-mode 1)))

(defun x/tbl-mode-p ()
  x/tbl-mode)

(add-hook 'text-mode-hook #'x--tbl-setup 90)

(provide 'x-org-roam)
;;; x-org-roam.el ends here
