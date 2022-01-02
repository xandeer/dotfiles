;;; x-org-roam.el --- Settings for org-roam -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-roam-v2-ack t)
(require-package 'org-roam)
(with-eval-after-load 'gcmh
  (require 'org-roam))

(setq org-roam-directory org-directory)
(setq org-roam-db-gc-threshold gc-cons-threshold)
(setq org-roam-db-location (no-littering-expand-var-file-name "roam.db"))
(setq org-roam-dailies-directory "journal/")
(setq org-roam-node-display-template "${title:48} ${tags:36}")
;; (org-roam-node-display-template . "${title:36} ${tags:20} ${backlinkscount:6}")
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
                            ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %<%B %m-%d>\n#+STARTUP: content\n\n")
         ;; :file-name "%<%Y-%m-%d>.org"
         ;; :head ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %<%B %m-%d>\n#+STARTUP: content\n\n"
         )))

(with-eval-after-load 'org-roam
  (org-roam-db-autosync-enable)

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
      (company-mode -1)))

  (add-hook 'org-mode-hook #'x--enable-valign-when-valign)
  (add-hook 'org-mode-hook #'x--disable-company-when-nocompany)

  (define-key org-roam-mode-map (kbd "H-r") 'kill-buffer-and-window))

(defun x/roam-buffer-p ()
  "Whether the current is in roam directory."
  (and (buffer-file-name)
       (s-ends-with? ".org" (buffer-file-name))
       (s-contains? (expand-file-name org-roam-directory) (buffer-file-name))))

(defun x/roam-node-find-other-window ()
  (interactive)
  (if (> (count-windows) 1)
      (other-window 1)
    (select-window (split-window-right)))
  (org-roam-node-find))

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

(setq x/org-today-tag "@home:")
(defun x--insert-journal-in-year (year)
  "Insert a journal heading like: ** YEAR :Mon:."
  (interactive "nYear[< 3: 2x, else: 1x]: ")
  (setq year (+ year (if (< year 3) 20 10)))
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
                        (buffer-substring-no-properties
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
  "Remove links between BEG and END."
  (interactive "r")
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (x/replace "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1" beg end))

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

(autoload #'org-roam-dailies-goto-previous-note "org-roam" nil t)
(autoload #'org-roam-dailies-goto-next-note "org-roam" nil t)
(defhydra x-hydra-roam-org (:exit t :columns 4 :idle 0.3)
  "
Roam\n"
  ("r" org-roam-buffer-toggle "roam buffer")
  ("a" org-roam-alias-add "roam add alias")
  (";" org-roam-tag-add "roam add tag")
  ("H-;" org-roam-tag-remove "roam remove tag")
  ("k" org-roam-dailies-goto-previous-note "dailies previous" :exit nil)
  ("j" org-roam-dailies-goto-next-note "dailies next" :exit nil)
  ("l" x--remove-links "remove links")
  ("m" x--migrate-journal "migrate journal" :exit nil)
  ("f" x/fill-subtree "fill subtree")
  ("i" org-id-get-create "create id")
  ("n" org-toggle-narrow-to-subtree "narrow")
  ("0" (x--insert-journal-in-year 0) "insert journal in 2020")
  ("1" (x--insert-journal-in-year 1) "insert journal in 2021")
  ("2" (x--insert-journal-in-year 2) "insert journal in 2022")
  ("3" (x--insert-journal-in-year 3) "insert journal in 2013")
  ("4" (x--insert-journal-in-year 4) "insert journal in 2014")
  ("6" (x--insert-journal-in-year 6) "insert journal in 2016")
  ("7" (x--insert-journal-in-year 7) "insert journal in 2017")
  ("8" (x--insert-journal-in-year 8) "insert journal in 2018")
  ("9" (x--insert-journal-in-year 9) "insert journal in 2019"))
(define-key org-mode-map (kbd "H-k") #'x-hydra-roam-org/body)

(global-set-key (kbd "H-n") #'org-roam-node-find)
(global-set-key (kbd "H-g") #'x/roam-node-find-other-window)

(define-key org-mode-map (kbd "H-i") #'org-roam-node-insert)

(provide 'x-org-roam)
;;; x-org-roam.el ends here
