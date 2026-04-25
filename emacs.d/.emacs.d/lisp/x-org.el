;;; x-org.el --- Settings for org-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (straight-use-package '(org :type built-in))

(setq org-modules
      '(ol-docview
        ol-info
        ;; org-toc
        org-id
        org-habit
        org-inlinetask
        org-protocol
        ol-w3m))
(setq org-directory "~/projects/personal/notes/")
(setq org-startup-folded 'nofold)
(setq org-archive-location "archive/%s_archive::* Archived Tasks")
(setq org-clone-delete-id t)
(setq org-id-locations-file-relative t)
(setq org-id-locations-file (x/expand-note "id-locations.el"))
(setq org-id-extra-files `,(remove (expand-file-name "index.org" org-directory)
                                   (directory-files org-directory 'full (rx ".org" eos))))
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(setq org-image-actual-width '(200))
(setq org-fontify-quote-and-verse-blocks t)
(setq org-return-follows-link t)
(setq org-imenu-depth 3)
(setq org-list-allow-alphabetical t)
(setq org-edit-src-content-indentation 0
      ;; Disable first level indentation in code block. https://emacs.stackexchange.com/a/18892/16450
      org-src-preserve-indentation t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t)
(setq org-indent-indentation-per-level 2)
(setq org-catch-invisible-edits 'error)
(setq org-startup-with-inline-images t)
(setq org-cycle-separator-lines 0)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-use-speed-commands t)
(setq org-blank-before-new-entry
      '((heading)
        (plain-list-item . auto)))
(setq org-export-preserve-breaks t)
(setq org-confirm-babel-evaluate nil)
(setq org-startup-indented t)
(setq org-indent-indentation-per-level 2)
(setq org-adapt-indentation nil)
(setq org-cycle-level-faces t)
(setq org-fontify-done-headline t)
(setq org-fontify-todo-headline t)
(setq org-hide-emphasis-markers t)
(setq org-ellipsis "…")
(setq org-auto-align-tags nil)
(setq org-tags-column 0)
(setq org-pretty-entities t)
(setq org-fontify-emphasized-text t)
(setq org-fontify-whole-heading-line t)
(setq org-allow-promoting-top-level-subtree t)
(setq org-fontify-whole-block-delimiter-line t)

;; global Effort estimate values
;; global STYLE property values for completion
(setq org-global-properties
      '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
        ("STYLE_ALL"  . "habit")))
(setq org-speed-commands
      '(("Outline Navigation")
        ("j" . (org-speed-move-safe 'org-next-visible-heading))
        ("k" . (org-speed-move-safe 'org-previous-visible-heading))
        ("f" . (org-speed-move-safe 'org-forward-heading-same-level))
        ("b" . (org-speed-move-safe 'org-backward-heading-same-level))
        ("u" . (org-speed-move-safe 'outline-up-heading))

        ("Outline Visibility")
        (" " . org-display-outline-path)
        ("n" . org-toggle-narrow-to-subtree)
        ("v" . (lambda () (interactive) (recenter-top-bottom 1)))
        ("=" . org-columns)

        ("Outline Structure Editing")
        ("c" . org-cut-subtree)
        ("w" . org-metaup)
        ("s" . org-metadown)
        ("." . org-metaright)
        ("," . org-metaleft)
        (">" . org-shiftmetaright)
        ("<" . org-shiftmetaleft)
        ("^" . org-sort)
        ("R" . org-refile)
        ("m" . org-mark-subtree)
        ("a" . org-insert-heading-after-current)

        ("Clock Commands")
        ("i" . org-clock-in)
        ("l" . org-clock-out)
        ("S" . x/org-schedule)

        ("Meta Data Editing")
        ("t" . org-todo)
        ("z" . org-add-note)
        (";" . org-set-tags-command)
        ("e" . org-set-effort)
        ("E" . org-inc-effort)
        ("W" . (lambda (m) (interactive "sMinutes before warning: ") (org-entry-put (point) "APPT_WARNTIME" m)))

        ("Agenda Views etc")
        ;; ("q" . org-agenda-list)

        ("Misc")
        ("q" . x/unfill-subtree)
        ("o" . org-open-at-point)
        ("?" . org-speed-command-help)))

(defun x/org-toggle-marks ()
  "Toggle visibility of `org-mode' markup characters."
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (org-restart-font-lock))

(defun x/fill-subtree ()
  "Toggle fill in current subtree."
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (unfill-toggle)))

(defun x/unfill-subtree ()
  "Unfill current subtree."
  (interactive)
  (org-mark-subtree)
  (call-interactively #'unfill-region))

(defun x/wrap-block (beg end type)
  "Wrap block with TYPE between BEG and END."
  (interactive
   (let ((type (read-string "Block type, default[verse], q[quote]: ")))
     (list (region-beginning) (region-end) type)))

  (setq type
        (cond ((s-blank-str? type) "verse")
              ((s-equals? type "q") "quote")
              (t type)))
  (goto-char end)
  (insert (concat "\n#+end_" type))
  (goto-char beg)
  (insert (concat "#+begin_" type "\n")))

(defun x/org-heading-beginning-p ()
  "Whether the point is at beginning of a heading."
  (and (org-at-heading-p) (= (line-beginning-position) (point))))

(defun x/org-goto-heading-beginning ()
  "Goto beginning of the heading."
  (interactive)
  (org-back-to-heading)
  (if (x/org-heading-beginning-p) (org-beginning-of-line)))

(defun x/org-schedule ()
  "Insert an active timestamp at the beginning of the headline."
  (interactive)
  (save-excursion
    (x/org-goto-heading-beginning)
    (setq ts (call-interactively #'org-time-stamp))
    (x/org-goto-heading-beginning)
    (replace-regexp ">\\([^ ]\\)" "> \\1"))
  ts)

;; (add-hook 'org-mode-hook #'auto-fill-mode)

;; https://emacs-china.org/t/org-mode/26643
(defun x/org-emphasis-markers-for-chinese ()
  ;; 配置 prettify
  (setq prettify-symbols-unprettify-at-point t)
  (defun org-prettify-set ()
    (interactive)
    (setq prettify-symbols-alist
          (mapcan (lambda (x)
                    (list x (cons (upcase (car x)) (cdr x)))) '(
                    ("\u200b" . 8248)   ; 零宽空格
                    )))
    (prettify-symbols-mode 1))
  (add-hook 'org-mode-hook 'org-prettify-set)
  ;; 导出时（导出为 org 时除外），去除零宽空格
  (defun +org-export-remove-zero-width-space (text _backend _info)
    "Remove zero width spaces from TEXT."
    (unless (org-export-derived-backend-p 'org)
      (replace-regexp-in-string "\u200b" "" text)))
  (with-eval-after-load 'ox         ; 没有这一行的话，会因变量未定义而报错。
    (add-to-list 'org-export-filter-final-output-functions #'+org-export-remove-zero-width-space t))

  (add-hook 'org-mode-hook
            (lambda ()
              ;; emphasis字符加上零宽空格
              (defmacro add_zero_space_char (ch)
                `(lambda ()
                   (interactive)
                   ;; r支持region！但注意有些键`easy-mark'会影响到，如=
                   (tempel-insert (list "" "\x200B" ,ch 'r ,ch "\x200B"))))
              (local-set-key "*" (add_zero_space_char "*"))
              (local-set-key "/" (add_zero_space_char "/"))
              (local-set-key "_" (add_zero_space_char "_"))
              (local-set-key "=" (add_zero_space_char "="))
              (local-set-key "~" (add_zero_space_char "~"))
              (local-set-key "+" (add_zero_space_char "+"))
              (local-set-key "`" (add_zero_space_char "`")))))

(with-eval-after-load 'org
  (defface x/org-bold
    '((t (:inherit bold :foreground "#f00056")))
    "Bold face.")

  (add-to-list 'org-emphasis-alist
               ;; set emphasis face
               '("*" x/org-bold))
  (x/org-emphasis-markers-for-chinese)

  (defun x--reset-filling ()
    (let ((paragraph-ending (concat (substring org-element-paragraph-separate 1)
                                    "\\|^\\(#\\+end_.*\\)")))
      (setq-local paragraph-start paragraph-ending)
      (setq-local paragraph-separate paragraph-ending)))
  (advice-add 'org-setup-filling :after #'x--reset-filling)

  (add-hook 'org-mode-hook
            (lambda ()
              ;; Treat the Chinese colon as a word separator
              (modify-syntax-entry ?\uff1a "." (syntax-table))))

  ;; babel
  (add-to-list 'org-babel-load-languages '(shell . t))
  (add-to-list 'org-babel-load-languages '(clojure . t))
  (add-to-list 'org-babel-load-languages '(plantuml . t))
  (add-to-list 'org-babel-load-languages '(restclient . t))
  (add-to-list 'org-babel-load-languages '(swift . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

  ;; https://tecosaur.github.io/emacs-config/config.html#lsp-support-src
  (cl-defmacro lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (setq centaur-lsp 'lsp-mode)
    ;; (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((file-name (->> info caddr (alist-get :file))))
             (unless file-name
               (setq file-name (make-temp-file "babel-lsp-")))
             (setq buffer-file-name file-name)
             (lsp-deferred)))
         (put ',intern-pre 'function-documentation
              (format "Enable lsp-mode in the buffer of org source block (%s)."
                      (upcase ,lang)))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))
  (defvar org-babel-lang-list
    '("ts" "typescript" "bash" "sh" "kotlin"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enable ,lang)))

  ;; follow link in the current window
  (add-to-list 'org-link-frame-setup '(file . find-file)))

;;; utils
(defun x/wiki-with-title ()
  "Goto wiki with the buffer's title."
  (interactive)
  (let ((title (car (cdr (car (org-collect-keywords '("title")))))))
    (browse-url (concat "https://zh.wikipedia.org/wiki/"
                        (url-encode-url title)))))

;;; tags
(setq org-use-fast-tag-selection nil)
(setq org-tag-alist
      `((:startgroup)
       ("Mon"       . ?m)
       ("Tue"       . ?t)
       ("Wed"       . ?w)
       ("Thu"       . ?T)
       ("Fri"       . ?f)
       ("Sat"       . ?s)
       ("Sun"       . ?S)
       (:startgroup)
       (,x/home . ?h)
       (:startgroup)
       ("NightRunning" . ?n)
       ("MorningRunning" . ?r)
       (:startgroup)
       ("Sunny")
       ("MostlySunny")
       ("MostlyClear")
       ("Cloudy")
       ("PartlyCloudy")
       ("MostlyCloudy")
       ("Showers")
       ("Haze")
       ("Thunderstorm")
       ("HeavyRain")
       (:endgroup)))
(defun x/org-set-tags-without-fast-ui ()
  (interactive)
  (org-set-tags-command '(16)))

(require 'x-org-attach)

;;; org modern and faces
(with-eval-after-load 'org-modern
  (setq org-modern-replace-stars '("◉" "○" "◇"))
  (setq org-modern-star 'replace)
  (set-face-attribute 'org-modern-label nil :family "Latin Modern Mono")
  ;; (set-face-attribute 'org-modern-time-active nil :background "gray92")
  (set-face-attribute 'org-modern-date-active nil :background "#c88562" :foreground "white" :distant-foreground "black")
  (set-face-attribute 'org-modern-date-inactive nil :background "#e0ce87" :foreground "white" :distant-foreground "black")
  (set-face-attribute 'org-modern-done nil :background "#ede5c3" :foreground "white" :distant-foreground "black")
  (when (x/theme-light?)
    (set-face-attribute 'org-modern-tag nil
                        :foreground "white"
                        :distant-foreground "black"))

  (defvar x/org-modern-date-large? nil)
  (defun x/org-modern-switch-date-face ()
    "Switch date face."
    (interactive)
    (let ((height (if x/org-modern-date-large? 'unspecified 1.1)))
      (set-face-attribute 'org-modern-date-active nil :height height)
      (setq x/org-modern-date-large? (not x/org-modern-date-large?)))))

(global-org-modern-mode)

;; it will be changed by doom-theme, so we need to set it again
(run-with-idle-timer 0.1 nil
                     (lambda ()
                       (when (x/theme-light?)
                         (set-face-foreground 'org-block-begin-line "gray60"))))

(set-face-attribute 'org-verse nil :slant 'italic)

;;; keybindings
(x/define-keys org-mode-map
               '(("M-n" org-next-visible-heading)
                 ("M-p" org-previous-visible-heading)
                 ("H-S-<return>" org-insert-todo-heading)
                 ([remap org-schedule] x/org-schedule)))

(global-set-key (kbd "C-c l") #'org-store-link)

(x/define-keys org-src-mode-map
               '(("C-c C-c" org-edit-src-exit)))

;;; embark
(with-eval-after-load 'embark
  (add-to-list 'marginalia-prompt-categories '("Go to heading" . consult-org-heading))
  ;; doesn't work without the context
  (defvar-keymap embark-org-heading-map
    :doc "Keymap for embark org heading."
    :parent embark-org-heading-map
    "i" 'org-clock-in
    "r" 'org-refile)
  (add-to-list 'embark-keymap-alist '(consult-org-heading embark-org-heading-map))

  (x/define-keys embark-heading-map
                 '(("i" org-clock-in)
                   ;; something wrong with this
                   ("r" org-refile)
                   ("j" outline-next-visible-heading)
                   ("k" outline-previous-visible-heading)
                   ("h" outline-hide-subtree)
                   ("w" outline-move-subtree-up)
                   ("s" outline-move-subtree-down)
                   ("D" x/duplicate-line)
                   ("c" org-cut-subtree))))

;;; visual line mode
;; see: [[info:emacs#Visual Line Mode][emacs#Visual Line Mode]]
;; (info-apropos "visual-line-mode")
;; for cjk support
(setq word-wrap-by-category t)
(add-hook 'org-mode-hook #'visual-line-mode)

(provide 'x-org)
;;; x-org.el ends here
