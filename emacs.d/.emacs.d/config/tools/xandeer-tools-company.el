;;; xandeer-tools-company.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Company Mode.

;;; Code:

(straight-use-package 'company)
(leaf company
  :hook prog-mode-hook
  :bind
  ("M-/"     . company-files)
  ("M-C-/"   . xandeer/company-complete)
  ("C-<tab>" . xandeer/company-complete)
  (:company-mode-map
  ("M-/" . company-tabnine))
  (:company-active-map
   ("M-/" . company-other-backend)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  :custom ((company-tooltip-limit       . 10)
           (company-dabbrev-downcase    . nil)
           (company-dabbrev-ignore-case . t)
           (company-global-modes
            . '(not erc-mode message-mode help-mode gud-mode eshell-mode))
           `(company-frontends
             . ,(if *c-box*
                    '(company-box-frontend)
                  '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)))
           (company-dabbrev-other-buffers     . 'all)
           (company-tooltip-align-annotations . t)
           (company-minimum-prefix-length     . 2)
           (company-idle-delay                . .2)
           (company-tooltip-idle-delay        . .2)
           (company-require-match             . 'never))
  :config
  (add-hook 'after-change-major-mode-hook #'xandeer/company-init-backends-h 'append)
  :defer-config
  (add-to-list 'completion-styles 'initials t)
  (setq company-backends '(company-capf))
  (defvar xandeer/prev-whitespace-mode nil)
  (make-variable-buffer-local 'xandeer/prev-whitespace-mode)
  (defvar xandeer/show-trailing-whitespace nil)
  (make-variable-buffer-local 'xandeer/show-trailing-whitespace)
  (global-company-mode)
  (defun pre-popup-draw ()
    "Turn off whitespace mode before showing company complete tooltip"
    (if whitespace-mode
        (progn
          (gsetq my-prev-whitespace-mode t)
          (whitespace-mode -1)))
    (gsetq xandeer/show-trailing-whitespace show-trailing-whitespace)
    (gsetq show-trailing-whitespace nil))
  (defun post-popup-draw ()
    "Restore previous whitespace mode after showing company tooltip"
    (if xandeer/prev-whitespace-mode
        (progn
          (whitespace-mode 1)
          (gsetq xandeer/prev-whitespace-mode nil)))
    (gsetq show-trailing-whitespace xandeer/show-trailing-whitespace))
  :advice
  (:before company-pseudo-tooltip-unhide pre-popup-draw)
  (:after  company-pseudo-tooltip-hide   post-popup-draw))

(straight-use-package 'company-prescient)
(leaf company-prescient
  :hook company-mode-hook)

(straight-use-package 'company-quickhelp)
(leaf company-quickhelp
  :when (not *c-box*)
  :bind
  (:company-active-map
   ("C-c h" . company-quickhelp-manual-begin))
  :hook company-mode-hook
  :custom
  (pos-tip-use-relative-coordinates . t))

(straight-use-package 'company-tabnine)
(leaf company-tabnine
  :after company
  :require t
  :config
  (add-to-list 'company-backends #'company-tabnine)
  ;; The free version of TabNine is good enough,
  ;; and below code is recommended that TabNine not always
  ;; prompt me to purchase a paid version in a large project.
  (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
    (let ((company-message-func (ad-get-arg 0)))
      (when (and company-message-func
                 (stringp (funcall company-message-func)))
        (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
          ad-do-it))))
  :custom
  `(company-tabnine-log-file-path
    . ,(concat company-tabnine-binaries-folder "/log")))

(straight-use-package 'company-flx)
(leaf company-flx
  :hook company-mode-hook)

(xandeer/s-u-p
  (:when *c-box* company-box))
(leaf company-box
  :when *c-box*
  :hook company-mode-hook
  :custom
  (company-box-show-single-candidate . t)
  (company-box-max-candidates        . 25)
  (company-box-icons-alist           . 'company-box-icons-all-the-icons)
  :config
  (gsetq
   company-box-icons-functions
   (cons #'xandeer/company-box-icons--elisp-fn
         (delq 'company-box-icons--elisp
               company-box-icons-functions)))

  (defun xandeer/company-box-icons--elisp-fn (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp  sym) 'ElispFunction)
              ((boundp   sym) 'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep    sym) 'ElispFace)))))

  (after-x 'all-the-icons
    (gsetq
     company-box-icons-all-the-icons
     (let ((all-the-icons-scale-factor 0.8))
       `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
         (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
         (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-yellow))
         (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-yellow))
         (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-yellow))
         (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-yellow))
         (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
         (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-cyan))
         (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-cyan))
         (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-cyan))
         (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-lorange))
         (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
         (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
         (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-lorange))
         (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-lgreen))
         (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-lblue))
         (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-green))
         (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-green))
         (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-silver))
         (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-green))
         (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-lorange))
         (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-blue))
         (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-blue))
         (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-yellow))
         (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
         (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
         (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
         (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
         (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
         (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
         (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink))))))

  (defun xandeer/company-remove-scrollbar-a (orig-fn &rest args)
   "This disables the company-box scrollbar, because:
  https://github.com/sebastiencs/company-box/issues/44"
   (cl-letf (((symbol-function #'display-buffer-in-side-window)
              (symbol-function #'ignore)))
     (apply orig-fn args)))

  :advice (:around
           company-box--update-scrollbar
           xandeer/company-remove-scrollbar-a))

(provide 'xandeer-tools-company)
;;; xandeer-tools-company.el ends here
