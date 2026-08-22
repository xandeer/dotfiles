;;; x-rime.el --- Xandeer's Emacs Configuration tools rime file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Editor Rime.

;;; Code:

(defconst x/rime-translate-keys
  '("C-f" "C-b" "C-n" "C-p" "C-d" "C-h" "C-a" "C-e" "C-g" "C-k" "C-c" "C-v" "M-v" "M-a" "M-e" "M-b" "M-f" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "C-`" "C-i" "C-j" "C-l")
  "Keys that should be passed through Rime to Emacs.")

(with-eval-after-load 'rime
  (require 'x-rime-ai)
  (x/rime-ai-install)

  (setq rime--module-path
        (expand-file-name (concat "var/rime/librime-emacs" module-file-suffix)
                          user-emacs-directory))

  (defun x/rime-compile-module ()
    "Build the patched Rime module outside the Straight checkout."
    (interactive)
    (let ((script (expand-file-name "libexec/build-emacs-rime-module.zsh"
                                    user-emacs-directory))
          (process-environment (copy-sequence process-environment)))
      (unless (file-executable-p script)
        (error "Emacs Rime build script is not executable: %s" script))
      (unless module-file-suffix
        (error "Variable `module-file-suffix' is nil"))
      (setenv "EMACS_RIME_SOURCE" (expand-file-name rime--root))
      (setenv "EMACS_RIME_MODULE_DIR" (file-name-directory rime--module-path))
      (setenv "LIBRIME_ROOT"
              "/Library/Input Methods/Squirrel.app/Contents/Frameworks")
      (setenv "LIBRIME_HEADER_ROOT"
              (and rime-librime-root (expand-file-name rime-librime-root)))
      (setenv "EMACS_MODULE_HEADER_ROOT"
              (and rime-emacs-module-header-root
                   (expand-file-name rime-emacs-module-header-root)))
      (setenv "MODULE_FILE_SUFFIX" module-file-suffix)
      (unless (zerop (call-process script nil "*Compile Emacs Rime*" t))
        (error "Compile patched Rime dynamic module failed"))
      (message "Compile patched Rime dynamic module succeeded")))

  (unless (advice-member-p #'x/rime-compile-module 'rime-compile-module)
    (advice-add 'rime-compile-module :override #'x/rime-compile-module))

  (defun x/rime-vterm-translate-p ()
    "Check if current key should be translated in vterm."
    (and (derived-mode-p 'vterm-mode)
         (not (and rime--preedit-overlay (overlay-buffer rime--preedit-overlay)))
         (let* ((keys (this-command-keys))
                (desc (key-description keys))
                (translate-keys (mapcar (lambda (k) (key-description (kbd k)))
                                        x/rime-translate-keys)))
           (member desc translate-keys))))

  (setq rime-user-data-dir "~/.cache/rime")
  (setq rime-librime-root "~/syncthing/personal/configs/librime")
  (setq rime-emacs-module-header-root "/opt/homebrew/opt/emacs-plus@30/include/")
  (setq rime-show-candidate 'posframe)
  (setq rime-show-preedit t)
  (setq rime-cursor "˰")
  (setq rime-posframe-style 'vertical)
  ;; (setq rime-inline-ascii-trigger 'shift-l)
  (setq rime-translate-keybindings x/rime-translate-keys)

  ;; face
  ;; (set-face-attribute 'rime-default-face nil :foreground "#363737" :background "#e5dfb0")

  (defun x/rime--telega-msg-p ()
    (and (fboundp 'telega-msg-at) (telega-msg-at)))

  (setq rime-disable-predicates
        '(rime-predicate-after-alphabet-char-p
          rime-predicate-current-uppercase-letter-p
          meow-normal-mode-p
          x/ace-mode-p
          x/tbl-mode-p
          ;; x-point-speed-activate-p
          x/org-heading-beginning-p
          rime-predicate-hydra-p
          rime-predicate-ace-window-p
          rime-predicate-prog-in-code-p
          rime-predicate-space-after-cc-p
          x/rime-vterm-translate-p
          x/rime--telega-msg-p))

  (defun x/rime-return ()
    "Commit raw input through Rime so shared processors run."
    (interactive)
    (if rime-return-insert-raw
        (let ((last-input-event 'return))
          (rime-send-keybinding))
      (rime--commit-preview)))

  (define-key rime-mode-map (kbd "M-I") 'rime-force-enable)
  (define-key rime-active-mode-map (kbd "M-i") 'rime-inline-ascii)
  (define-key rime-active-mode-map (kbd "RET") #'x/rime-return)
  (define-key rime-active-mode-map (kbd "<return>") #'x/rime-return))

(global-set-key (kbd "M-i") 'toggle-input-method)

(setq default-input-method "rime")

(defcustom x/rime-ignored-adim-buffers
  '("COMMIT_EDITMSG"
    "CAPTURE-inbox.org"
    "inbox.org")
  "When `buffer-name is contained in it, don't active default input method.")

(defun x/rime-ignored-adim-buffer-p ()
  "Predicate for checking `x/rime-ignored-adim-buffers."
  (member (buffer-name) x/rime-ignored-adim-buffers))

(defvar x/rime-ignored-adim-preds
  '(x/rime-ignored-adim-buffer-p
    x/tbl-mode-p)
  "Predicates for ignoring to active default input method.")

(defun x/activate-default-input-method ()
  (unless (-any? (lambda (p) (funcall p)) x/rime-ignored-adim-preds)
    (activate-input-method default-input-method)))

(add-hook 'text-mode-hook #'x/activate-default-input-method)
(add-hook 'telega-chat-mode-hook #'x/activate-default-input-method)

(defun x/switch-cursor-background ()
  "Switch cursor background whether `rime-mode' active and not inline ascii mode."
  (let ((indicator (rime-lighter)))
    (set-face-background
     'cursor
     (if (and (not (string-empty-p indicator))
              (equal (cadar (cddar (object-intervals indicator))) 'rime-indicator-face))
         "#f48225"
       "#51afef"))))

(add-hook 'post-command-hook #'x/switch-cursor-background)

(defvar x/last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(make-variable-buffer-local 'x/last-post-command-position)
(defvar x/cursor-move-hook nil
  "Holds the cursor move hook from the last run of `post-command-hook'.")

(defun x/do-stuff-if-moved-post-command ()
  (unless (equal (point) x/last-post-command-position)
    (run-hooks 'x/cursor-move-hook))
  (setq x/last-post-command-position (point)))

;; (add-hook 'post-command-hook #'x/do-stuff-if-moved-post-command)
;; (remove-hook 'post-command-hook #'x/do-stuff-if-moved-post-command)
;; (add-hook 'x/cursor-move-hook #'x/switch-cursor-background)

;; (x/package-use 'sis)
;; (setq sis-external-ism "im-select")
;; (sis-ism-lazyman-config
;;  "com.apple.keylayout.ABC"
;;  "com.apple.inputmethod.SCIM.Shuangpin")
;; (sis-global-cursor-color-mode t)
;; (sis-global-respect-mode t)
;; (sis-global-context-mode t)
;; (sis-global-inline-mode t)

(provide 'x-rime)
;;; x-rime.el ends here
