;;; x-embark.el --- x-embark -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'marginalia)
(x/append-init-hook #'marginalia-mode)

(require 'embark)

;; Hide the mode line of the Embark live/completions buffers
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

;; (add-to-list 'embark-keymap-alist '(org-roam-node . embark-roam-map))
;; (setq embark-keymap-alist (-remove-at 0 embark-keymap-alist))
;; (add-to-list 'marginalia-prompt-categories '("Find file: " . file))

(autoload #'consult-ripgrep "consult")
(autoload #'consult-line "consult")
(autoload #'consult-imenu "consult")
(autoload #'consult-outline "consult")
(autoload #'consult-recent-file "consult")

(with-eval-after-load 'consult
  (require 'embark-consult))

(with-eval-after-load 'grep
  (x/define-keys
   grep-mode-map
   '(("i" wgrep-change-to-wgrep-mode))))

(defgroup x/embark ()
  "Extensions for `embark`."
  :group 'editing)

(defvar x/embark-become-general-map
  (let ((map (make-sparse-keymap)))
    (x/define-keys map '(("f" consult-find)
                         ("g" consult-ripgrep)))
    map)
  "General custom cross-package `embark-become` keymap.")

(defvar x/embark-become-line-map
  (let ((map (make-sparse-keymap)))
    (x/define-keys map '(("l" consult-line)
                         ("i" consult-imenu)
                         ("s" consult-outline))) ; as my default is 'M-s M-s'
    map)
  "Line-specific custom cross-package `embark-become' keymap.")

(defvar embark-become-file+buffer-map)

(defvar x/embark-become-file+buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-become-file+buffer-map)
    (x/define-keys map '(("r" consult-recent-file)
                         ;; ("B" project-switch-to-buffer)
                         ("F" projectile-find-file)
                         ("O" x/find-file-external)
                         ("P" x/projectile-find-file-external)))
    map)
  "File+buffer custom cross-package `embark-become' keymap.")

(defvar embark-become-keymaps)

;;;###autoload
(define-minor-mode x/embark-keymaps
  "Add or remove keymaps from Embark.
This is based on the value of `x/embark-add-keymaps'
and is meant to keep things clean in case I ever wish to disable
those so-called 'extras'."
  :init-value nil
  :global t
  (let ((maps (list 'x/embark-become-general-map
                    'x/embark-become-line-map
                    'x/embark-become-file+buffer-map)))
    (if x/embark-keymaps
        (dolist (map maps)
          (cl-pushnew map embark-become-keymaps))
      (setq embark-become-keymaps
            (dolist (map maps)
              (delete map embark-become-keymaps))))))
(x/embark-keymaps 1)

(autoload 'embark-act "embark")
(autoload 'embark-act-noexit "embark")
(autoload 'embark-become "embark")

(global-set-key (kbd "H-l") #'embark-act)

(with-eval-after-load 'ace-window
  (defun x/wrap-embark-ace ()
    "Switch window before running default command."
    (interactive)
    (with-demoted-errors "%s"
      (aw-switch-to-window (aw-select nil))
      (call-interactively embark--command)))

  (defun x/wrap-embark-window-previous ()
    "Switch to the previous window before runing default command."
    (interactive)
    (with-demoted-errors "%s"
      (other-window -1)
      (call-interactively embark--command)))

  (defun x/wrap-embark-window-next ()
    "Switch to the next window before runing default command."
    (interactive)
    (with-demoted-errors "%s"
      (other-window 1)
      (call-interactively embark--command)))

  (mapc (lambda (map)
          (x/define-keys map '(("o"   x/wrap-embark-ace)
                               ("C-u" x/wrap-embark-window-previous)
                               ("C-i" x/wrap-embark-window-next))))
        (list embark-general-map
              embark-buffer-map
              embark-file-map
              embark-bookmark-map)))

(defun x/wrap-embark-split-right ()
  "Split window right before running default command."
  (interactive)
  (with-demoted-errors "%s"
    (select-window (split-window-right))
    (call-interactively embark--command)))

(defun x/wrap-embark-split-below ()
  "Split window below before running default command."
  (interactive)
  (with-demoted-errors "%s"
    (select-window (split-window-below))
    (call-interactively embark--command)))

(defun x/wrap-embark-new-frame ()
  "Make a new frame before running default command."
  (interactive)
  (with-demoted-errors "%s"
    (make-frame)
    (call-interactively embark--command)))

(x/define-keys embark-general-map '(("2" x/wrap-embark-split-below)
                                    ("3" x/wrap-embark-split-right)
                                    ("5" x/wrap-embark-new-frame)
                                    ("." sdcv-search-pointer+)))

(x/define-keys embark-symbol-map '(("." sdcv-search-pointer+)
                                   ("h" helpful-at-point)))
(x/define-keys embark-command-map '(("." sdcv-search-pointer+)
                                    ("h" helpful-at-point)))

;; (global-set-key (kbd "H-i") 'embark-act)
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-o") #'embark-act))

(defun x/wrap-default-p (action)
  (s-starts-with? "x/wrap-embark" (symbol-name action)))

(defun embark-act (&optional arg)
  "Prompt the user for an action and perform it.
The targets of the action are chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate.  When called from a non-minibuffer buffer
there can multiple targets and you can cycle among them by using
`embark-cycle' (which is bound by default to the same key
binding `embark-act' is, but see `embark-cycle-key').

This command uses `embark-prompter' to ask the user to specify an
action, and calls it injecting the target at the first minibuffer
prompt.

If you call this from the minibuffer, it can optionally quit the
minibuffer.  The variable `embark-quit-after-action' controls
whether calling `embark-act' with nil ARG quits the minibuffer,
and if ARG is non-nil it will do the opposite.  Interactively,
ARG is the prefix argument.

If instead you call this from outside the minibuffer, the first
ARG targets are skipped over (if ARG is negative the skipping is
done by cycling backwards) and cycling starts from the following
target."
  (interactive "P")
  (let* ((targets (or (embark--targets) (user-error "No target found")))
         (indicators (mapcar #'funcall embark-indicators))
         (default-done nil))
    (when (and arg (not (minibufferp)))
      (setq targets (embark--rotate targets (prefix-numeric-value arg))))
    (unwind-protect
        (while
            (let* ((target (car targets))
                   (action
                    (or (embark--prompt
                         indicators
                         (let ((embark-default-action-overrides
                                (if default-done
                                    `((t . ,default-done))
                                  embark-default-action-overrides)))
                           (embark--action-keymap (plist-get target :type)
                                                  (cdr targets)))
                         targets)
                        (progn
                          (message "%s" target)
                          (message "copied: %s" (plist-put
                                                 (plist-put
                                                  (copy-sequence target)
                                                  :target (plist-get target :orig-target))
                                                 :type (plist-get target :orig-type)))
                          (user-error "Canceled"))))
                   (default-action (or default-done
                                       (embark--default-action
                                        (plist-get target :type)))))
              (cond
               ;; When acting twice in the minibuffer, do not restart
               ;; `embark-act'.  Otherwise the next `embark-act' will
               ;; find a target in the original buffer.
               ((eq action #'embark-act)
                (message "Press an action key"))
               ((eq action #'embark-cycle)
                (setq targets (embark--rotate
                               targets (prefix-numeric-value prefix-arg))))
               (t
                ;; if the action is non-repeatable, cleanup indicator now
                (let ((repeat (memq action embark-repeat-actions)))
                  (unless repeat (mapc #'funcall indicators))
                  (condition-case err
                      (embark--act
                       action
                       (if (or (x/wrap-default-p action)
                               (and (eq action default-action)
                                    (eq action embark--command)))
                           (plist-put
                            (plist-put
                             (copy-sequence target)
                             :target (plist-get target :orig-target))
                            :type (plist-get target :orig-type))
                         target)
                       (if embark-quit-after-action (not arg) arg))
                    (user-error
                     (funcall (if repeat #'message #'user-error)
                              "%s" (cadr err))))
                  (when-let (new-targets (and repeat (embark--targets)))
                    ;; Terminate repeated prompter on default action,
                    ;; when repeating. Jump to the region type if the
                    ;; region is active after the action, or else to the
                    ;; current type again.
                    (setq default-done #'embark-done
                          targets
                          (embark--rotate
                           new-targets
                           (or (cl-position-if
                                (let ((desired-type
                                       (if (eq action 'mark)
                                           'region
                                         (plist-get (car targets) :type))))
                                  (lambda (x)
                                    (eq (plist-get x :type) desired-type)))
                                new-targets)
                               0)))))))))
      (mapc #'funcall indicators))))

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))
(setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(defun embark-vertico-indicator ()
  (let ((fr face-remapping-alist))
    (lambda (&optional keymap _targets prefix)
      (when (bound-and-true-p vertico--input)
        (setq-local face-remapping-alist
                    (if keymap
                        (cons '(vertico-current . embark-target) fr)
                      fr))))))

(add-to-list 'embark-indicators #'embark-vertico-indicator)

(provide 'x-embark)
;;; x-embark.el ends here
