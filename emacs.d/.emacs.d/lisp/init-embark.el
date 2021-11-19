;;; init-embark.el --- init-embark -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'marginalia)
(add-hook 'after-init-hook 'marginalia-mode)

(require-package 'embark)
(require 'embark)

(with-eval-after-load 'marginalia
  (with-eval-after-load 'embark
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))

    ;; (add-to-list 'embark-keymap-alist '(org-roam-node . embark-roam-map))
    ;; (setq embark-keymap-alist (-remove-at 0 embark-keymap-alist))
    ;; (add-to-list 'marginalia-prompt-categories '("Find file: " . file))

    (with-eval-after-load 'consult
      (require-package 'embark-consult)
      (require 'embark-consult))

    (with-eval-after-load 'ace-window
      (defun xr/wrap-embark-ace ()
        "Switch window before running default command."
        (interactive)
        (with-demoted-errors "%s"
          (aw-switch-to-window (aw-select nil))
          (call-interactively embark--command)))

      (define-key embark-general-map (kbd "o") 'xr/wrap-embark-ace))

    (defun xr/wrap-embark-split-right ()
      "Split window right before running default command."
      (interactive)
      (with-demoted-errors "%s"
        (select-window (split-window-right))
        (call-interactively embark--command)))

    (defun xr/wrap-embark-split-below ()
      "Split window below before running default command."
      (interactive)
      (with-demoted-errors "%s"
        (select-window (split-window-below))
        (call-interactively embark--command)))

    (define-key embark-general-map (kbd "2") 'xr/wrap-embark-split-below)
    (define-key embark-general-map (kbd "3") 'xr/wrap-embark-split-right)

  ;; (global-set-key (kbd "H-i") 'embark-act)
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "C-o") 'embark-act))

  (defun xr/wrap-default-p (action)
    (s-starts-with? "xr/wrap-embark" (symbol-name action)))

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
                       (if (or (xr/wrap-default-p action)
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
      (mapc #'funcall indicators))))))

(provide 'init-embark)
;;; init-embark.el ends here
