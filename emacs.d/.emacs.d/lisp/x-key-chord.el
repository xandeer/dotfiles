;;; x-key-chord.el --- x-key-chord -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'key-chord)

(setq key-chord-one-key-delay 0.18)
(add-hook 'after-init-hook (lambda () (key-chord-mode 1)))

(key-chord-define-global "jj" 'x/ace-goto-char-timer)
(key-chord-define-global "jh" 'consult-outline)
(key-chord-define-global "kk" 'x/switch-to-last-buffer)
(key-chord-define-global "hh" 'org-agenda-list)
(key-chord-define-global "ii" 'toggle-input-method)
(key-chord-define-global "jl" 'link-hint-open-link)
(key-chord-define-global "ww" 'ace-window)
(key-chord-define-global "dd" 'hippie-expand)
(key-chord-define-global "bb" 'consult-buffer)
(key-chord-define-global "jr" 'consult-recent-file)
(key-chord-define-global "jf" 'find-file)
(key-chord-define-global "jt" 'org-roam-dailies-goto-today)
(key-chord-define-global "jy" 'org-roam-dailies-goto-yesterday)
(key-chord-define org-mode-map "kk" 'org-mark-ring-goto)
(key-chord-define org-mode-map "nn" 'org-toggle-narrow-to-subtree)
(key-chord-define org-agenda-mode-map "jj" 'consult-line)

;; https://emacs.stackexchange.com/questions/2105/how-do-i-disable-key-chord-mode-in-the-minibuffer
(defun disable-key-chord-mode ()
  (set (make-local-variable 'input-method-function) nil))

(add-hook 'minibuffer-setup-hook #'disable-key-chord-mode)

(provide 'x-key-chord)
;;; x-key-chord.el ends here