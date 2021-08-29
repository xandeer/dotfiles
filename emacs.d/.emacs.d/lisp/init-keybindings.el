;;; init-keybindings.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf which-key
  :straight t
  :custom
  (which-key-allow-imprecise-window-fit . nil)
  :hook after-init-hook
  :custom
  (mac-option-modifier        . 'meta)
  (mac-command-modifier       . 'hyper)
  (mac-right-command-modifier . 'hyper)
  (mac-function-modifier      . 'super)
  :init
  (defun xr/switch-to-last-buffer ()
    (interactive)
    (switch-to-buffer (other-buffer)))
  :bind*
  ([remap kill-buffer]            . kill-current-buffer)
  ([remap comment-dwim]           . comment-line)
  :bind
  ([remap move-beginning-of-line] . xr/smart-beginning-of-line)
  ([remap newline]                . newline-and-indent)

  ("M-[" . xr/switch-to-last-buffer)
  ("C-z" . ns-next-frame)

  ("H-<up>"   . beginning-of-buffer)
  ("H-<down>" . end-of-buffer)
  ("H-l"      . goto-line)

  ;; text Operations
  ("H-a" . mark-whole-buffer)
  ("H-c" . kill-ring-save)
  ("H-d" . xr/duplicate-line)
  ("H-s" . save-buffer)
  ("H-v" . yank)

  ("H-z" . undo)

  ("H-n" . make-frame)
  ("H-e" . xr/kill-other-window-buffer))

(leaf keyfreq
  :straight t
  :require t
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          org-self-insert-command
          disable-mouse--handle
          forward-char
          backward-char
          previous-line
          next-line
          newline-and-indent))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  :bind
  ("H-j" . hydra-fv/body)
  :hydra
  (hydra-fv
   (:hint nil :exit t)
   "
   Agent: _a_gent default _s_elect
      Xr: _xd_elete current buffer _xl_ remove links _xc_convert quotations
          _xf_ill subtree _xj_ insert journal in year _xm_igirage journal
Bookmark: _be_motion _bg_et up
  Others: _bo_okmark
  Cancel: _q_
"
   ("a" org-agenda-list)
   ("s" org-agenda)
   ("bo" counsel-bookmark)
   ("be" (xr/bookmark "emotion"))
   ("bg" (xr/bookmark "get_up"))
   ("xc" xr/convert-chinese-quotations)
   ("xd" xr/delete-current-buffer)
   ("xf" xr/fill-subtree)
   ("xl" xr/remove-links)
   ("xj" xr/insert-journal-in-year)
   ("xm" xr/migrate-journal)
   ("q" nil)))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
