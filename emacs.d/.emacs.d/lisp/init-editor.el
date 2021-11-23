;;; init-editor.el --- init-editor -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'mmm-mode)

;; Delete multiple consecutive blank characters at once
(leaf hungry-delete
  :straight t
  :hook
  (activate-mark-hook . (lambda () (hungry-delete-mode -1)))
  (deactivate-mark-hook . hungry-delete-mode)
  (after-init-hook . global-hungry-delete-mode))

(leaf easy-kill
  :straight t
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp]      . easy-mark))

(leaf jka-cmpr
  :hook (after-init-hook . auto-compression-mode))

(straight-register-package
 '(auto-save :host github
             :repo "manateelazycat/auto-save"))
(leaf auto-save
  :straight t
  :require t
  :custom
  (auto-save-silent                     . t)
  (auto-save-delete-trailing-whitespace . t)
  :hook
  (org-capture-mode-hook             . auto-save-disable)
  (org-capture-prepare-finalize-hook . auto-save-enable)
  (after-init-hook                   . auto-save-enable))

(leaf eldoc-box
  :straight t
  :hook (eldoc-mode-hook . eldoc-box-hover-at-point-mode))

(leaf eldoc-overlay
  :disabled t
  :hook eldoc-mode-hook)

(leaf expand-region
  :straight t
  :bind
  ("C-;" . er/expand-region))

(leaf unfill
  :straight t
  :bind
  ("M-q" . unfill-toggle))


(leaf visual-fill-column
  :straight t
  :commands maybe-adjust-visual-fill-column
  :hook
  ((visual-line-mode-hook visual-fill-column-mode-hook) . maybe-adjust-visual-fill-column)
  :config
  (defun maybe-adjust-visual-fill-column nil
    "Readjust visual fill column when the global font size is modified.\nThis is helpful for writeroom-mode, in particular."
    (if visual-fill-column-mode
        (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
      (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t))))

(leaf page-break-lines
  :straight t
  :hook (after-init-hook . global-page-break-lines-mode)
  :custom (page-break-lines-max-width . 80)
  :defer-config
  (when (fboundp 'diminish)
    (diminish 'page-break-lines-mode)))

(leaf hl-line
  :hook (after-init-hook . global-hl-line-mode))


(leaf color-identifiers-mode
  :straight t
  :hook prog-mode-hook)

(leaf indent-tools
  :straight t
  :bind ("C-c TAB" . indent-tools-hydra/body))

(leaf highlight-indent-guides
  :straight t
  :hook prog-mode-hook
  :custom
  (highlight-indent-guides-responsive . nil)
  (highlight-indent-guides-delay      . 0.5))

(leaf rainbow-mode
  :straight t
  :hook (after-init-hook
         text-mode-hook
         org-mode-hook
         css-mode-hook
         html-mode-hook
         prog-mode-hook)
  :defer-config
  (when (fboundp 'diminish)
    (diminish 'rainbow-mode)))

(leaf rainbow-identifiers
  :straight t
  :hook (after-init-hook
         text-mode-hook
         org-mode-hook
         css-mode-hook
         html-mode-hook
         prog-mode-hook)
  :defer-config
  (when (fboundp 'diminish)
    (diminish 'rainbow-identifiers-mode)))

(leaf rainbow-delimiters
  :straight t
  :hook prog-mode-hook org-src-mode-hook)

(leaf htmlize
  :straight t
  :custom (htmlize-pre-style . t))

(leaf mmm-auto
  :custom
  (mmm-global-mode . 'buffers-with-submode-classes)
  (mmm-submode-decoration-level . 2))

(leaf paren
  :hook (after-init-hook . show-paren-mode))

(leaf smartparens
  :straight t
  :bind* ("C-c s" . hydra-sp/body)
  :hook (after-init-hook . smartparens-global-mode)
  :custom (sp-hybrid-kill-entire-symbol . nil)
  :hydra
  (hydra-sp
   (:hint nil)
   "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_J_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
   ;; Moving
   ("a" sp-beginning-of-sexp)
   ("e" sp-end-of-sexp)
   ("f" sp-forward-sexp)
   ("b" sp-backward-sexp)
   ("n" sp-down-sexp)
   ("N" sp-backward-down-sexp)
   ("p" sp-up-sexp)
   ("P" sp-backward-up-sexp)

   ;; Slurping & barfing
   ("h" sp-backward-slurp-sexp)
   ("H" sp-backward-barf-sexp)
   ("l" sp-forward-slurp-sexp)
   ("L" sp-forward-barf-sexp)

   ;; Wrapping
   ("R" sp-rewrap-sexp)
   ("u" sp-unwrap-sexp)
   ("U" sp-backward-unwrap-sexp)
   ("(" sp-wrap-round)
   ("{" sp-wrap-curly)
   ("[" sp-wrap-square)

   ;; Sexp juggling
   ("S" sp-split-sexp)
   ("s" sp-splice-sexp)
   ("r" sp-raise-sexp)
   ("J" sp-join-sexp)
   ("t" sp-transpose-sexp)
   ("A" sp-absorb-sexp)
   ("E" sp-emit-sexp)
   ("o" sp-convolute-sexp)

   ;; Destructive editing
   ("c" sp-change-inner :exit t)
   ("C" sp-change-enclosing :exit t)
   ("k" sp-kill-sexp)
   ("K" sp-backward-kill-sexp)
   ("w" sp-copy-sexp)

   ("/" undo)
   ("j" xr/ace-pinyin-goto-word-1)
   ("y" yank)

   ("q" nil)
   ("g" nil)))

;; https://stackoverflow.com/questions/22107501/set-emacs-to-smart-auto-line-after-a-parentheses-pair
(with-eval-after-load 'smartparens
  (sp-with-modes '(js-mode web-mode kotlin-mode css-mode typescript-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(leaf quick-peek
  :straight t
  :custom-face
  (quick-peek-border-face  . '((t (:background "#75b79e" :height 0.1))))
  (quick-peek-padding-face . '((t (:height 0.1)))))

(leaf subword
  :hook prog-mode-hook)

(leaf symbol-overlay
  :straight t
  :bind (("M-i"  . symbol-overlay-put)
         ("M-n"  . symbol-overlay-switch-forward)
         ("M-p"  . symbol-overlay-switch-backward)
         ("<f8>" . symbol-overlay-remove-all)
         ("<f7>" . symbol-overlay-mode)))

(leaf uniquify
  :custom
  (uniquify-buffer-name-style   . 'reverse)
  (uniquify-separator           . " • ")
  (uniquify-after-kill-buffer-p . t)
  (uniquify-ignore-buffers-re   . "^\\*"))

(leaf writeroom-mode
  :preface
  (define-minor-mode prose-mode
    "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
    nil " Prose" nil
    (if prose-mode
        (progn
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 1))
          (setq truncate-lines nil)
          (setq word-wrap t)
          (setq cursor-type 'bar)
          (when (eq major-mode 'org)
            (kill-local-variable 'buffer-face-mode-face))
          (buffer-face-mode 1)
          ;;(delete-selection-mode 1)
          (set (make-local-variable 'blink-cursor-interval) 0.6)
          (set (make-local-variable 'show-trailing-whitespace) nil)
          (set (make-local-variable 'line-spacing) 0.2)
          (set (make-local-variable 'electric-pair-mode) nil)
          (ignore-errors (flyspell-mode 1))
          (visual-line-mode 1))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      ;; (delete-selection-mode -1)
      (flyspell-mode -1)
      (visual-line-mode -1)
      (when (fboundp 'writeroom-mode)
        (writeroom-mode 0)))))

(provide 'init-editor)
;;; init-editor.el ends here
