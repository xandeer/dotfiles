;;; x-editor.el --- x-editor -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'mmm-mode)
(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)

;; Delete multiple consecutive blank characters at once
(require-package 'hungry-delete)
(add-hook 'activate-mark-hook (lambda () (hungry-delete-mode -1)))
(add-hook 'deactivate-mark-hook 'hungry-delete-mode)
(add-hook 'after-init-hook 'global-hungry-delete-mode)

(require-package 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

(unless
    (fboundp 'auto-compression-mode)
  (autoload #'auto-compression-mode "jka-cmpr" nil t))
(add-hook 'after-init-hook #'auto-compression-mode)

(straight-register-package
 '(auto-save :host github
             :repo "manateelazycat/auto-save"))
(require-package 'auto-save)
(require 'auto-save)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)
(add-hook 'org-capture-mode-hook 'auto-save-disable)
(add-hook 'org-capture-prepare-finalize-hook 'auto-save-enable)
(add-hook 'after-init-hook 'auto-save-enable)

(require-package 'eldoc-box)
(add-hook 'eldoc-mode-hook 'eldoc-box-hover-at-point-mode)

(require-package 'expand-region)
(global-set-key (kbd "C-;") #'er/expand-region)

(require-package 'unfill)
(global-set-key (kbd "M-q") 'unfill-toggle)


(require-package 'visual-fill-column)
(with-eval-after-load 'visual-fill-column
  (defun maybe-adjust-visual-fill-column nil "Readjust visual fill column when the global font size is modified.\nThis is helpful for writeroom-mode, in particular."
         (if visual-fill-column-mode
             (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
           (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t))))

(require-package 'page-break-lines)
(add-hook 'after-init-hook #'global-page-break-lines-mode)
(setq page-break-lines-max-width 80)
(with-eval-after-load 'page-break-lines
  (when
      (fboundp 'diminish)
    (diminish 'page-break-lines-mode)))

(add-hook 'after-init-hook #'global-hl-line-mode)


(require-package 'color-identifiers-mode)
(add-hook 'prog-mode-hook #'color-identifiers-mode)

(unless
    (fboundp 'indent-tools-hydra/body)
  (autoload #'indent-tools-hydra/body "indent-tools" nil t))
(require-package 'indent-tools)
(global-set-key (kbd "C-c TAB") 'indent-tools-hydra/body)

(require-package 'highlight-indent-guides)
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
(setq highlight-indent-guides-responsive nil)
(setq highlight-indent-guides-delay 0.5)

(require-package 'rainbow-mode)
(add-hook 'after-init-hook #'rainbow-mode)
(add-hook 'text-mode-hook #'rainbow-mode)
(add-hook 'org-mode-hook #'rainbow-mode)
(add-hook 'css-mode-hook #'rainbow-mode)
(add-hook 'html-mode-hook #'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)
(with-eval-after-load 'rainbow-mode
  (when (fboundp 'diminish)
    (diminish 'rainbow-mode)))

(require-package 'rainbow-identifiers)
(add-hook 'after-init-hook #'rainbow-identifiers-mode)
(add-hook 'text-mode-hook #'rainbow-identifiers-mode)
(add-hook 'org-mode-hook #'rainbow-identifiers-mode)
(add-hook 'css-mode-hook #'rainbow-identifiers-mode)
(add-hook 'html-mode-hook #'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
(with-eval-after-load 'rainbow-identifiers
  (when (fboundp 'diminish)
    (diminish 'rainbow-identifiers-mode)))

(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-src-mode-hook #'rainbow-delimiters-mode)

(require-package 'htmlize)
(setq htmlize-pre-style t)

(add-hook 'after-init-hook #'show-paren-mode)

(require-package 'smartparens)

(add-hook 'after-init-hook #'smartparens-global-mode)
(setq sp-hybrid-kill-entire-symbol nil)
(defhydra hydra-sp
  (:hint nil)
  "\n Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive\n------------------------------------------------------------------------------------------------------------------------\n [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy\n [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer\n [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit\n [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_J_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-down-sexp)
  ("N" sp-backward-down-sexp)
  ("p" sp-up-sexp)
  ("P" sp-backward-up-sexp)
  ("h" sp-backward-slurp-sexp)
  ("H" sp-backward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("L" sp-forward-barf-sexp)
  ("R" sp-rewrap-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("J" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("o" sp-convolute-sexp)
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)
  ("/" undo)
  ("j" x/ace-pinyin-goto-word-1)
  ("y" yank)
  ("q" nil)
  ("g" nil))

(global-set-key (kbd "C-c s") 'hydra-sp/body)

;; https://stackoverflow.com/questions/22107501/set-emacs-to-smart-auto-line-after-a-parentheses-pair
(with-eval-after-load 'smartparens
  (sp-with-modes '(js-mode web-mode kotlin-mode css-mode typescript-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(require-package 'quick-peek)
(custom-set-faces
 '(quick-peek-border-face
   ((t
     (:background "#75b79e" :height 0.1))))
 '(quick-peek-padding-face
   ((t
     (:height 0.1)))))

(add-hook 'prog-mode-hook #'subword-mode)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;; recenter

(defhydra x-hydra-recenter (:exit t)
  ""
  ("l" recenter-top-bottom "center")
  ("C-l" recenter-top-bottom "center")
  ("j" (recenter-top-bottom 1) "top")
  ("C-j" (recenter-top-bottom 1) "top")
  ("k" (recenter-top-bottom -1) "bottom")
  ("C-k" (recenter-top-bottom -1) "bottom"))
(global-set-key (kbd "C-l") #'x-hydra-recenter/body)

(provide 'x-editor)
;;; x-editor.el ends here