;;; x-editor.el --- x-editor -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; case
(define-prefix-command 'x/case-map)
(x/define-keys x/case-map
               '(("u" upcase-region)
                 ("l" downcase-region)
                 ("c" upcase-initials-region)))
(x/define-keys ctl-x-map
               '(("c" x/case-map)))

;;; transpose
(define-prefix-command 'x/transpose-map)
(x/define-keys x/transpose-map
               '(("w" transpose-words)
                 ("l" transpose-lines)
                 ("s" transpose-sexps)
                 ("p" transpose-paragraphs)
                 ("t" transpose-sentences)
                 ("C-t" transpose-chars)))

(x/define-keys global-map
               '(("C-t" x/transpose-map)))

;; https://stackoverflow.com/questions/145291/smart-home-in-emacs/145359
(defun x/smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line`.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^")                 ; Use (interactive) in Emacs 22 or older
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(x/define-keys global-map
               '(([remap move-beginning-of-line] x/smart-beginning-of-line)
                 ([remap newline] newline-and-indent)
                 ("M-;" comment-line)
                 ("H-z" undo)))

(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)

;; Delete multiple consecutive blank characters at once
(add-hook 'activate-mark-hook (lambda () (hungry-delete-mode -1)))
(add-hook 'deactivate-mark-hook #'hungry-delete-mode)
(x/append-init-hook #'global-hungry-delete-mode)

;; Something wrong with `golden-ration-mode'.
;; (setq-default visual-fill-column-width 76)
;; (x/append-init-hook #'global-visual-fill-column-mode)

(x/define-keys global-map
               '(([remap kill-ring-save] easy-kill)
                 ([remap mark-sexp] easy-mark)))

(setq whitespace-style '(newline newline-mark))
(run-with-idle-timer 1 nil
                     (lambda ()
                       (global-whitespace-mode 1)
                       (set-face-foreground 'whitespace-newline "gray85")
                       (set-face-attribute 'whitespace-newline nil :height 0.7)))

(unless
    (fboundp 'auto-compression-mode)
  (autoload #'auto-compression-mode "jka-cmpr" nil t))
(x/append-init-hook #'auto-compression-mode)

(require 'auto-save)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)
(setq auto-save-idle 0.5)
(add-hook 'org-capture-mode-hook #'auto-save-disable)
(add-hook 'org-capture-prepare-finalize-hook #'auto-save-enable)
(x/append-init-hook #'auto-save-enable)

(add-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode)
(with-eval-after-load 'eldoc-box
  (set-face-background 'eldoc-box-border "gray10"))

(setq expand-region-subword-enabled t)
(x/define-keys global-map
               '(("C-;" er/expand-region)
                 ("M-q" unfill-region)
                 ("H-q" (lambda ()
                          (interactive)
                          (mark-whole-buffer)
                          (call-interactively #'unfill-region)))))


(x/append-init-hook #'global-page-break-lines-mode)
(setq page-break-lines-max-width 80)
(with-eval-after-load 'page-break-lines
  (when
      (fboundp 'diminish)
    (diminish 'page-break-lines-mode)))

(x/append-init-hook #'global-hl-line-mode)
;; (with-eval-after-load 'hl-line
;;   (set-face-background 'hl-line "#2a2e48"))


(add-hook 'prog-mode-hook #'color-identifiers-mode)

(unless
    (fboundp 'indent-tools-hydra/body)
  (autoload #'indent-tools-hydra/body "indent-tools" nil t))
(global-set-key (kbd "C-c TAB") 'indent-tools-hydra/body)

(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
(setq highlight-indent-guides-responsive nil)
(setq highlight-indent-guides-delay 0.5)

(x/append-init-hook #'rainbow-mode)
(add-hook 'text-mode-hook #'rainbow-mode)
(add-hook 'org-mode-hook #'rainbow-mode)
(add-hook 'css-mode-hook #'rainbow-mode)
(add-hook 'html-mode-hook #'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)
(with-eval-after-load 'rainbow-mode
  (when (fboundp 'diminish)
    (diminish 'rainbow-mode)))

;; (x/append-init-hook #'rainbow-identifiers-mode)
;; (add-hook 'text-mode-hook #'rainbow-identifiers-mode)
;; (add-hook 'org-mode-hook #'rainbow-identifiers-mode)
(add-hook 'css-mode-hook #'rainbow-identifiers-mode)
(add-hook 'html-mode-hook #'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
(with-eval-after-load 'rainbow-identifiers
  (when (fboundp 'diminish)
    (diminish 'rainbow-identifiers-mode)))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-src-mode-hook #'rainbow-delimiters-mode)

(setq htmlize-pre-style t)

(x/append-init-hook #'show-paren-mode)

(x/append-init-hook #'smartparens-global-mode)
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
  (require 'smartparens-config)
  (sp-with-modes '(js-mode json-mode web-mode kotlin-mode css-mode typescript-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(custom-set-faces
 '(quick-peek-border-face
   ((t
     (:background "#75b79e" :height 0.1))))
 '(quick-peek-padding-face
   ((t
     (:height 0.1)))))

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'x-editor)
;;; x-editor.el ends here
