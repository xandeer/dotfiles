;;; init-avy.el --- avy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'avy)

(straight-register-package
 '(pinyinlib :host github
             :repo "xlshiz/pinyinlib.el"))
(straight-use-package 'ace-pinyin)
(leaf avy
  :doc "Jump to things in Emacs tree-style."
  :url "https://github.com/abo-abo/avy"
  :bind
  ("M-g a"   . beginning-of-buffer)
  ("M-g e"   . end-of-buffer)
  ("M-g M-a" . beginning-of-buffer-other-window)
  ("M-g M-e" . end-of-buffer-other-window)
  ("M-g v"   . scroll-other-window)
  ("M-g M-v" . scroll-other-window-down)
  ("M-g l"   . avy-goto-line)
  ("M-g M-g" . xr/ace-pinyin-goto-word-1))

(setq xr/ace-mode nil)
;;;###autoload
(defun xr/ace-mode-p ()
  "Whether ace goto word."
  (bound-and-true-p xr/ace-mode))
(leaf ace-pinyin
  :after avy
  :config
  (ace-pinyin-global-mode 1)
  (defun xr/ace-pinyin-goto-word-1 ()
    "Ace-pinyin replacement of `avy-goto-word-1'."
    (interactive)
    (setq xr/ace-mode t)
    (call-interactively 'ace-pinyin-goto-word-1)
    (setq xr/ace-mode nil))
  ;; Another way
  (defun -xr/ace-pinyin-goto-word-1 ()
    "Ace-pinyin replacement of `avy-goto-word-1'."
    (interactive)
    (let ((ace--input-method current-input-method)
          (ace--buffer (buffer-file-name)))
      (when ace--input-method (toggle-input-method))
      (call-interactively 'ace-pinyin-goto-word-1)
      (when (and ace--input-method
                 (string-equal (buffer-file-name) ace--buffer))
        (toggle-input-method)))))

(provide 'init-avy)
;;; init-avy.el ends here
