;;; x-dictionary.el --- x-dictionary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package
 '(sdcv :host github
        :repo "manateelazycat/sdcv"))

(require-package 'posframe)
(require-package 'sdcv)

(autoload #'sdcv-search-pointer "sdcv" nil t)
(autoload #'sdcv-search-input "sdcv" nil t)

(setq sdcv-program (executable-find "sdcv"))
(setq sdcv-say-word-p nil)
(setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict"))

(with-eval-after-load 'sdcv
  ;; face
  (set-face-attribute 'sdcv-tooltip-face nil
                      :foreground "#E0F0E9")

  (defun x-sdcv--translate-br (s)
    (s-replace "<br>" "\n" s))
  (advice-add 'sdcv-translate-result :filter-return #'x-sdcv--translate-br)

  ;; say
  (defvar x/sdcv-say-word-p t)

  (defun x-sdcv--say-word (&optional word)
    (when (and x/sdcv-say-word-p
               word
               (not (jieba-chinese-word? word)))
      (osx-lib-say word)))

  (advice-add 'sdcv-search-detail :after #'x-sdcv--say-word)

  (define-key sdcv-mode-map (kbd "s")
              (lambda ()
                (interactive)
                (osx-lib-say sdcv-current-translate-object))))

;;; translate
(require-package 'go-translate)

(autoload #'facemenu-add-face "facemenu" nil t)
(autoload #'gts-translator "go-translate" nil t)

(setq gts-translate-list '(("zh" "en")))
(setq gts-default-translator
      (gts-translator
       :picker (gts-prompt-picker)
       :engines (list (gts-google-engine) (gts-google-rpc-engine))
       :render (gts-buffer-render)))

(provide 'x-dictionary)
;;; x-dictionary.el ends here
