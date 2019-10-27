;;; xandeer/chinese/config.el -*- lexical-binding: t; -*-

(use-package! pyim
  :bind
  (("M-i" . pyim-convert-string-at-point))
  :config
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (map! :map pyim-mode-map
        "," #'pyim-page-previous-page
        "." #'pyim-page-next-page)
  (if (display-graphic-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))

  (setq default-input-method "pyim"
        pyim-default-scheme 'xiaohe-shuangpin
        pyim-page-length 4
        pyim-dcache-directory "~/.cache/pyim")
        ;; pyim-dicts
        ;; `((:name
           ;; "pyim-bigdict"
           ;; :file
           ;; ,(expand-file-name (concat doom-private-dir "etc/pyim/pyim-bigdict.pyim.gz")))))

  (setq pyim-punctuation-dict
        '(
          ; ("'" "‘" "’")
          ; ("\"" "“" "”")
          ("'" "「" "」")
          ("\"" "『" "』")
          ("_" "——")
          ("^" "…")
          ("]" "】")
          ("[" "【")
          ("@" "◎")
          ("?" "？")
          (">" "》")
          ("=" "＝")
          ("<" "《")
          (";" "；")
          (":" "：")
          ("/" "、")
          ("\\" "、")
          ("." "。")
          ("-" "－")
          ("," "，")
          ("+" "＋")
          ("*" "×")
          (")" "）")
          ("(" "（")
          ("&" "※")
          ("%" "％")
          ("$" "￥")
          ("#" "＃")
          ("!" "！")
          ("`" "・")
          ("~" "～")
          ("}" "」")
          ("|" "÷")
          ("{" "「")))

  (use-package! pyim-greatdict
    :config (pyim-greatdict-enable))

  (add-hook 'emacs-startup-hook
          #'(lambda () (pyim-restart-1 t))))
