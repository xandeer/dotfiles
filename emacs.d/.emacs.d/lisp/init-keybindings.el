;;; init-keybindings.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf general
  :straight t
  :doc "More convenient key definitions in emacs."
  :url "https://github.com/noctuid/general.el"
  :commands gsetq gsetq-local gsetq-default
  :init
  (defalias 'gsetq #'general-setq)
  (defalias 'gsetq-local #'general-setq-local)
  (defalias 'gsetq-default #'general-setq-default))

(general-define-key
 "C-."  #'imenu
 "M-x"  #'counsel-M-x
 "C-x C-f" #'counsel-find-file

 ;;; newlines
 [remap newline]  #'newline-and-indent

 ;; search
 "C-s"            #'swiper-isearch
 "C-S-s"          #'swiper-isearch-thing-at-point
 "C-r"            #'ivy-resume

 ;;; buffers
 "C-x b"    #'switch-to-buffer
 "C-x 4 b"  #'switch-to-buffer-other-window
 "C-x C-b"  #'ibuffer-list-buffers)

(leaf mac-keybind
  :custom
  (mac-option-modifier        . 'meta)
  (mac-command-modifier       . 'hyper)
  (mac-right-command-modifier . 'super)
  (mac-function-modifier      . 'super)
  :bind
   ;; cursor Movement
  ("H-<up>"   . beginning-of-buffer)
  ("H-<down>" . end-of-buffer)
  ("H-l"      . goto-line)

   ;; text Operations
  ("H-a" . mark-whole-buffer)
  ("H-v" . yank)
  ("H-c" . kill-ring-save)
  ("H-s" . save-buffer)
  ("H-z" . undo)
  ("H-w" . delete-window)
  ("H-<backspace>" . n/kill-line-0)
  :init
  ;; unset
  (defun n/kill-line-0 ()
    (interactive)
    (kill-line 0))
  (global-unset-key (kbd "<magnify-down>"))
  (global-unset-key (kbd "<magnify-up>")))

(general-define-key
 "<mouse-4>" (lambda () (scroll-down 1))
 "<mouse-5>" (lambda () (scroll-up 1)))

(general-define-key
 :keymaps 'company-active-map
 "C-o"        #'company-search-kill-others
 "C-n"        #'company-select-next
 "C-p"        #'company-select-previous
 "C-h"        #'company-quickhelp-manual-begin
 "C-S-h"      #'company-show-doc-buffer
 "C-s"        #'company-search-candidates
 "M-s"        #'company-filter-candidates
 [C-tab]      #'xr/company-complete
 [tab]        #'company-complete-common-or-cycle
 [backtab]    #'company-select-previous
 [C-return]   #'counsel-company)

(general-define-key
 :keymaps 'company-search-map
 "C-n"        #'company-search-repeat-forward
 "C-p"        #'company-search-repeat-backward
 "C-s"        (lambda () (company-search-abort) (company-filter-candidates)))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
