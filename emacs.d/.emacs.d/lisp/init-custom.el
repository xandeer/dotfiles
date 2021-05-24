;;; init-custom.el --- init-custom -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup xandeer nil
  "Xandeer Emacs Custom Configurations."
  :group 'emacs)

(defvar *eldoc-use* nil "The Theme.")

(defcustom *theme* 'doom-dracula
  "The Theme."
  :group 'xandeer
  :type 'symbol)

(defcustom *mouse-color* "black"
  "Mouse color."
  :group 'xandeer
  :type 'string)

(defcustom *font* "Consola Mono"
 "The main font.  After change it, run `M-x xandeer/set-font' to see the effect."
 :group 'xandeer
 :type 'string)

(defcustom *font-size* 16.0
 "The main font.  After change it, run `M-x xandeer/set-font' to see the effect."
 :group 'xandeer
 :type 'float)

(defcustom *font-weight* 'normal
 "The main font.  After change it, run `M-x xandeer/set-font' to see the effect."
 :group 'xandeer
 :type 'symbol)

(defcustom *font-cjk* "TsangerJinKai05-6763 W05" ;"Xingkai SC"
 "The cjk font.  After change it, run `M-x xandeer/set-font' to see the effect."
 :group 'xandeer
 :type 'string)

(defcustom *font-size-cjk* 18.0
 "The cjk font.  After change it, run `M-x xandeer/set-font' to see the effect."
 :group 'xandeer
 :type 'float)

(defcustom *font-weight-cjk* 'light
 "The cjk font.  After change it, run `M-x xandeer/set-font' to see the effect."
 :group 'xandeer
 :type 'symbol)

(provide 'init-custom)
;;; init-custom.el ends here
