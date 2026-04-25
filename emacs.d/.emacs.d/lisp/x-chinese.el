;;; x-chinese.el --- chinese -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; after clone, run `emt-download-module' to download the module
(x/package-use '(emt . ("roife/emt"
                        :files ("*.el" "module/*" "module"))))
(x/append-init-hook #'emt-mode)

(provide 'x-chinese)
;;; x-chinese.el ends here
