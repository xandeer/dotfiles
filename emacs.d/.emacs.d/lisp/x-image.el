;;; x-image.el --- x-image -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((map image-mode-map))
  (define-key map ">" #'x/telega-send-to-chat)
  (define-key map (kbd "RET") #'x/open-in-default-program)
  (define-key map "o" #'x/open-in-default-program))

(provide 'x-image)
;;; x-image.el ends here
