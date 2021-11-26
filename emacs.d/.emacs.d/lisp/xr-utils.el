;;; xr-utils.el --- xr-utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defun xr-append-init-hook (fn)
  "Add FN as a hook on `after-init-hook`."
  (add-hook 'after-init-hook fn))

(provide 'xr-utils)
;;; xr-utils.el ends here
