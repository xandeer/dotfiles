;;; x-mouse.el --- x-mouse -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default
  blink-cursor-interval           .6
  blink-matching-paren            t
  cursor-in-non-selected-windows  t)

(setq hscroll-margin                   3
      scroll-margin                    3
      hscroll-step                     3
      scroll-step                      3
      scroll-conservatively            100000
      scroll-preserve-screen-position  'always
      scroll-error-top-bottom          t
      pixel-scroll-precision-mode      t)

(pixel-scroll-mode 1)
(blink-cursor-mode 1)

(provide 'x-mouse)
;;; x-mouse.el ends here
