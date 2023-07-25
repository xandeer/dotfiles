;;; x-rss.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq elfeed-feeds
      '(("http://nedroid.com/feed/" webcomic)
        ("https://sachachua.com/blog/feed/" emacs)
        ("https://irreal.org/blog/?feed=rss2" emacs blog)
        ("https://oremacs.com/atom.xml" emacs)
        ("https://karthinks.com/index.xml" emacs)

        ("https://developer.android.com/feeds/androidx-release-notes.xml" android release)
        ("https://medium.com/feed/androiddevelopers" android blog)
        ("https://android-developers.googleblog.com/feeds/posts/default?alt=rss" android blog)

        ("https://www.huangjiwei.com/blog/?feed=rss2" comic)))

(with-eval-after-load 'elfeed
  (defun x/elfeed-show-setup ()
    (setq-local x/shr-next-document-fn 'elfeed-show-next)
    (setq-local x/shr-previous-document-fn 'elfeed-show-prev))

  (setq browse-url-browser-function 'eww)

  (x/define-keys
   elfeed-show-mode-map
   '(("d" x/shr-scroll-up)
     ("e" x/shr-scroll-down)
     ("f" x/link-hint-open-in-current-window)
     ("l" sdcv-search-pointer))))

(provide 'x-rss)
;;; x-rss.el ends here
