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

(add-hook 'elfeed-show-mode-hook 'immersive-translate-setup)

(with-eval-after-load 'elfeed
  (x/define-keys
   elfeed-show-mode-map
   '(("d" scroll-up-command)
     ("e" scroll-down-command)
     ("f" x/link-hint-open-in-current-window)
     ("l" sdcv-search-pointer))))

(provide 'x-rss)
;;; x-rss.el ends here
