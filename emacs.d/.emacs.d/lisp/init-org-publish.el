;;; init-org-publish.el --- init-org-publish -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf org
  :init
  (setq org-alphabetical-lists t)
  (require 'ox-html)
  :config
  (setq org-html-inline-images t)
  (setq org-html-head-include-default-style nil)
  (setq org-export-headline-levels 6)
  (setq org-publish-timestamp-directory (no-littering-expand-var-file-name "org/timestamps"))

  (setq xr/publish-base-dir "~/projects/personal/notes")
  (setq xr/publish-pub-dir "~/projects/personal/xandeer.github.io")
  (setq org-publish-project-alist
        `(("xandeer-org"
           :base-directory ,xr/publish-base-dir
           :publishing-directory ,xr/publish-pub-dir
           :recursive t
           :exclude "area/\\|journal-?.*/\\|gtd/\\|work/\\|private/\\|copy_about_doc_norang\\|hledger.*"
           :base-extension "org"
           :publishing-function org-html-publish-to-html
           :style-include-default nil
           :html-head ,(concat
                        "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>"
                        "<meta charset=\"utf-8\">"
                        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">")
           :auto-sitemap t
           :sitemap-filename "index.org"
           :with-author nil
           :with-creator nil)
          ("xandeer-extra"
           :base-directory ,xr/publish-base-dir
           :publishing-directory ,xr/publish-pub-dir
           :exclude "area/\\|journal-?.*/\\|gtd/\\|work/\\|private/\\|copy_about_doc_norang\\|hledger.*"
           :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|webp"
           :publishing-function org-publish-attachment
           :recursive t
           :with-author nil)
          ("xandeer"
           :components ("xandeer-extra" "xandeer-org")))))

(provide 'init-org-publish)
;;; init-org-publish.el ends here
(org-link-types)