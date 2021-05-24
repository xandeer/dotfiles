;;; init-org-publish.el --- init-org-publish -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq org-alphabetical-lists t)

;; Explicitly load required exporters
(require 'ox-html)

(setq org-html-inline-images t)
(setq org-html-head-include-default-style nil)
(setq org-export-headline-levels 6)

(setq xr/publish-base-dir "~/projects/personal/notes")
(setq xr/publish-pub-dir "~/projects/personal/pub")
(setq org-publish-project-alist
      `(("xandeer-org"
         :base-directory ,xr/publish-base-dir
         :publishing-directory ,xr/publish-pub-dir
         :recursive t
         :exclude "area/\\|journal/\\|gtd/\\|work/\\|private/\\|copy_about_doc_norang"
         :base-extension "org"
         :publishing-function org-html-publish-to-html
         :style-include-default nil
         :html-head "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>"
         :auto-sitemap t
         :sitemap-filename "index.html"
         :with-author nil
         :with-creator nil)
        ("xandeer-extra"
         :base-directory ,xr/publish-base-dir
         :publishing-directory ,xr/publish-pub-dir
         :exclude "area/\\|journal/\\|gtd/\\|work/\\|private/\\|copy_about_doc_norang"
         :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
         :publishing-function org-publish-attachment
         :recursive t
         :with-author nil)
        ("xandeer"
         :components ("xandeer-extra" "xandeer-org"))))

(provide 'init-org-publish)
;;; init-org-publish.el ends here
