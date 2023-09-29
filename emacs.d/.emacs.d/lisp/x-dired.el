;;; x-dired.el --- x-dired -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'dired
  (setq dired-deletion-confirmer '(lambda (x) t))
  (setq insert-directory-program (executable-find "ls"))
  ;; this doesn't work
  (setq dired-no-confirm t)
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq dired-create-destination-dirs 'always)
  (setq dired-mouse-drag-files t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-auto-revert-buffer t)

  (setq dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  ;; (dirvish-override-dired-mode)

  ;; copy from doom
  (defun +dired/quit-all ()
    "Kill all `dired-mode' buffers."
    (interactive)
    (mapc #'kill-buffer (doom-buffers-in-mode 'dired-mode))
    (message "Killed all dired buffers"))

  (defun x/dired-find-file ()
    "Use `x/open-with' to open media files in dired."
    (interactive)
    (unless (or (eq major-mode 'dired-mode)
                (eq major-mode 'dirvish-mode))
      (user-error "Not in dired"))
    (if (string-match-p
         (regexp-opt '(".png" ".jpg" ".mov" ".pdf" ".numbers"))
         (dired-get-filename))
        (x/open-with)
      (dired-find-file)))

  (defun x/dired-find-file-other-window ()
    "Use `x/open-with' to open video files in dired."
    (interactive)
    (unless (or (eq major-mode 'dired-mode)
                (eq major-mode 'dirvish-mode))
      (user-error "Not in dired"))
    (if (string-match-p
         (regexp-opt '(".mov"))
         (dired-get-filename))
        (x/open-with)
      (dired-find-file-other-window)))

  (define-key dired-mode-map [remap dired-find-file] #'x/dired-find-file)
  (define-key dired-mode-map [remap dired-find-file-other-window] #'x/dired-find-file-other-window)

  (defun x/dired-copy-files-to-downloads ()
    "Copy marked files to Downloads folder."
    (interactive)
    (let ((downloads-dir (expand-file-name "~/Downloads"))
          (files (dired-get-marked-files)))
      (mapc (lambda (file)
              (copy-file
               file
               (expand-file-name (file-name-nondirectory file)
                                 downloads-dir))) files)))

  (x/define-keys
   dired-mode-map
   '(("* n" dired-next-marked-file)
     ("* p" dired-prev-marked-file)
     ("q"  +dired/quit-all)
     ("d"  dired-do-delete)
     ("h"  dired-up-directory)
     ("u"  dired-up-directory)
     ("n"  dired-unmark)
     ("N"  dired-unmark-all-marks)
     ("@"  x/change-hs-on-dired)
     ("^"  x/cow-current)
     (">"  x/telega-send-to-chat)
     ("i"  wdired-change-to-wdired-mode)
     ("j"  dired-next-line)
     ("k"  dired-previous-line)
     ("l"  dired-find-file)
     ("r"  x/reveal-in-finder)
     ("."  dirvish))))

(require 'dired-x)

(setq-default dired-omit-extensions (remove ".bin" dired-omit-extensions))
;; KOReader
;; (add-to-list 'dired-omit-extensions ".sdr")
;; (setq dired-omit-extensions (delete ".sdr" dired-omit-extensions))
;; syncthing
(setq dired-omit-files
      (concat dired-omit-files "\\|" (rx ".stfolder" string-end)))
(setq dired-omit-files
      (concat dired-omit-files "\\|" (rx ".DS_Store" string-end)))

(setq dired-clean-confirm-killing-deleted-buffers nil)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
;; (add-hook 'dired-mode-hook #'dired-collapse-mode)
;; (remove-hook 'dired-mode-hook #'dired-collapse-mode)

(require 'dired-filter)
(with-eval-after-load 'dired-filter
  (add-hook 'dired-mode-hook #'dired-filter-group-mode)
  (define-key dired-filter-map "p" #'dired-filter-pop-all)

  (setq dired-filter-revert 'always)
  (setq dired-filter-group-saved-groups
        '(("default"
           ("Directory"
            (directory))
           ("PDF"
            (extension "pdf"))
           ("LaTeX"
            (extension "tex" "bib"))
           ("Source"
            (extension "c" "cpp" "hs" "rb" "py" "r" "cs" "el" "lisp" "html" "js" "ts"))
           ("Config"
            (regexp . "config\\|package.json\\|^\\..*rc"))
           ("Style"
            (extension "css"))
           ("Doc"
            (extension "md" "rst" "txt"))
           ("Org"
            (extension "org"))
           ("Archives"
            (extension "zip" "rar" "gz" "bz2" "tar"))
           ("Media"
            (extension "mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
           ("Images"
            (extension "jpg" "JPG" "webp" "png" "PNG" "jpeg" "JPEG" "bmp" "BMP" "TIFF" "tiff" "gif" "GIF"))
           ("Symlinks" (symlink))
           ("Git"
            (regexp . "^\\.git"))
           ("KOReader"
            (extension "sdr"))))))

(require 'dired-rainbow)
(with-eval-after-load 'dired-rainbow
  (dired-rainbow-define html "#eb5286"
                        ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#A17F7E"
                        ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2"
                        ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#8aadff"
                        ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd"
                        ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f"
                        ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b"
                        ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11"
                        ("log"))
  (dired-rainbow-define shell "#f6993f"
                        ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172"
                        ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5"
                        ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff"
                        ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a"
                        ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63"
                        ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#e3342f"
                        ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb"
                        ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f"
                        ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9"
                        ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(require 'dired-narrow)
(with-eval-after-load 'dired-narrow
  (define-key dired-narrow-map (kbd "<down>") 'dired-narrow-next-file)
  (define-key dired-narrow-map (kbd "<up>") 'dired-narrow-previous-file)
  (define-key dired-narrow-map (kbd "<right>") 'dired-narrow-enter-directory))

(provide 'x-dired)
;;; x-dired.el ends here
