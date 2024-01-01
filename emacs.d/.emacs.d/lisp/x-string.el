;;; x-string.el --- string utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defun x/text-normalize (text)
  "Normalize text by removing or modifying specific patterns.

This function takes a string TEXT and performs the following transformations:
- Removes Org-mode links in the form [[...][...]].
- Removes Anki cloze deletions in the form {{c1::...}}.
- Removes Org-mode dynamic blocks (e.g., #+BEGIN_... and #+END_...).
- Removes asterisk (*) characters.
- Replaces newline characters with spaces.

The result is a normalized version of TEXT with the specified patterns removed.

Example:
\(x/text-normalize \"This is some [[link]] with {{c1::Anki cloze}}.\")
=> \"This is some link with Anki cloze.\"

Args:
  TEXT (string): The input text to be normalized.

Returns:
  string: The normalized text."
  (let ((org-link (rx (seq "[[" (*? anything) "][" (group (+? anything)) "]]")))
        (anki-cloze (rx (seq "{{c1::" (group (+? anything)) "}}")))
        (asterisk (rx (any "*")))
        (org-comment (rx (seq "#+" (*? anything) "\n")))
        ;; (org-dblock (rx (seq "#+" (group (| "BEGIN" "begin" "END" "end")) (0+ nonl))))
        )
    (->> text
         (replace-regexp-in-string org-link "\\1")
         (replace-regexp-in-string anki-cloze "\\1")
         (replace-regexp-in-string org-comment "")
         ;; (replace-regexp-in-string org-dblock "")
         (replace-regexp-in-string asterisk "")
         (replace-regexp-in-string org-property-drawer-re "")
         (replace-regexp-in-string "\n" " "))))

;;;###autoload
(defun x/text-normalize-region (beg end)
  "Normalize text in the region from BEG to END.

This function is a wrapper around `x/text-normalize' that operates on the
region from BEG to END.  It is intended to be used as an interactive command.

Args:
  BEG (int): The beginning of the region.
  END (int): The end of the region."
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (insert (x/text-normalize text))))

;;;###autoload
(defun x/replace (old new &optional beg end)
  "Replace occurrences of string OLD with string NEW in the current buffer.
Operate between positions BEG and END.
If BEG and END are not provided, the function operates on the entire buffer."
  ;; Set BEG and END to the buffer boundaries if they are not provided
  (setq beg (or beg (point-min)))
  (setq end (or end (point-max)))

  ;; Ensure BEG is less than or equal to END
  (when (> beg end)
    (let (mid)
      (setq mid end end beg beg mid)))
  ;; Save the current position and restore it later
  (save-excursion
    (goto-char beg)
    (while (re-search-forward old end t)
      (replace-match new))))

;;;###autoload
(defun x/convert-to-chinese-quotations ()
  "Convert all [“|“ ‘|’] to [ 「|」『|』] in current buffer."
  (interactive)

  (let ((quotas
         '(("‘" . "『")
           ("’" . "』")
           ("“" . "「")
           ("”" . "」")
           (", " . "，")
           ("," . "，")
           ("; " . "；")
           ("\\. " . "。")
           ("^\\([0-9]*\\)。" . "\\1. ")
           ;; sub items
           ("\\(  [0-9]*\\)。" . "\\1. ")
           ("\\.」" . "。」")
           ("\\.』" . "。』")
           ("\\.$" . "。")
           ("? " . "？")
           ;; (": " . "：")
           (" \\{2,\\}:" . " :")
           ("（" . "(")
           ("）" . ")")
           ("・" . "·")
           ("! " . "！")
           (" )" . ")")
           (" 」" . "」")
           (" 』" . "』"))))
    (mapc (lambda (q)
            (x/replace (car q) (cdr q)))
          quotas)))

;;;###autoload
(defun x/journal-replace-addr-tags ()
  "Replace home tags in current buffer."
  (interactive)
  (let ((office21 (rx (seq (group "** 2021 " (*? anything)) "@office")))
        (office22 (rx (seq (group "** 2022 " (*? anything)) "@office")))
        (home21 (rx (seq (group "** 2021 " (*? anything)) "@home")))
        (home22 (rx (seq (group "** 2022 " (*? anything)) "@home")))
        (home23 (rx (seq (group "** 2023 " (*? anything)) "@home")))
        (addr16 (rx (seq (group "** 2016 " (*? anything)) ":" eol)))
        (addr17 (rx (seq (group "** 2017 " (*? anything)) ":" eol)))
        (addr18 (rx (seq (group "** 2018 " (*? anything)) ":" eol)))
        (addr19 (rx (seq (group "** 2019 " (*? anything)) ":" eol)))
        (addr20 (rx (seq (group "** 2020 " (*? anything)) ":" eol)))
        (addr21 (rx (seq (group "** 2021 " (*? anything)) ":" eol)))
        (replace-tag (lambda (reg to)
                       (goto-char (point-min))
                       (while (re-search-forward reg nil t)
                         (replace-match to)))))
    (save-excursion
      (funcall replace-tag office21 "\\1海信")
      (funcall replace-tag office22 "\\1海信")
      (funcall replace-tag addr16 "\\1:兴东:中粮:")
      (funcall replace-tag addr17 "\\1:大冲:泰邦:")
      (funcall replace-tag addr18 "\\1:大冲:泰邦:")
      (funcall replace-tag addr19 "\\1:金钟:海信:")
      (funcall replace-tag addr20 "\\1:金钟:海信:")
      (funcall replace-tag addr21 "\\1:南水:海信:")
      ;; (funcall replace-tag home21 "\\1南水")
      (funcall replace-tag home22 "\\1水湾")
      (funcall replace-tag home23 "\\1水湾"))))

(provide 'x-string)
;;; x-string.el ends here
