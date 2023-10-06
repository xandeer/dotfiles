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
        (org-dblock (rx (seq "#+" (group (| "BEGIN" "begin" "END" "end")) (0+ nonl)))))
    (->> text
         (replace-regexp-in-string org-link "\\1")
         (replace-regexp-in-string anki-cloze "\\1")
         (replace-regexp-in-string org-dblock "")
         (replace-regexp-in-string asterisk "")
         (replace-regexp-in-string "\n" " "))))

(provide 'x-string)
;;; x-string.el ends here
