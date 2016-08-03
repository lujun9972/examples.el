(require 'f)

(defun examples--tldr-to-examples (tldr-file &optional dest-dir)
  "translate TLDR-FILE into examples file which located in DEST-DIR"
  (let* ((dest-dir (or dest-dir (file-name-directory (buffer-file-name))))
         (filename (f-join dest-dir (format "%s.org" (file-name-base tldr-file)))))
    (with-temp-file filename
      (insert-file-contents tldr-file)
      (replace-regexp "^# \\(.+\\)$" "* \\1" nil (point-min) (point-max))
      (replace-regexp "^> \\(.+\\)$" "\\1" nil (point-min) (point-max))
      (replace-regexp "^- \\(.+\\)$" "** \\1" nil (point-min) (point-max))
      (replace-regexp "^`\\(.+\\)`$" "#+BEGIN_SRC sh\n  \\1\n#+END_SRC" nil (point-min) (point-max))
      (replace-regexp "{{" "${" nil (point-min) (point-max))
      (replace-regexp "}}" "}" nil (point-min) (point-max)))))

(dolist (file (directory-files "~/github/tldr/pages/linux" t ".md$"))
  (examples--tldr-to-examples file "~/github/examples.el/examples"))
