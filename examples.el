(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'yasnippet)
(defgroup examples nil
  "examples group"
  :prefix "examples-")

(defcustom examples-mode-lang-alist '((eshell-mode "sh" "emacs-lisp" "elisp"))

  "Mapping the correspondence between `major-mode' and the src-block languages.

The car of element is the major-mode.
The cdr of element is the src-block language list"
  :group 'examples)

(defcustom examples-default-org-files (let ((example-dir (concat (file-name-directory (buffer-file-name)) "examples")))
                                        (unless (file-exists-p example-dir)
                                          (make-directory example-dir t))
                                        (directory-files example-dir t "org$"))
  "Default example files"
  :group 'examples)

(defun examples-get-response-languages-by-major-mode (major-mode)
  (if (assoc major-mode examples-mode-lang-alist)
      (cdr (assoc major-mode examples-mode-lang-alist))
    (list (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))))

(defun examples-get-headline-and-src-blocks (languages &rest example-org-files)
  ""
  (let ((example-org-files (or example-org-files examples-default-org-files)))
    (with-temp-buffer
      (mapc (lambda (file)
              (insert-file-contents file)
              (newline))
            example-org-files) 
      (let ((headline-and-blocks (org-element-map (org-element-parse-buffer) 'headline
                                   (lambda (ele)
                                     (cons (org-element-property :raw-value ele)
                                           (examples-get-src-blocks ele languages))))))
        (cl-delete-if (lambda (x)
                        (null (cdr x)))
                      headline-and-blocks)))))

(defun examples-get-src-blocks (element languages)
  (org-element-map element 'src-block
    (lambda (src-block)
      (let ((src-language (org-element-property :language src-block))
            (src-preserve-indent (org-element-property :preserve-indent src-block)))
        (when (member src-language languages)
          (with-temp-buffer
            (insert (org-element-property :value src-block))
            (unless src-preserve-indent
              (org-do-remove-indentation))
            (buffer-substring-no-properties (point-min) (- (point-max) 1)) ;; minus 1 to trim the last <RET>
            ))))))

(defun examples (&rest example-org-files)
  ""
  (interactive)
  (unless yas-minor-mode
    (yas-minor-mode 1))
  (let* ((languages (examples-get-response-languages-by-major-mode major-mode))
         (headline-and-blocks (examples-get-headline-and-src-blocks languages))
         (headlines (mapcar #'car headline-and-blocks))
         (headline (completing-read "example:" headlines))
         (src-blocks (cdr (assoc-string headline headline-and-blocks)))
         (src-block (if (> (length src-blocks) 1)
                        (completing-read "" src-blocks)
                      (or (car src-blocks) "")))
         (idx 0))
    (yas-expand-snippet (replace-regexp-in-string "{[^{}0-9][^{}]*}"
                                                  (lambda (text)
                                                    (let ((desc (substring text 1 (- (length text) 1))))
                                                      (setq idx (+ 1 idx))
                                                      (format "{%d:%s}" idx desc)))
                                                  src-block))))

