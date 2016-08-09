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

(defun examples-get-corresponding-languages-by-major-mode (major-mode)
  "Get corresponding languages by MAJOR-MODE"
  (if (assoc major-mode examples-mode-lang-alist)
      (cdr (assoc major-mode examples-mode-lang-alist))
    (list (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))))

(defun examples-get-headline-and-src-blocks (languages &rest example-org-files)
  "Search in EXAMPLE-ORG_FILES and get a list of (headline . src-blocks) accourding to LANGUAGES.

The default value of EXAMPLE-OG-FILES is specified by `examples-default-org-files'"
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

(defvar examples-headline-and-src-blocks-hashtable (make-hash-table :test #'equal))

(defun examples-get-headline-and-src-blocks-memorize (languages &rest example-org-files)
  "Same as `examples-get-headline-and-src-blocks' but with memorize."
  (let ((hash-key (cons languages example-org-files)))
    (if (member hash-key (hash-table-keys examples-headline-and-src-blocks-hashtable))
        (gethash hash-key examples-headline-and-src-blocks-hashtable)
      (let ((result (apply #'examples-get-headline-and-src-blocks languages example-org-files)))
        (puthash hash-key result examples-headline-and-src-blocks-hashtable)
        result))))

(defun examples-get-src-blocks (element languages)
  "Get src-blocks which language is in LANGUAGES in ELEMENT"
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

;;;###autoload
(defun examples (&rest example-org-files)
  "Do by examples which stored in EXMPLE-ORG-FILES"
  (interactive)
  (unless yas-minor-mode
    (yas-minor-mode 1))
  (let* ((languages (examples-get-corresponding-languages-by-major-mode major-mode))
         (headline-and-blocks (apply #'examples-get-headline-and-src-blocks-memorize languages example-org-files))
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

