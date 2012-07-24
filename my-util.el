;; Functions to open buffers on all a project's files filtered by extension
(defun filter (condp lst)
  "Functional programming 101"
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun find-files-by-ext (dir ext)
  "Find all files with extension ext in dir"
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((default-directory dir)
	(to-search (concat "*." ext)))
    (file-expand-wildcards to-search)))

(defun visit-file-list (files)
  "Visits all files in a list"
  (unless (null files)
    (dolist (file files) (find-file file))))

(defun list-all-dirs (dir)
  "Returns list of dir and all nested subdirs"
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
	(dirs (list dir))
	(files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
	(let ((file (concat dir "/" file)))
	  (when (file-directory-p file)
	    (setq dirs (append (cons file
				     (directory-dirs file))
			       dirs))))))
    dirs))

(defun visit-files-by-types (exts)
  "Visits all files with extensions exts in default-directory and subdirs"
  (let ((dirs (list-all-dirs default-directory)))
    (dolist (dir dirs)
      (mapc (lambda (ext) (visit-file-list (find-files-by-ext dir ext)))
	    exts))))

(provide 'my-util)
