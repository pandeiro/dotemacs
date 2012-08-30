(defun filter (condp lst)
  "Functional programming 101"
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;;
;; Functions to open buffers on all a project's files filtered by extension
;;
;; Usage: (visit-files-by-types '("html" "js" "css"))
;;
(defun find-files-by-ext (dir ext)
  "Find all files with extension ext in dir"
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((default-directory dir)
	(to-search (concat "*." ext)))
    (mapcar 'expand-file-name (file-expand-wildcards to-search))))

(defun visit-file-list (files)
  "Visits all files in a list; files must be relative to default-directory or absolute paths"
  (unless (null files)
    (dolist (file files) (find-file file))))

(defun list-all-subdirs (dir)
  "List all nested subdirs"
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
	(dirs '())
	(files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
	(let ((file (concat dir "/" file)))
	  (when (file-directory-p file)
	    (setq dirs (append (cons file
				     (list-all-subdirs file))
			       dirs))))))
    dirs))

(defun list-all-files-by-types (dirs exts)
  (let ((result '()))
    (dolist (dir dirs)
      (dolist (ext exts)
	(setq result (append result (find-files-by-ext dir ext)))))
    result))


(defun visit-files-by-types (exts)
  "Visits all files with extensions exts in default-directory and subdirs"
  (let ((dirs (append (list default-directory)
		      (list-all-subdirs default-directory))))
    (visit-file-list (list-all-files-by-types dirs exts))))

(provide 'my-util)
