;; This is where I am storing elisp functions that I write from scratch, not
;; just customizations and specific mode-related stuff

(defun filter (condp lst)
  "Functional programming 101"
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun remove-whitespace (str)
  (replace-regexp-in-string "[\s\t\n]+" "" str))

(defun count-chars ()
  "Counts all non-whitespace characters in the current buffer and outputs results to
mini-buffer. Gives totals for up to point, from point to end of buffer, and total."
  (interactive)
  (let ((before-point (length (remove-whitespace (buffer-substring (point-min) (point)))))
	(after-point (length (remove-whitespace (buffer-substring (point) (point-max)))))
	(output "Done: %d chars; To-do: %d chars; Total: %d chars"))
    (message (apply 'format output (list before-point after-point (+ before-point after-point))))))

(defun current-region-or-word ()
  (or (and transient-mark-mode mark-active
	   (buffer-substring-no-properties
	    (region-beginning) (region-end)))
      (current-word)))

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

;; borrowed from https://github.com/bodil/emacs.d/blob/master/bodil-defuns.el
(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun dired-insert-subdir-stay-put ()
  (interactive)
  (when (eq 'dired-mode major-mode)
    (let ((dir (dired-file-name-at-point)))
      (dired-maybe-insert-subdir dir)
      (set-mark-command 4)
      (message (concat dir " inserted below")))))

(provide 'my-util)
