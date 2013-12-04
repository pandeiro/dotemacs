;;; couchdb.el --- CouchDb client library for Emacs Lisp

;; Copyright (C) 2007  Edward O'Connor

;; Author: Edward O'Connor <hober0@gmail.com>
;; Keywords: data, convenience
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Prety basic, bare-bones CouchDb client library for Emacs Lisp.
;; Lightly tested. Share and Enjoy! The latest version lives here:
;;
;;             http://edward.oconnor.cx/elisp/couchdb.el


;;; History:
;; 2007-10-25: Initial version.

;;; Code:

(require 'json)
(require 'url)

(defvar url-http-end-of-headers)

(defvar couchdb-host "localhost")

(defvar couchdb-port 5984)

(defun couchdb-url (&optional relative)
  "Generate a CouchDb URL for RELATIVE."
  (format "http://%s:%d/%s" couchdb-host couchdb-port (or relative "")))

;; HTTP

(defvar couchdb-request-headers
  '(("GET" ("Accept" . "application/json"))
    ("POST" ("Content-type" . "application/json"))
    ("PUT" ("Content-type" . "application/json"))
    ("DELETE"))
  "HTTP headers to be used for these request types.")

(defun couchdb-response (buf)
  "Extract the CouchDb response from BUF."
  (with-current-buffer buf
    (goto-char url-http-end-of-headers)
    (prog1 (json-read)
      (kill-buffer buf))))

(defun couchdb-request (method url &optional data)
  "Use HTTP METHOD to request URL with optional payload DATA."
  (let ((url-package-name "couchdb.el")
        (url-request-method method)
        (url-request-extra-headers (cdr (assoc method couchdb-request-headers)))
        (url-request-data data))
    (couchdb-response (url-retrieve-synchronously url))))

;; Cross-database

(defun couchdb-list-dbs ()
  "Request a list of databases."
  (couchdb-request "GET" (couchdb-url "_all_dbs")))

;; Per-database

(defun couchdb-db-create (db)
  "Create a new database named DB."
  (couchdb-request "PUT" (couchdb-url db)))

(defun couchdb-db-info (db)
  "Fetch information about the database named DB."
  (couchdb-request "GET" (couchdb-url db)))

(defun couchdb-db-delete (db)
  "Delete the database named DB."
  (couchdb-request "DELETE" (couchdb-url db)))

;; Document

(defun couchdb-doc-list (db)
  "List the documents available in DB."
  (couchdb-request "GET" (couchdb-url (format "%s/_all_docs" db))))

(defun couchdb-doc-info (db docid)
  "Fetch the document in DB whose id is DOCID."
  (couchdb-request "GET" (couchdb-url (format "%s/%s" db docid))))

(defun couchdb-doc-save (db doc &optional docid)
  "Save to DB the document DOC.
If DOCID is non-null, update the document with that ID. Otherwise,
create a new document."
  (let ((doc (json-encode doc)))
    (if docid
        (couchdb-request "PUT" (couchdb-url (format "%s/%s" db docid)) doc)
      (couchdb-request "POST" (couchdb-url (format "%s/" db)) doc))))

(defun couchdb-doc-delete (db docid)
  "Delete from DB the document identified by DOCID."
  (couchdb-request "DELETE" (couchdb-url (format "%s/%s" db docid))))

(provide 'couchdb)
;;; couchdb.el ends here
