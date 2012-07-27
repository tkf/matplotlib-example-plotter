;;; helm-matplotlib-examples.el --- View matplotlib example figures

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; helm-matplotlib-examples.el is free software: you can redistribute
;; it and/or modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.

;; helm-matplotlib-examples.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with helm-matplotlib-examples.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provide two commands: `helm-matplotlib-examples' for
;; helm user and `anything-matplotlib-examples' for anything user.

;;; Code:

(require 'anything-books)

;; (defmacro hme:aif (test-form then-form &rest else-forms)
;;   "Anaphoric IF.  Adapted from `e2wm:aif'."
;;   (declare (debug (form form &rest form))
;;            (indent 2))
;;   `(let ((it ,test-form))
;;      (if it ,then-form ,@else-forms)))

(defmacro hme:aand (test &rest rest)
  "Anaphoric AND.  Adapted from `e2wm:aand'."
  (declare (debug (form &rest form)))
  `(let ((it ,test))
     (if it ,(if rest (macroexpand-all `(hme:aand ,@rest)) 'it))))

(defcustom hme:data-directory
  (hme:aand
   (or (hme:aand load-file-name (file-name-directory it)) default-directory)
   (concat (file-name-as-directory it) "build"))
  "Directories where PDF figures are stored."
  :group 'hme)

(defvar hme:actions
  '(("Open code" . hme:open-code)))

(defun hme:open-code (pdf-path)
  "Given a PDF-PATH, open associated Python file."
  (let* ((dir (replace-regexp-in-string "-[0-9]+\\.pdf$" "/" pdf-path))
         (file-glob (concat dir "*.py")))
    (find-file file-glob t)))

(defun hme:view-examples (command)
  (let ((abks:books-dir hme:data-directory)
        (anything-books-actions (append hme:actions anything-books-actions)))
    (funcall command)))

(defun helm-matplotlib-examples ()
  "Choose matplotlib examples."
  (interactive)
  (hme:view-examples #'helm-books-command))

(defun anything-matplotlib-examples ()
  "Choose matplotlib examples."
  (interactive)
  (hme:view-examples #'anything-books-command))

(provide 'helm-matplotlib-examples)

;;; helm-matplotlib-examples.el ends here
