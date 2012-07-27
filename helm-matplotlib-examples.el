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

(defun helm-matplotlib-examples ()
  "Choose matplotlib examples."
  (interactive)
  (let ((abks:books-dir hme:data-directory))
    (helm-books-command)))

(defun anything-matplotlib-examples ()
  "Choose matplotlib examples."
  (interactive)
  (let ((abks:books-dir hme:data-directory))
    (anything-books-command)))

(provide 'helm-matplotlib-examples)

;;; helm-matplotlib-examples.el ends here
