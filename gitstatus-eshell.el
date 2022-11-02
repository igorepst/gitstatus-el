;;; gitstatus-eshell.el --- Front-end for `eshell' and `gitstatusd' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Igor Epstein

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `eshell' front-end for `gitstatusd'.
;; This is an extra package to allow adding the information to the `eshell' prompt.

;;; Code:

(require 'gitstatusd)
(require 'gitstatus)
(require 'em-prompt)
(eval-when-compile (require 'cl-lib))


;;; Customizable variables

(defgroup gitstatus-eshell nil
  "`eshell' front-end for `gitstatusd'."
  :group 'gitstatus)

(defcustom gitstatus-eshell-neighbour-regex "\\( [$#]\\)"
  "Neighbour of the `gitstatus' in `eshell' prompt."
  :type 'string
  :group 'gitstatus-eshell)

(defcustom gitstatus-eshell-is-neighbour-append nil
  "Whether to append (or prepend) the `gitstatus' to the `eshell' prompt."
  :type 'boolean
  :group 'gitstatus-eshell)

(defcustom gitstatus-eshell-prompt-lines 1
  "Search for `gitstatus-eshell-neighbour-regex' in this many lines."
  :type 'integer
  :group 'gitstatus-eshell)


;;; Internal variables

(defvar-local gitstatus-eshell--req-id nil "`gitstatusd' request ID.")


;;; Public interface

;;;###autoload
(defun gitstatus-eshell-start ()
  "Run `gitstatusd' to get the `gitstatus' information."
  (when gitstatus-eshell--req-id
    (gitstatusd-remove-callback gitstatus-eshell--req-id))
  (setq gitstatus-eshell--req-id
	(gitstatusd-get-status default-directory #'gitstatus-eshell-build)))

;;;###autoload
(defun gitstatus-eshell-build (res)
  "Build `eshell' prompt based on `gitstatusd' result, represented by RES."
  (let ((buf (gitstatus-eshell--get-buffer res)))
    (when buf
      (with-current-buffer buf
	(save-mark-and-excursion
	  (let ((msg (gitstatus-build-str res)))
	    (when (gitstatus--string-not-empty-p msg)
	      (save-match-data
		(let ((place (gitstatus-eshell--find-place)))
		  (when place
		    (forward-char place)
		    (let* ((pos (point))
			   (inhibit-read-only t))
		      (insert (concat " " msg))
		      (add-text-properties pos (+ 1 pos (length msg))
					   '(read-only t
						       front-sticky (face read-only)
						       rear-nonsticky (face read-only))))))))))))))


;;; Utility functions

(defun gitstatus-eshell--get-buffer (res)
  "Return the buffer the request came from with result RES."
  (let ((res-id (gitstatusd-req-id res)))
    (cl-dolist (buf (buffer-list))
      (when (string-equal res-id
			  (buffer-local-value 'gitstatus-eshell--req-id buf))
	(cl-return buf)))))

(defun gitstatus-eshell--find-place ()
  "Find the right place in `eshell' prompt."
  (goto-char (point-max))
  (re-search-backward eshell-prompt-regexp nil t 1)
  (let ((cnt gitstatus-eshell-prompt-lines)
	(mstart (match-beginning 0))
	(place))
    (when (and mstart (> cnt 0))
      (while (> cnt 0)
	(let* ((start (if (= cnt gitstatus-eshell-prompt-lines) mstart (line-beginning-position)))
	       (end (if (= cnt gitstatus-eshell-prompt-lines) (match-end 0) (line-end-position))))
	  (string-match gitstatus-eshell-neighbour-regex (buffer-substring start end))
	  (setq place
		(if gitstatus-eshell-is-neighbour-append
		    (match-end 1)
		  (match-beginning 1)))
	  (if place
	      (setq cnt 0)
	    (forward-line -1)
	    (beginning-of-line)
	    (setq cnt (1- cnt))))))
    place))

(provide 'gitstatus-eshell)
;;; gitstatus-eshell.el ends here
