;;; gitstatus.el --- Common front-end for `gitstatusd' -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Igor Epstein

;; Author: Igor Epstein <igorepst@gmail.com>
;; Version: 0.1.0
;; Keywords: tools, processes
;; URL: https://github.com/igorepst/gitstatus-el
;; Package-Requires: ((emacs "25.1"))

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

;; Common front-end for `gitstatusd'.
;; Extracts the Git status information and returns it usually fontified and
;; suitable for end-user presentation, for example, as a part of a prompt.
;; Additional package for the `eshell' prompt is provided for your convenience.

;; Note, the package wraps an external executable called "gitstatusd"
;; (see <https://github.com/romkatv/gitstatus>).  The executable is distributed
;; under GNU General Public License v3.0 and provides a much faster alternative
;; to "git status" and "git describe".  It works on Linux, macOS, FreeBSD, Android,
;; WSL, Cygwin and MSYS2.  The original work includes scripts to be used in
;; Bash and Zsh shells' prompts.
;; The executable has to be installed on the end-user machine for the package
;; to work.

;;; Code:

(require 'gitstatusd)
(eval-when-compile (require 'cl-lib))


;;; Customizable variables

(defgroup gitstatus nil
  "Front-end for `gitstatusd'."
  :group 'gitstatusd)

(defcustom gitstatus-prefix "on "
  "Prefix to prepend to the `gitstatus'."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-suffix nil
  "Suffix to append to the `gitstatus'."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-branch-truncate-after 30
  "Truncate branch name after this much characters."
  :type 'integer
  :group 'gitstatus)

(defcustom gitstatus-branch-truncation-sep "…"
  "In case the branch name is truncated, use this as a separator."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-branch-icon ""
  "Icon for the branch."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-tag-icon ""
  "Icon for the tag."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-hash-icon ""
  "Icon for the hash."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-default-remote-icon ""
  "Default icon for the remote URL."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-remote-icons '(("github" "") ("bitbucket" "") ("stash" "") ("gitlab" ""))
  "Icons for the remote URLs."
  :type '(alist :key-type (group string) :value-type (group string)))

(defcustom gitstatus-upstream-sep ":"
  "Separator between local and upstream branch."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-commit-behind-icon "⇣"
  "Icon for the number of commits the current branch is behind upstream."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-commit-ahead-icon "⇡"
  "Icon for the number of commits the current branch is ahead of upstream."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-push-commit-behind-icon "⇠"
  "Icon for the number of commits the current branch is behind push remote."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-push-commit-ahead-icon "⇢"
  "Icon for the number of commits the current branch is ahead of push-remote."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-stash-icon "*"
  "Icon for the number of stashes."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-conflict-icon "~"
  "Icon for the number of conflicted changes."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-staged-icon "+"
  "Icon for the number of staged changes."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-unstaged-icon "!"
  "Icon for the number of unstaged changes."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-untracked-icon "?"
  "Icon for the number of untracked files."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-unstaged-unknown-icon "─"
  "Icon when the number of unstaged changes is unknown."
  :type 'string
  :group 'gitstatus)

(defcustom gitstatus-is-fontify t
  "Whether to fontify the `gitstatus' string."
  :type 'boolean
  :group 'gitstatus)


;;; Faces

(defgroup gitstatus-faces nil
  "Faces used by `gitstatus'."
  :group 'gitstatus
  :group 'faces)

(defface gitstatus-default-face
  '((t (:inherit default)))
  "Default face for `gitstatus'."
  :group 'gitstatus-faces)

(defface gitstatus-clean-face
  '((t (:inherit success)))
  "Clean face for `gitstatus'."
  :group 'gitstatus-faces)

(defface gitstatus-modified-face
  '((t (:inherit font-lock-constant-face)))
  "Modified face for `gitstatus'."
  :group 'gitstatus-faces)

(defface gitstatus-untracked-face
  '((t (:inherit font-lock-comment-face)))
  "Untracked face for `gitstatus'."
  :group 'gitstatus-faces)

(defface gitstatus-conflicted-face
  '((t (:inherit warning)))
  "Conflicted face for `gitstatus'."
  :group 'gitstatus-faces)


;;; Macros and defsubst

(defsubst gitstatus--string-not-empty-p (string)
  "Check whether STRING is not null and not empty."
  (not (or (null string) (string-equal string ""))))

(defsubst gitstatus--fontify (str face)
  "Return STR that was possibly fontified with FACE."
  (if gitstatus-is-fontify (propertize str 'face face) str))

(defmacro gitstatus--push-prop (val sym msgl face)
  "Helper to add VAL and SYM to MSGL.

Propertize with FACE if needed."
  `(when (and (gitstatus--string-not-empty-p ,val) (not (string-equal "0" ,val)))
     (let ((msg (gitstatus--fontify (concat ,sym ,val) ,face)))
       (push msg ,msgl))))


;;; Public interface

;;;###autoload
(defun gitstatus-build-str (res)
  "Build `gitstatus' string from RES."
  (when (string-equal "1" (gitstatusd-is-git-repo res))
    (let ((branch (gitstatus--get-branch-name res))
	  (case-fold-search nil)
	  (wip (string-match "[^[:alnum:]]\*\\(wip\\|WIP\\)[^[:alnum:]]\*" (gitstatusd-commit-msg-par res)))
	  (msgl-s (mapconcat 'identity (reverse (gitstatus--get-counters res)) " ")))
      (concat
       (when (gitstatus--string-not-empty-p gitstatus-prefix) (gitstatus--fontify gitstatus-prefix 'gitstatus-default-face))
       (gitstatus--fontify (concat (gitstatus--get-remote-icon res) " ") 'gitstatus-clean-face)
       branch
       (when wip (concat " " (gitstatus--fontify "wip" 'gitstatus-modified-face)))
       (when (gitstatus--string-not-empty-p msgl-s) (concat " " msgl-s))
       (when (gitstatus--string-not-empty-p gitstatus-suffix) (gitstatus--fontify gitstatus-suffix 'gitstatus-default-face))))))


;;; Utility functions

(defun gitstatus--get-counters (res)
  "Get counters according to RES."
  (let ((msgl (list))
	(unstaged (gitstatusd-unstaged-num res)))
    (gitstatus--push-prop (gitstatusd-commit-behind-num res) gitstatus-commit-behind-icon msgl 'gitstatus-clean-face)
    (gitstatus--push-prop (gitstatusd-commit-ahead-num res) gitstatus-commit-ahead-icon msgl 'gitstatus-clean-face)
    (gitstatus--push-prop (gitstatusd-push-commit-behind-num res) gitstatus-push-commit-behind-icon msgl 'gitstatus-clean-face)
    (gitstatus--push-prop (gitstatusd-push-commit-ahead-num res) gitstatus-push-commit-ahead-icon msgl 'gitstatus-clean-face)
    (gitstatus--push-prop (gitstatusd-stash-num res) gitstatus-stash-icon msgl 'gitstatus-clean-face)
    (gitstatus--push-prop (gitstatusd-repo-state res) nil msgl 'gitstatus-conflicted-face)
    (gitstatus--push-prop (gitstatusd-conflict-num res) gitstatus-conflict-icon msgl 'gitstatus-conflicted-face)
    (gitstatus--push-prop (gitstatusd-staged-num res) gitstatus-staged-icon msgl 'gitstatus-modified-face)
    (gitstatus--push-prop unstaged gitstatus-unstaged-icon msgl 'gitstatus-modified-face)
    (gitstatus--push-prop (gitstatusd-untrack-num res) gitstatus-untracked-icon msgl 'gitstatus-untracked-face)
    (when (string-equal "-1" unstaged)
      (push (gitstatus--fontify gitstatus-unstaged-unknown-icon 'gitstatus-modified-face) msgl))
    msgl))

(defun gitstatus--get-remote-icon (res)
  "Get branch name according to RES."
  (let ((remote-url (gitstatusd-remote-url res))
	(icon))
    (when remote-url
      (cl-dolist (rem gitstatus-remote-icons)
	(when (string-match-p (car rem) remote-url)
	  (setq icon (car (cdr rem)))
	  (cl-return))))
    (if icon icon gitstatus-default-remote-icon)))

(defun gitstatus--get-branch-name (res)
  "Get branch name according to RES."
  (let ((branch (gitstatusd-local-branch res))
	(up-branch (gitstatusd-upstream-branch res))
	(ret))
    (if (gitstatus--string-not-empty-p branch)
	(setq ret
	      (gitstatus--fontify
	       (concat gitstatus-branch-icon " " (gitstatus--branch-truncate branch))
	       'gitstatus-clean-face))
      (setq ret (gitstatusd-last-tag res))
      (if (gitstatus--string-not-empty-p ret)
	  (setq ret
		(gitstatus--fontify
		 (concat gitstatus-tag-icon " " (gitstatus--branch-truncate ret))
		 'gitstatus-clean-face))
	(setq ret
	      (gitstatus--fontify
	       (concat gitstatus-hash-icon " " (substring (gitstatusd-commit-hash res) 0 7))
	       'gitstatus-clean-face))))
    (when (and (gitstatus--string-not-empty-p up-branch) (not (string-equal up-branch branch)))
      (setq ret (concat ret
			(gitstatus--fontify gitstatus-upstream-sep 'gitstatus-default-face)
			(gitstatus--fontify (gitstatus--branch-truncate up-branch) 'gitstatus-clean-face))))
    ret))

(defun gitstatus--branch-truncate (branch)
  "Truncate the name of the BRANCH."
  (if (< (length branch) gitstatus-branch-truncate-after)
      branch
    (let ((end
	   (if (> 15 gitstatus-branch-truncate-after) 3
	     (if (> 30 gitstatus-branch-truncate-after) 7 10))))
      (concat (substring branch 0 end) gitstatus-branch-truncation-sep (substring branch (- 0 end))))))

(provide 'gitstatus)
;;; gitstatus.el ends here
