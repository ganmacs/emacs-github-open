;;; github-commit-open.el --- raku raku blame

;; Copyright (C) 2015 ganmacs

;; Author: ganmacs <ganmacs_at_gmail.com>
;; Maintainer: ganmacs <ganmacs_at_gmail.com>
;; URL: https://github.com/ganmacs/emacs-git-grep
;; Version: 0.0.2
;; Keywords: git

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defun github-commit-open--chomp (str)
  (replace-regexp-in-string "[\n\r]" "" str))

(defun github-commit-open--git-repository? ()
  (let* ((cmd "git rev-parse --is-inside-work-tree")
         (project (github-commit-open--chomp (shell-command-to-string cmd))))
    (string= project "true")))

(defun github-commit-open--commit? (commit-id)
  (let ((uncommit-id "0000000000000000000000000000000000000000"))
    (not (string= uncommit-id commit-id))))

(defun github-commit-open--chomp-suffix(suffix str)
  (let ((pos (- (length suffix))))
    (if (string= suffix (substring str pos))
        (substring str 0 pos)
      str)))

(defun github-commit-open--at-commit-id ()
  (let* ((blame-cmd "git blame -l -L %s,+1  %s | cut -d ' ' -f 1")
         (cmd (format blame-cmd (line-number-at-pos) buffer-file-name)))
    (github-commit-open--chomp (shell-command-to-string cmd))))

(defun github-commit-open--get-url ()
  (let ((url "git config --get remote.origin.url"))
    (github-commit-open--chomp-suffix
     ".git"
     (github-commit-open--chomp
      (shell-command-to-string url)))))

(defun github-commit-open--url ()
  (let* ((github-url-pattern "^\\(\\(https\\|ssh\\):\\/\\/\\)?\\(git@\\)?github\\.com\\(\\/\\|:\\)\\(.+\\)\\/\\(.+\\)$")
         (github-url "https://github.com/%s/%s/commit/%s")
         (url (github-commit-open--get-url))
         (commit-id (github-commit-open--at-commit-id)))
    (if (and (string-match github-url-pattern url)
             (github-commit-open--commit? commit-id))
        (let ((user (match-string 5 url))
              (repo (match-string 6 url)))
          (format github-url user repo commit-id))
      (message (concat "Unknown url : " url)))))

(defun github-commit-open ()
  (interactive)
  (if (github-commit-open--git-repository?)
      (let ((cmd (concat "open " (github-commit-open--url))))
        (shell-command-to-string cmd))
    (message "This directory is not git repository.")))

(provide 'github-commit-open)

;; github-commit-open.el ends here
