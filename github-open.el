;;; github-open.el --- raku raku blame

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

(defun github-open--chomp (str)
  (replace-regexp-in-string "[\n\r]" "" str))

(defun github-open--git-repository? ()
  (let* ((cmd "git rev-parse --is-inside-work-tree")
         (project (github-open--chomp (shell-command-to-string cmd))))
    (string= project "true")))

(defun github-open--commit? (commit-id)
  (let ((uncommit-id "0000000000000000000000000000000000000000"))
    (not (string= uncommit-id commit-id))))

(defun github-open--at-commit-id ()
  (let* ((blame-cmd "git blame -l -L %s,+1  %s | cut -d ' ' -f 1")
         (cmd (format blame-cmd (line-number-at-pos) buffer-file-name)))
    (github-open--chomp (shell-command-to-string cmd))))

(defun github-open--url ()
  (let* ((host "git config --get remote.origin.url")
         (github-ssh-url-regex "git@github.com:\\(.*\\)\\/\\(.*\\).git$")
         (github-https-url-regex "https:\\/\\/.*\\/\\(.*\\)\\/\\(.*\\)\\.git$")
         (github-url "https://github.com/%s/%s/commit/%s")
         (url (shell-command-to-string host))
         (commit-id (github-open--at-commit-id)))
    (if (and (or (string-match github-https-url-regex url)
                 (string-match github-ssh-url-regex url))
             (github-open--commit? commit-id))
        (let ((user (match-string 1 url))
              (repo (match-string 2 url)))
          (format github-url user repo commit-id))
      (message (concat "Unknown host" host)))))

(defun github-open ()
  (interactive)
  (if (github-open--git-repository?)
      (let ((cmd (concat "open " (github-open--url))))
        (shell-command-to-string cmd))
    (message "this file is not git repository")))

(provide 'github-open)

;; github-open.el ends here
