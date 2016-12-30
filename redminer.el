;;; redminer.el --- redmine client                   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Graf

;; Author:  <m@grafoo.at>
;; Keywords:tools

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

;; 

;;; Code:

(defun redminer-get-issue-subject (id)
  "make a rest call"
  (interactive "sredmine id: ")
  (setq response
        (progn
          (setq url
                (format "http://www.redmine.org/issues/%s.json" id))
          (with-current-buffer (url-retrieve-synchronously url)
            (goto-char (point-min))     ;set point to first char of buffer
            (goto-char (search-forward-regexp "^$")) ;get point of empty line between header and body
            (prog1 (json-read) (kill-buffer))))) ;kill buffer after reading from begin of body to end of buffer
  (setq subject (alist-get 'subject (alist-get 'issue response)))
  (insert subject))


(provide 'redminer)
;;; redminer.el ends here

