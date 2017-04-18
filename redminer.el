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


(defun http-get (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (goto-char (search-forward-regexp "^$"))
    (prog1 (json-read) (kill-buffer))))


(defun lookup-project-id ()
  "list of project names using helm"
  ;; (let ((projects (mapcar (lambda (project) (list (alist-get 'name project) (alist-get 'id project)))(alist-get 'projects (http-get "http://demo.redmine.org/projects.json")))))
  (setq projects
        (mapcar (lambda (project)
                  (list
                   (alist-get 'name project)
                   (alist-get 'id project)))
                (alist-get 'projects (http-get "http://demo.redmine.org/projects.json"))))

  (helm :sources `((name . "projects")
                   (candidates . ,(mapcar (lambda (project) (car project)) projects))
                   (action . (lambda (candidate) (cadr (assoc candidate projects)))))))


(defun redminer-fetch-issue-as-org-link ()
  "Insert issue subject and url as org formated link at point."
  (interactive)
  (if (boundp 'redminer-hostname)
      (let* ((project-id (lookup-project-id))
             (url (format "http://demo.redmine.org/issues.json?project_id=%s" project-id))
             (issues (alist-get 'issues (http-get url))))
        (helm :sources (helm-build-sync-source "heading"
                         :candidates (append issues nil)
                         :candidate-transformer #'(lambda (issues)
                                                    (mapcar #'(lambda (issue)
                                                                (list (alist-get 'subject issue)
                                                                      (format "http://demo.redmine.org/issues/%d" (alist-get 'id issue))
                                                                      (alist-get 'description issue)))
                                                            issues))
                         :action (lambda (issue) (insert (format "[[%s][%s]]" (car issue) (cadr issue)))))))
    (message "no hostname defined. please bind a redmine hostname to redminer-hostname.")))


(provide 'redminer)
;;; redminer.el ends here
