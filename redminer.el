;;; redminer.el --- Search redmine host for Projects and Issues using helm.
;; 
;; Filename: redminer.el
;; Description: Search redmine host for Projects and Issues using helm.
;; Author: gertsch@gertsch.cc & m@grafoo.at
;; Created: Wed Apr 19 22:52:48 2017 (+0200)
;; 
;;; Commentary:
;; Use function "redminer-lookup" to start
;; or bind it to a key.
;; e.g. (global-set-key (kbd "C-c r") 'redminer-lookup)
;; requires redminer-hostname and redminer-key to be set.
;; (setq redminer-hostname "http://demo.redmine.org")
;; (setq redminer-key nil)
;; optional INT redminer-default-project
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'helm)


;; (setq redminer-hostname "http://demo.redmine.org")
;; (setq redminer-key nil)
;; (setq redminer-default-project-id 0) ;; optional
(load-file "~/.config/redminer/config.el")


(defun http-get (url)
  "Make a HTTP GET request and return a JSON object of URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (goto-char (search-forward-regexp "^$"))
    (prog1 (json-read) (kill-buffer))))


(defun get-redmine-data (section limit offset id)
  "List of project names using helm; takes SECTION, LIMIT, OFFSET and project ID."
  (if id
      (cond
       ((equal 'integer (type-of id))
	(alist-get section
		   (http-get (format "%s/%s.json?key=%s&limit=%s&offset=%s&project_id=%s"
				     redminer-hostname section redminer-key limit offset id))))
       ((equal 'string (type-of id))
	(alist-get 'issue
		   (http-get (format "%s/issues/%s.json?key=%s&limit=%s&offset=%s&include=%s"
				     redminer-hostname section redminer-key limit offset id)))))
    (mapcar (lambda (project)
	      (list
	       (alist-get 'name project)
	       (alist-get 'id project)))
	    (alist-get section
		       (http-get (format "%s/%s.json?key=%s&limit=%s&offset=%s&project_id=%s"
					 redminer-hostname section redminer-key limit offset id))))))


(defun redmine-grabber (section id)
  "Get all name and id pairs of given SECTION.  Takes project ID."
  (let (
	(r '())
	(i 0)
	(project_name (car id))
	(id (cadr id)))
    (if id
	(if (equal 'string (type-of id))
	    (setq r (get-redmine-data section 1 i id))
	  (while (append (get-redmine-data section 1 i id) nil)
	    (setq r (append r (append (get-redmine-data section 100 i id) nil)))
	    (setq i (+ i 100))))
      (while (get-redmine-data section 1 i id)
	(setq r (append r (get-redmine-data section 100 i id)))
	(setq i (+ i 100))))
    (list project_name r)))


(defun redminer-lookup ()
  "Search redmine host for Projects and Issues."
  (interactive)
  (if (not (condition-case err
	       (cond ((not (boundp 'redminer-hostname)) (error "STRING 'redminer-hostname not set"))
		     ((not (boundp 'redminer-key)) (error "INT/SYMBOL(nil) 'redminer-key not set")))
	     (error (princ (format "Error: %s" err)))))
      (if (boundp 'redminer-issues)
	  (lookup-issues-in-project redminer-issues)
	(if (boundp 'redminer-default-project-id)
	    (lookup-issues-in-project
	     (redmine-grabber 'issues
			      (list
			       (get-name-from-id
				(cadr (redmine-grabber 'projects nil))
				redminer-default-project-id)
			       redminer-default-project-id)))
	  (lookup-redmine-projects)))))


(defun lookup-redmine-projects ()
  "List of project names using helm."
  (helm :sources (helm-build-sync-source (format "Projects of %s" redminer-hostname)
			 :candidate-number-limit 999
			 :candidates (let ((x (mapcar (lambda (a)
							(list (car a)
							 (list (car a) (car (cdr a)))))
						      (cadr (redmine-grabber 'projects nil)))))
				       x)
			 :action (helm-make-actions
				  "List Issues" (lambda (project)
				    (lookup-issues-in-project (redmine-grabber 'issues (car project))))
				  "Save Issues of Project" (lambda (project)
							     (setq redminer-issues (redmine-grabber 'issues (car project)))
							     (setq redminer-default-project-id (cadr (car project)))
							     (print (format "Issues of %s saved" (car (car project)))))))))


(defun lookup-issues-in-project (issues)
  "List ISSUES in given project."
  (helm :sources (helm-build-sync-source (format "Issues of %s" (car issues))
		   :candidate-number-limit 999
		   :candidates (cadr issues)
		   :candidate-transformer (lambda (issues)
					    (mapcar (lambda (issue)
						      (list (format "%d :: %s :: %s :: %s"
								    (alist-get 'id issue)
								    (alist-get 'name (alist-get 'status issue))
								    (alist-get 'subject issue)
								    (alist-get 'name (alist-get 'assigned_to issue)))
							    (format "%s/issues/%d"
								    redminer-hostname
								    (alist-get 'id issue))
							    (alist-get 'subject issue)
							    (alist-get 'description issue)
							    (alist-get 'id issue)))
						    issues))
		   :action (helm-make-actions
			    "Open URL in Browser" (lambda (issue) (browse-url (car issue)))
			    "Insert into org" (lambda (issue) (insert (format "[[%s][%s]]" (car issue) (cadr issue))))
			    "List Projects" (lambda (i) (lookup-redmine-projects))
			    "Show Issue in Buffer" (lambda (issue) (show-redmine-issue-inbuffer issue))
			    ;; "Print issue" (lambda (issue)
			    ;; 		    (print
			    ;; 		     (redmine-grabber (cadr (cdr (cdr issue))) (list (cadr issue) "journals"))))
			    ;; "Print candidate" (lambda (candidate) (print candidate))
			    ))))

(defun show-redmine-issue-inbuffer (issue)
  "Show ISSUE in buffer."
  (let* ((i (redmine-grabber (cadr (cdr (cdr issue))) (list (cadr issue) "journals")))
	 )
    (get-buffer-create "redminer")
    (with-output-to-temp-buffer "redminer"
      (mapc (lambda (a)
	      (princ
	       (format "* %s\n%s\n"
		       (car a)
		       (cdr a))
	       )
	      )
	    (car (cdr i))))
    (with-current-buffer "redminer" (org-mode))
    ))


(defun get-name-from-id (data id)
  "Return NAME from DATA\(STRING INT) with given ID\(INT)."
  (catch 'name
    (mapc (lambda (d)
	    (when (eq id (cadr d))
		(throw 'name (car d))))
	  data)))


;; (defun get-id-from-name (name)
;;   "Get ID from NAME."
;;   (catch 'id
;;     (mapc (lambda (data)
;; 	    (when (string= name (car data))
;; 		(throw 'id (cadr data))))
;; 	  (redmine-grabber))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; redminer.el ends here
