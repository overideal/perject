;;; perject-ibuffer.el --- Ibuffer integration for Perject -*- lexical-binding: t -*-


;;; Commentary:

;; Integrate perject with ibuffer.
;; This allows the user to filter by project and to add to and remove buffers
;; from projects in the ibuffer interface.


;;; Code:

(require 'perject)
(require 'ibuffer)
(require 'ibuf-ext)


;;;; Customization

(defgroup perject-ibuffer nil
  "Integrate `perject' with `ibuffer'."
  :group 'perject
  :prefix "perject-ibuffer-")

(defcustom perject-ibuffer-buffer-to-project-message t
  "If non-nil, print a message when adding a buffer from a project in `ibuffer'.
This also influences removing buffers from a project in `ibuffer'.
The commands affected are `perject-ibuffer-add-to-project' and
`perject-ibuffer-remove-from-project'."
  :type '(choice
		  (const :tag "Print message" t)
		  (const :tag "Don't print message" nil)))

(defcustom perject-ibuffer-update-after-buffer-to-project t
  "If non-nil, update ibuffer after adding a buffer from a project in `ibuffer'.
The update also happens when removing buffers from a project in `ibuffer'.
This influences the commands `perject-ibuffer-add-to-project' and
`perject-ibuffer-remove-from-project'."
  :type '(choice
		  (const :tag "Update IBuffer" t)
		  (const :tag "Don't update IBuffer" nil)))


;;;; Filters

;;;###autoload (autoload 'ibuffer-filter-by-collection "perject-ibuffer")
(define-ibuffer-filter collection
	"Limit current view to buffers belonging to collection QUALIFIER.
If QUALIFIER is nil, only show the anonymous buffers; i.e. those not
belonging to any project."
  (:description
   "collection"
   :reader
   (perject--get-collection-name "Filter by collection: "
						 'active nil t nil
						 "No collection active" #'ignore))
  (if qualifier
	  (perject-is-assoc-with buf qualifier)
	(perject-anonymous-buffer-p buf)))

;;;###autoload (autoload 'ibuffer-filter-by-project "perject-ibuffer")
(define-ibuffer-filter project
	"Limit current view to buffers belonging to project QUALIFIER.
If QUALIFIER is nil, only show the anonymous buffers; i.e. those not
belonging to any project.
The user is asked for the project, defaulting to those projects belonging to the
current collection. With a prefix argument, the user may choose from all
projects."
  (:description
   "project"
   :reader
   (perject--get-project-name "Filter by project: "
							  (if current-prefix-arg 'all 'current) nil t nil
							  "No project active in the collection" #'ignore))
  (if qualifier
	  (perject-is-assoc-with buf qualifier)
	(perject-anonymous-buffer-p buf)))

;;;###autoload
(defun perject-ibuffer-enable-filter-by-collection (&optional name)
  "Enable the filter `ibuffer-filter-by-collection' for the collection named NAME.
This means that only the buffers that belong to a project of that collection are
shown. If NAME is nil, it defaults to the current collection.
This also disables all previous filters by project or collection."
  (let ((name (or name (car (perject-current))))
		(current-filter (alist-get 'collection ibuffer-filtering-qualifiers))
		(current-filter-proj (alist-get 'project ibuffer-filtering-qualifiers)))
	(setq ibuffer-filtering-qualifiers
		  (assoc-delete-all 'project ibuffer-filtering-qualifiers))
	(when (and current-filter (not (equal current-filter name)))
	  (setq ibuffer-filtering-qualifiers
			(assoc-delete-all 'collection ibuffer-filtering-qualifiers)))
	(if (equal current-filter name)
		(setq current-filter nil)
	  (ibuffer-filter-by-collection name))
	(when (or current-filter current-filter-proj)
	  (ibuffer-update nil t))))

;;;###autoload
(defun perject-ibuffer-enable-filter-by-project (&optional proj)
  "Enable the filter `ibuffer-filter-by-project' for the project PROJ.
This means that only the buffers of the project PROJ are shown.
PROJ may be nil or a dotted pair with car a collection name and cdr a project
name. If nil, PROJ defaults to the current project.
This also disables all previous filters by project or collection."
  (let ((proj (or proj (perject-current)))
		(current-filter (alist-get 'project ibuffer-filtering-qualifiers))
		(current-filter-col (alist-get 'collection ibuffer-filtering-qualifiers)))
	(setq ibuffer-filtering-qualifiers
		  (assoc-delete-all 'collection ibuffer-filtering-qualifiers))
	(when (and current-filter (not (equal current-filter proj)))
	  (setq ibuffer-filtering-qualifiers
			(assoc-delete-all 'project ibuffer-filtering-qualifiers)))
	(if (equal current-filter proj)
		(setq current-filter nil)
	  (ibuffer-filter-by-project proj))
	(when (or current-filter current-filter-col)
	  (ibuffer-update nil t))))


;;;; Commands

;;;###autoload
(defun perject-ibuffer-add-to-project (proj)
  "Add the marked buffers or the buffer at point to the project PROJ.
PROJ is a dotted pair with car a collection and cdr a project name.
In interactive use, PROJ defaults to the current project. If the current frame
is not associated with any project or if a single prefix argument is supplied,
let the user choose a project from the current collection. In any other case the
user may choose from the list of all projects from all active collections."
  (interactive
   (list (if (and (not current-prefix-arg) (cdr (perject-current)))
			 (perject-current)
		   (perject--get-project-name
            "Add marked buffers to project: "
			(if (equal current-prefix-arg '(4)) 'current 'all)
			nil t nil
			(if (and (equal current-prefix-arg '(4)) (perject-current))
				"The current collection has no projects"
			  "No projects exist")
			"No project specified"))))
  (let ((buffers (ibuffer-marked-buffer-names)))
    (if buffers
        (let ((buffers-added nil))
          (dolist (buffer-name buffers)
            (let ((buffer (get-buffer buffer-name)))
              (when (and (not (perject-is-assoc-with buffer proj))
                         (buffer-live-p buffer))
                (perject-add-buffer-to-project buffer proj nil)
                (push buffer-name buffers-added))))
          (pcase buffers-added
            ('nil
             (message "No buffers added. All selected buffers are already associated with the project '%s'."
                      (perject-project-to-string proj)))
            (`(,buffer)
             (message "Added buffer '%s' to project '%s'." buffer (perject-project-to-string proj)))
            (_
             (message "Added the following buffers to project '%s': %s."
                      (perject-project-to-string proj) (string-join buffers-added ", ")))))
      (perject-add-buffer-to-project
       (ibuffer-current-buffer t) proj perject-ibuffer-buffer-to-project-message))
	(when perject-ibuffer-update-after-buffer-to-project
	  (ibuffer-update nil t))))

;;;###autoload
(defun perject-ibuffer-remove-from-project (proj)
  "Remove the marked buffers or the buffer at point from the project named NAME.
PROJ is a dotted pair with car a collection and cdr a project name.
In interactive use, PROJ defaults to the current project. If the current frame
is not associated with any project or if a prefix argument is supplied, let the
user choose a project from the current collection. In any other case the user
may choose from the list of all projects from all active collections."
  (interactive
   (list (if (and (not current-prefix-arg) (cdr (perject-current)))
			 (perject-current)
		   (perject--get-project-name
            "Remove marked buffers from project: "
		    (if (equal current-prefix-arg '(4)) 'current 'all)
			nil t nil
			(if (and (equal current-prefix-arg '(4)) (perject-current))
				"The current collection has no projects"
			  "No projects exist")
			"No project specified"))))
  (let ((buffers (ibuffer-marked-buffer-names)))
    (if buffers
        (let ((buffers-removed nil))
          (dolist (buffer-name buffers)
            (let ((buffer (get-buffer buffer-name)))
              (when (perject-is-assoc-with buffer proj)
                (perject-remove-buffer-from-project buffer proj nil)
                (push buffer-name buffers-removed))))
          (pcase buffers-removed
            ('nil
             (message "No buffers removed. None of the selected buffers were associated with the project '%s'."
                      (perject-project-to-string proj)))
            (`(,buffer)
             (message "Removed buffer '%s' from project '%s'." buffer (perject-project-to-string proj)))
            (_
             (message "Removed the following buffers from project '%s': %s."
                      (perject-project-to-string proj) (string-join buffers-removed ", ")))))
      (perject-remove-buffer-from-project
       (ibuffer-current-buffer t) proj perject-ibuffer-buffer-to-project-message))
	(when perject-ibuffer-update-after-buffer-to-project
	  (ibuffer-update nil t))))

;;;###autoload
(defun perject-ibuffer-print-buffer-projects ()
  "Print the names of the projects with which the buffer at point is associated."
  (interactive)
  (perject-print-buffer-projects (ibuffer-current-buffer t)))


(provide 'perject-ibuffer)
;;; perject-ibuffer.el ends here
