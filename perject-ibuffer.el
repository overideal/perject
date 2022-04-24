;;; perject-ibuffer.el --- Ibuffer integration for Perject -*- lexical-binding: t -*-


;;; Commentary:

;; Integrate perject with ibuffer.
;; This allows the user to filter by project and to add to and remove buffers
;; from projects in the ibuffer interface.


;;;  Code

(require 'perject)
(require 'ibuffer)
(require 'ibuf-ext)


;;;; Customization

(defcustom perject-ibuffer-buffer-to-project-message t
  "If non-nil, print a message when adding or removing a buffer from a project in `ibuffer'.
This influences the commands `perject-ibuffer-add-to-project' and
`perject-ibuffer-remove-from-project'."
  :type '(choice
		  (const :tag "Print message" t)
		  (const :tag "Don't print message" nil)))

(defcustom perject-ibuffer-update-after-buffer-to-project t
  "If non-nil, update the ibuffer list after adding or removing a buffer from a project in `ibuffer'.
This influences the commands `perject-ibuffer-add-to-project' and
`perject-ibuffer-remove-from-project'."
  :type '(choice
		  (const :tag "Update IBuffer" t)
		  (const :tag "Don't update IBuffer" nil)))


;;;; Functions

;;;###autoload (autoload 'ibuffer-filter-by-name "ibuf-ext")
(define-ibuffer-filter project
	"Limit current view to buffers belonging to project QUALIFIER.
If QUALIFIER is nil, only show the anonymous buffers; i.e. those not
belonging to any project."
  (:description
   "project name"
   :reader
   (perject--get-project-name "Filter by project name: "
						 'active nil t nil
						 "No project active" #'ignore))
  (if qualifier
	  (perject--is-assoc-with buf qualifier)
	(perject--is-buffer-anonymous buf)))


(defun perject-ibuffer-enable-filter-by-project (&optional name)
  "Enable the filter `ibuffer-filter-by-project' for the project named NAME.
This also disables all previous filters by project.
This means that only the buffers matching the current project are shown. 
If nil, NAME defaults to the current project. If there is no current project,
ensure that there is no filter by project."
  (let ((name (or name (perject--current-project)))
		(current-filter (alist-get 'project ibuffer-filtering-qualifiers)))
	(cond
	 ((and name current-filter)
	  (unless (string-equal current-filter name)
		(setf (alist-get 'project ibuffer-filtering-qualifiers) name)
		(ibuffer-update nil t)))
	 (name
	  (ibuffer-filter-by-project name))
	 (current-filter
	  (setq ibuffer-filtering-qualifiers
			(assoc-delete-all 'project ibuffer-filtering-qualifiers))
	  (ibuffer-update nil t)))))

(defun perject-ibuffer-add-to-project (name)
  "Add the marked buffers or the buffer at point to the project named NAME.
If NAME is nil, ask the user for the project.
In interactive use, NAME defaults to the current project. If the current frame
is not associated with any project or if a prefix argument is supplied, let the
user choose the project."
  (interactive (list (and (not current-prefix-arg) (perject--current-project))))
  (let ((name (or
               name
               (perject--get-project-name
                "Add marked buffers to project: "
                'active nil t nil "There currently is no active project"
				"No project specified")))
        (buffers (ibuffer-marked-buffer-names)))
    (if buffers
        (let ((buffers-added nil))
          (dolist (buffer-name buffers)
            (let ((buffer (get-buffer buffer-name)))
              (when (and (not (perject--is-assoc-with buffer name))
                         (buffer-live-p buffer))
                (perject-add-buffer-to-project buffer name nil)
                (push buffer-name buffers-added))))
          (pcase buffers-added
            ('nil
             (message "No buffers added. All selected buffers are already associated with the project '%s'."
                      name))
            (`(,buffer)
             (message "Added buffer '%s' to project '%s'." buffer name))
            (_
             (message "Added the following buffers to project '%s': %s."
                      name (string-join buffers-added ", ")))))
      (perject-add-buffer-to-project
       (ibuffer-current-buffer t) name perject-ibuffer-buffer-to-project-message))
	(when perject-ibuffer-update-after-buffer-to-project
	  (ibuffer-update nil t))))

(defun perject-ibuffer-remove-from-project (name)
  "Remove the marked buffers or the buffer at point from the project named NAME.
If NAME is nil, ask the user for the project.
In interactive use, NAME defaults to the current project. If the current frame
is not associated with any project or if a prefix argument is supplied, let the
user choose the project."
  (interactive (list (and (not current-prefix-arg) (perject--current-project))))
  (let ((name (or
               name
               (perject--get-project-name
                "Remove marked buffers from project: "
                'active nil t nil "There currently is no active project"
				"No project specified")))
        (buffers (ibuffer-marked-buffer-names)))
    (if buffers
        (let ((buffers-removed nil))
          (dolist (buffer-name buffers)
            (let ((buffer (get-buffer buffer-name)))
              (when (perject--is-assoc-with buffer name)
                (perject-remove-buffer-from-project buffer name nil)
                (push buffer-name buffers-removed))))
          (pcase buffers-removed
            ('nil
             (message "No buffers removed. None of the selected buffers were associated with the project '%s'."
                      name))
            (`(,buffer)
             (message "Removed buffer '%s' from project '%s'." buffer name))
            (_
             (message "Removed the following buffers from project '%s': %s."
                      name (string-join buffers-removed ", ")))))
      (perject-remove-buffer-from-project
       (ibuffer-current-buffer t) name perject-ibuffer-buffer-to-project-message))
	(when perject-ibuffer-update-after-buffer-to-project
	  (ibuffer-update nil t))))

(defun perject-ibuffer-print-buffer-projects ()
  "Print the names of the projects with which the buffer at point is associated."
  (interactive)
  (perject-print-buffer-projects (ibuffer-current-buffer t)))


(provide 'perject-ibuffer)
;;; perject-ibuffer.el ends here
