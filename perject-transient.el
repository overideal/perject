;;; perject-transient.el --- Transient commands for perject -*- lexical-binding: t -*-


;;; Commentary:

;; Provides a user interface to core commands of perject using transient.el.
;; This includes commands for closing, reloading and deleting collections.


;;; Code:

(require 'perject)
(require 'transient)
(require 'cl-lib)
(require 'seq)


;;;; Customization

(defface perject-current-face '((t :inherit font-lock-keyword-face))
  "The face used for displaying the the current collection or project.")

(defcustom perject-close-default '(t t t)
  "The default values for the command `perject-close'.
The value of this variable must be a list with three elements:
(save kill-frames kill-buffers)

Their values may be as follows:

save:
- nil: Don't save the collections.
- t: Save the collections.

kill-frames:
- nil: Keep the frames.
- t: Delete the frames belonging to the collection, unless all frames belong to
  it.

kill-buffers:
- nil: Keep the buffers.
- t: Kill all buffers belonging to the closed collection and to no other
  collection or project.
- 'all: Kill all buffers belonging to the closed collection.

Every element of the list may also be a function, in which case it is called
when closing the collection with the collection name as its only argument and
its return value (which must be one of the above values) is interpreted
accordingly.

If you need more sophisticated control over the frames and buffers, set the
above values to nil and use `perject-after-close-hook'."
  :type '(list
		  (choice
		   (const :tag "Don't save the collections" nil)
		   (const :tag "Save the collections" t)
		   (function :tag "Custom function"))
		  (choice
		   (const :tag "Keep the frames" nil)
		   (const :tag "Delete the frames belonging to the collection, unless
		   all frames belong to it" t)
		   (function :tag "Custom function"))
		  (choice
		   (const :tag "Keep the buffers" nil)
		   (const :tag "Kill all buffers belonging to the closed collection and
  to no other collection or project" t)
		   (const :tag "Kill all buffers belonging to the closed collection" all)
		   (function :tag "Custom function"))))

(defcustom perject-reload-default '(t t)
  "The default values for the command `perject-reload'.
The value of this variable must be a list with two elements:
(kill-frames kill-buffers)

Their values may be as follows:

kill-frames:
When reloading, the frames may be reused for the reloaded collection.
Only the frames that have not been reused are affected by this variable.
- nil: After reloading, remove the frames that were not reused from the
  collection (but do not delete them).
- 'keep: Keep the frames and their associations with the collection or one of
  its projects.
- t: After reloading, delete the frames that have not been reused. In case these
  are all existent frames, remove them from the collection instead.

kill-buffers:
- nil: Keep the buffers.
- t: Before reloading the collection, kill all its buffers that do not belong to
  any other collection or project.
- 'all: Before reloading the collection, kill all its buffers.

Every element of the list may also be a function, in which case it is called
when reloading the collection with the collection name as its only argument and
its return value (which must be one of the above values) is interpreted
accordingly.

If you need more sophisticated control over the frames and buffers, set the
above values above to nil and use `perject-after-reload-hook'."
  :type '(list
		  (choice
		   (const :tag "After reloading, remove the frames that were not reused
  from the collection (but do not delete them)" nil)
		   (const :tag "Keep the frames and their associations with the
		   collection or one of its projects" keep)
		   (const :tag "After reloading, delete the frames that have not been
  reused" t)
		   (function :tag "Custom function"))
		  (choice
		   (const :tag "Keep the buffers" nil)
		   (const :tag "Before reloading the collection, kill all its buffers
  that do not belong to any other collection or project" t)
		   (const :tag "Before reloading the collection, kill all its buffers" all)
		   (function :tag "Custom function"))))

(defcustom perject-delete-default '(t t keep t)
  "The default values for the command `perject-delete'.
The value of this variable must be a list with two elements:
(kill-frames kill-buffers kill-frames kill-buffers)

The first two elements have an effect when deleting collections and the other
two elemens influence deleting projects.

Their values may be as follows:

kill-frames:
- nil: Keep the frames.
- t: Delete the frames belonging to the collection, unless all frames belong to
  it.

kill-buffers:
- nil: Keep the buffers.
- t: Kill all buffers belonging to the deleted collection or project and to no
  other collection or project.
- 'all: Kill all buffers belonging to the deleted collection or project.

The second kill-frames may also have the value 'keep, in which case the frames
are removed from the deleted project but not from its collection (and are not
killed).
Every element of the list may also be a function, in which case it is called
when reloading the collection with the collection name as its only argument and
its return value (which must be one of the above values) is interpreted
accordingly.

If you need more sophisticated control over the frames and buffers, set the
above values above to nil and use `perject-after-delete-collection-hook' or
`perject-after-delete-project-hook'."
  :type '(list
		  (choice
		   (const :tag "Keep the frames when deleting a collection" nil)
		   (const :tag "Delete the frames belonging to the collection, unless
  all frames belong to it" t)
		   (function :tag "Custom function"))
		  (choice
		   (const :tag "Keep the buffers when deleting a collection" nil)
		   (const :tag "Kill all buffers belonging to the deleted collection and
  to no other collection or project" t)
		   (const :tag "Kill all buffers belonging to the deleted collection" all)
		   (function :tag "Custom function"))
		  (choice
		   (const :tag "Keep the frames when deleting a project, but remove them
 from the collection" nil)
		   (const :tag "Delete the frames belonging to the project, unless
  all frames belong to it" t)
		   (const :tag "Keep the frames and their association with the
  collection of the deleted project" keep)
		   (function :tag "Custom function"))
		  (choice
		   (const :tag "Keep the buffers when deleting a project" nil)
		   (const :tag "Kill all buffers belonging to the deleted project and
  to no other collection or project" t)
		   (const :tag "Kill all buffers belonging to the deleted project" all)
		   (function :tag "Custom function"))))

(defcustom perject-confirm-delete t
  "When non-nil, the user is asked for confirmation before a collection or project is deleted."
  :type '(choice
		  (const :tag "Ask for confirmation before deleting a collection or project" t)
		  (const :tag "Don't ask for confirmation before deleting a collection or project" nil)))


;;;; Internal Variables

(defvar perject--transient nil
  "A list used in the various transient functions of `perject'.")


;;;; Commands

;;;###autoload (autoload 'perject-close "perject-transient" nil t)
(transient-define-prefix perject-close ()
  "Transient menu to close an active collection.
This closes all projects belonging to the collection.

This function runs the hooks `perject-before-close-hook' and
`perject-after-close-hook'."
  [:description
   (lambda ()
	 (concat
	  "Close collection: "
	  (propertize (car perject--transient) 'face 'perject-current-face)))
   [""
	"Actions"
	("RET" "Confirm" perject--apply)
	("c" "Select a different collection" perject--choose-collection :transient t)
	""
	"Options"
	("s" (lambda () (if (cadr perject--transient) "Save collection" "Do not save collection"))
	 perject--close-toggle-save :transient t)
	("f" (lambda ()
		   (let ((transient-current-command 'perject-close))
			 (perject--kill-frames-info (car perject--transient) (nth 2 perject--transient))))
	 perject--cycle-frames :transient t)
	("b" (lambda ()
		   (let ((transient-current-command 'perject-close))
			 (perject--kill-buffer-info
			  (car perject--transient) (nth 3 perject--transient))))
	 perject--cycle-buffers :transient t)]]
  (interactive)
  (unless (and (listp perject-close-default) (eq (length perject-close-default) 3))
	(user-error "The variable `perject-close-default' has an invalid format"))
  (let ((transient-current-command 'perject-close))
	(perject--choose-collection)))

;;;###autoload (autoload 'perject-reload "perject-transient" nil t)
(transient-define-prefix perject-reload ()
  "Transient menu to reload an active collection.
This discards any changes to the collection and reverts it to the state from the
previous save. This is achieved by closing and reopening the collection.
Frames belonging to the collection are reused.

This function runs the hooks `perject-before-reload-hook' and
`perject-after-reload-hook'."
  [:description
   (lambda ()
	 (concat
	  "Reload collection: "
	  (propertize (car perject--transient) 'face 'perject-current-face)))
   [""
	"Actions"
	("RET" "Confirm" perject--apply)
	("c" "Select a different collection" perject--choose-collection :transient t)
	""
	"Options"
	("f" (lambda ()
		   (pcase (cadr perject--transient)
			 ('nil "After reloading, remove the frames that were not reused from the collection (but do not delete them)")
			 ('keep "Keep the frames and their associations with the collection or one of its projects")
			 (_ "After reloading, delete the frames that have not been reused (unless all frames belong to it)")))
	 perject--cycle-frames :transient t)
	("b" (lambda ()
		   (let ((transient-current-command 'perject-reload))
			 (perject--kill-buffer-info
			  (car perject--transient) (nth 2 perject--transient))))
	 perject--cycle-buffers :transient t)]]
  (interactive)
  (unless (and (listp perject-reload-default) (eq (length perject-reload-default) 2))
	(user-error "The variable `perject-reload-default' has an invalid format"))
  (let ((transient-current-command 'perject-reload))
	(perject--choose-collection)))

;;;###autoload (autoload 'perject-delete "perject-transient" nil t)
(transient-define-prefix perject-delete ()
  "Transient menu to delete a collection or project.
Deleting a collection encompasses closing the collection (if active) and
deleting all its projects and the corresponding desktop file.
Without a prefix argument, the user is asked for a collection to delete.
Otherwise, the user is asked for a project.

When deleting a collection, this function runs the hooks
`perject-before-delete-collection-hook' and
`perject-after-delete-collection-hook'. When deleting a project it runs
`perject-before-delete-project-hook' and `perject-after-delete-project-hook'
instead."
  [:description
   (lambda ()
	 (let ((col-or-proj (car perject--transient)))
	   (if (stringp col-or-proj)
		   (concat "Delete "
				   (unless (perject-collection-p col-or-proj 'active) "inactive ")
				   "collection: "
				   (propertize col-or-proj 'face 'perject-current-face))
		 (concat "Delete project: "
				 (propertize (perject-project-to-string col-or-proj)
							 'face 'perject-current-face)))))
   ""
   "Actions"
   ("RET" "Confirm" perject--apply)
   ("c" "Select a different collection" perject--choose-collection :transient t)
   ("p" "Select a different project" perject--choose-project :transient t)]
  ["Options"
   :if (lambda ()
		 (let ((name (car perject--transient)))
		   (or (not (stringp name)) (perject-collection-p name 'active))))
   ("f" (lambda ()
		  (let ((transient-current-command 'perject-delete))
			(perject--kill-frames-info (car perject--transient) (cadr perject--transient))))
	perject--cycle-frames :transient t)
   ("b" (lambda ()
		  (let ((transient-current-command 'perject-delete))
			(perject--kill-buffer-info (car perject--transient)
									   (nth 2 perject--transient))))
	perject--cycle-buffers :transient t)]
  (interactive)
  (unless (and (listp perject-delete-default) (eq (length perject-delete-default) 4))
	(user-error "The variable `perject-delete-default' has an invalid format"))
  (let ((transient-current-command 'perject-delete))
	(if current-prefix-arg
		(perject--choose-collection)
	  (perject--choose-project))))


;;;; Helper Functions

(defun perject--apply ()
  "Apply the action to the collection or project."
  (interactive)
  (pcase-exhaustive transient-current-command
	('perject-close
	 (apply #'perject-close-collection perject--transient))
	('perject-reload
	 (apply #'perject-reload-collection perject--transient))
	('perject-delete
	 (let* ((col-or-proj (car perject--transient))
			(col-p (stringp col-or-proj)))
	   (when (or (not perject-confirm-delete)
				 (y-or-n-p
				  (format
				   "Deleting %s. Are you sure?"
				   (if col-p
					   (concat "collection '" col-or-proj "'")
					 (concat "project '" (perject-project-to-string col-or-proj) "'"))
				   col-or-proj)))
	 (apply (if col-p #'perject-delete-collection #'perject-delete-project)
			perject--transient))))))

(defun perject--choose-collection ()
  "Let the user choose a collection."
  (interactive)
  (unless (memq transient-current-command '(perject-close perject-reload perject-delete))
	(user-error "This function may only be called within transient"))
  (let ((name (perject--get-collection-name
			   "Select collection: " (if (eq transient-current-command 'perject-delete) 'all 'active)
			   nil t (car (perject-current))
			   "There currently is no collection"
			   "No collection specified"))
		(default (pcase-exhaustive transient-current-command
				   ('perject-close perject-close-default)
				   ('perject-reload perject-reload-default)
				   ('perject-delete (seq-take perject-delete-default 2)))))
	(setq perject--transient
		  (cons name
				(mapcar (lambda (value)
						  (if (functionp value) (funcall value name) value))
						default))))
  (transient-setup transient-current-command))

(defun perject--choose-project ()
  "Let the user choose a project."
  (interactive)
  (unless (eq transient-current-command 'perject-delete)
	(user-error "This function may only be called within transient"))
  (let ((proj (perject--get-project-name
			   "Select project: " 'all nil t (perject-current)
			   "There currently is no project to delete"
			   "No project specified")))
	(setq perject--transient
		  (cons proj
				(mapcar (lambda (value)
						  (if (functionp value) (funcall value proj) value))
						(seq-drop perject-delete-default 2)))))
  (transient-setup 'perject-delete))

(defun perject--close-toggle-save ()
  "Toggle whether the collection should be saved or not."
  (interactive)
  (setf (cadr perject--transient) (not (cadr perject--transient))))

(defun perject--cycle-frames ()
  "Cycle whether the frames should be killed or not."
  (interactive)
  (pcase-exhaustive transient-current-command
	('perject-close (setf (nth 2 perject--transient) (not (nth 2 perject--transient))))
	((or 'perject-reload (and 'perject-delete (guard (consp (car perject--transient)))))
	 (setf (cadr perject--transient)
		   (pcase (cadr perject--transient)
			 ('nil 'keep)
			 ('keep t)
			 (_ nil))))
	('perject-delete (setf (cadr perject--transient) (not (cadr perject--transient))))))

(defun perject--cycle-buffers ()
  "Cycle whether the buffers should be killed or not."
  (interactive)
  (let ((index (pcase-exhaustive transient-current-command
				 ('perject-close 3)
				 ((or 'perject-reload 'perject-delete) 2))))
	(setf (nth index perject--transient)
		  (pcase (nth index perject--transient)
			('nil t)
			('all nil)
			(_ 'all)))))

(defun perject--kill-buffer-info (name state)
  "Return a string describing the state STATE for the collection named NAME."
  (let ((strings
		 (pcase-exhaustive transient-current-command
		   ((or 'perject-close 'perject-delete)
			(list "Keep all buffers"
				  "Kill the %s belonging to '%s'"
				  "Kill the %s belonging only to '%s' and to no other collection or project"))
		   ('perject-reload
			(list "Keep all buffers"
				  "Before reloading, kill the %s belonging to '%s'"
				  "Before reloading, kill the %s belonging only to '%s' and to no other collection or project")))))
	(pcase state
	  ('nil (car strings))
	  ('all (let ((buffers (perject-get-buffers name)))
			  (format (cadr strings)
					  (if (eq (length buffers) 1)
						  (concat "buffer '" (buffer-name (car buffers)) "'")
						(format "%s buffers" (length buffers)))
					  (if (stringp name) name (perject-project-to-string name)))))
	  (_ (let ((buffers
				(cl-remove-if-not
				 (if (stringp (car perject--transient))
					 (lambda (buf)
					   (cl-every (apply-partially #'string-equal name)
								 (mapcar #'car (buffer-local-value 'perject-buffer buf))))
				   (lambda (buf)
					 (cl-every (apply-partially #'equal name) (buffer-local-value 'perject-buffer buf))))
				 (perject-get-buffers name))))
		   (format (caddr strings)
				   (if (eq (length buffers) 1)
					   (concat "buffer '" (buffer-name (car buffers)) "'")
					 (format "%s buffers" (length buffers)))
				   (if (stringp name) name (perject-project-to-string name))))))))

(defun perject--kill-frames-info (proj state)
  "Return a string describing the state STATE for the collection or project PROJ."
  (let ((strings
		 (pcase-exhaustive transient-current-command
		   ((or 'perject-close (and 'perject-delete (guard (stringp proj))))
			(list "Keep the frames" nil "Delete the %s belonging to '%s'"))
		   ('perject-delete
			(list "Keep the frames, but remove them from the collection"
				  "Keep the %s with the collection '%s'"
				  "Delete the %s belonging to '%s'")))))
	(if state
		(let ((frames (perject-get-frames proj)))
		  (if (and (eq state 'keep)
				   (eq transient-current-command 'perject-delete)
				   (consp proj))
			  (format (cadr strings)
					  (if (eq (length frames) 1)
						  "frame and its association"
						(format "%s frames and their associations" (length frames)))
					  (car proj))
			(if (eq (length (frame-list)) frames)
				"Keep the frames (cannot delete all Emacs frames)"
			  (format (caddr strings)
					  (if (eq (length frames) 1)
						  "frame"
						(format "%s frames" (length frames)))
					  (if (stringp proj) proj (perject-project-to-string proj))))))
	  (car strings))))


(provide 'perject-transient)
;;; perject-transient.el ends here
