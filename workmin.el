;; This package allows the user to manage multiple projects in a single Emacs instance.
;;
;; Each project consists of a list of buffers and a list of frames,
;; which have corresponding window configurations.
;; Every project but the 'nil' one (corresponding to the non-associated buffers) is assumed to have at least one frame.

;; List frames: frame-list or frames-on-display-list
;; (add-to-list 'window-persistent-parameters (cons 'workspace 'writable))

;; check which buffers are in first and second project:
;; (cl-remove-if-not (lambda (buffer) (member buffer (cdadr workmin-projects))) (cdar workmin-projects))

;; TODO: modifying the buffer list from workmin--get-buffers: does this change the metadata?

;; auto-add-buffers variable (project . buffer-name)
;; rename to perview


;; difference prefix arg, current prefix arg

;; allow restoring of previously opened projects

(require 'desktop)
(require 'cl-lib) ;; for remove-if-not and some other stuff

(defvar workmin-projects nil ;; '(("test"))
  "A list representing the projects.
Each element is a list, where the car (first element) is a string,
which is the name of the project and the cdr is the list of buffers
belonging to that project.
Every project also has a list of associated frames.")


;; (defvar workmin-default-project nil
;;   "The name of the project, which the starting frame will be dedicated to.
;; If nil, the starting frame is dedicated to no project.")
;; use? initial-frame-alist


(defface workmin-mode-line-face
  '((t :foreground "orange")) ;; gold also nice; do nothing: '((t (nil)))
  "The face used by the mode line indicator of workmin."
  :group 'workmin)

;; TODOO (not implemented, should be implemented also for created projects...)
;; (defvar workmin-auto-add-buffers '(t . "*scratch*")
;;   "A list representing buffers and projects to be associated with each other.
;; Each element is a cons cell.
;; The car is a project name, a list of project names or t, the latter of which
;; is interpreted to mean all projects. The cdr is a buffer name.
;; The project or projects named as in car are associated with a buffer named as in
;; cdr.
;; Nothing happens if a project name does not correspond to an active project
;; or the buffer name does not correspond to an existing buffer.
;; It only makes sense to add buffer names to this list which cannot be restored by
;; desktop.")

;; There is no hook that is run after an arbitrary buffer is created.
;; See: https://stackoverflow.com/questions/7899949/is-there-an-emacs-hook-that-runs-after-every-buffer-is-created
(defvar workmin-auto-add-in-hooks '(find-file-hook dired-mode-hook)
  "A list of hooks, in which the current buffer is added to the current project.
This is used to automatically add newly created buffers to the current project.
The following hooks could be interesting to the user:
`change-major-mode-hook', `find-file-hook', `buffer-list-update-hook' and many
mode hooks.
Internally, the function `workmin--auto-add-buffer-to-project' is used.")

(defvar workmin-close-project-save 'ask
  "This variable controls if a project is saved when being closed using `workmin-close-project'.
It can have one of three values:
- t: The project is saved.
- 'ask: The user is asked if the project should be saved.
- nil: The project is not saved.")

(defvar workmin-switch-project-message t
  "If non-nil, the command `workmin-switch-project' will print a message upon completion.")

(defvar workmin-add-buffer-message t
  "If non-nil, the command `workmin-add-buffer-to-project' will print a message upon completion.")

(defvar workmin-remove-buffer-message t
  "If non-nil, the commands `workmin-remove-buffer-from-project' and
`workmin-remove-buffer-from-project-and-kill' will print a message upon completion.")

(defvar workmin-close-project-confirmation nil
  "If non-nil, the user is asked for confirmation before closing a project
with `workmin-close-project'.
Otherwise, the user is not asked for confirmation.")

(defvar workmin-delete-project-confirmation t
  "If non-nil, the user is asked for confirmation before deleting a project
with `workmin-delete-project'.
Otherwise, the user is not asked for confirmation.")

(defvar workmin-kill-buffers-confirmation t
  "If non-nil, the user is asked for confirmation before killing all the buffers
associated with a project.
Otherwise, the user is not asked for confirmation.
This affects `workmin-clear-buffers', `workmin-close-project' and
`workmin-delete-project'.")

(defvar workmin-empty-project-delete 'ask
  "This variable controls what happens when the last buffer is removed from a project.
It can have one of three values:
- t: The project is deleted.
- 'ask: The user is asked whether to delete the project or keep it.
- nil: The project is kept.")

(defvar workmin-load-on-startup '("lisp")
  "The variable controls which projects are automatically loaded at startup.
It may have one of the following values:
- nil: Load no project.
- A function: Call that function with a single argument, namely the list
  of saved projects. The function should return a list of project names
  in the form as described below. It could for example ask the user.
- A list of project names: Load all the specified projects.
  If a name in the list does not correspond to an existing project, do nothing.
  As a special case, the first element of the list may be nil. In that case all
  projects but the ones in the list are loaded at startup.
- t: Load all projects.
Note that regardless of the value of this variable, whether the buffers
which were associate with no project, are loaded, is determined by the
variable `workmin-load-non-project'.")

(defvar workmin-save-on-exit t
  "The variable controls which projects are automatically saved when exiting Emacs.
It may have one of the following values:
- nil: Load no project.
- A function: Call that function with a single argument, namely the list
  of active projects. The function should return a list of project names
  in the form as described below. It could for example ask the user.
- A list of project names: Save all the specified projects.
  If a name in the list does not correspond to an existing, active project,
  do nothing.
  As a special case, the first element of the list may be nil. In that case all
  projects but the ones in the list are saved upon exiting Emacs.
- t: Save all projects.
Note that regardless of the value of this variable, whether the buffers
which were associate with no project, are saved, is determined by the
variable `workmin-save-non-project'.")

(defvar workmin-load-non-project t
  "When non-nil, the buffers, which were associate with no project, are loaded at startup.
Otherwise, they are not loaded at startup, but may be manually loaded later by invoking
`workmin-load-non-project'.")

(defvar workmin-save-non-project t
  "When non-nil, the buffers, which were associate with no project, are saved when exiting Emacs.
Otherwise, they are not saved when exiting Emacs, but may be manually saved by invoking
`workmin-save-non-project'.")


(defvar workmin-mode-line-format t
  "This variable determines the format of the mode line indicator of workmin.
Its value may be of the following type:
- nil: No mode line entry is shown for workmin.
- A function: Call that function with a single string as an argument,
  which is the name of the current project.
  The function should return a string, which is then displayed in the mode line.
  Note that this function will get nil as an argument if the current frame is
  not associated with any project.
- t: Use the default format, which is simply the name of the project with face
`workmin-mode-line-face'.")

(defvar workmin-frame-title-format t
  "This variable determines the format of the title of a frame.
Its value may be of the following type:
- nil: The title of a frame is not altered by workmin.
- A function: Call that function with a single string as an argument,
  which is the name of the current project.
  The function should return a valid value for `frame-title-format',
  which is used to alter the frame title.
  See the documentation of that variable.
  Note that this function will get nil as an argument if the current frame is
  not associated with any project.
- t: Use the default format, which looks like \"emacs@computer:project\".
Note that this variable has no effect on manually named sessions,
because it is used with `frame-title-format'. See its documentation.")

(defvar workmin-directory "~/.emacs.d/workmin/"
  "The directory used by workmin to save the data.")
(unless (file-exists-p workmin-directory)
  (make-directory workmin-directory))

(defvar workmin-valid-naming-chars '(?_ ?- ? )
  "A list of characters, that may be used when naming a project.
All letters and digits are always allowed.
Note that the name of the project has to also be a valid (possibly non-existent)
directory name, so be careful.
By default, this variable allows '_', '-' and ' '.
This variable is used in the function `workmin--valid-project-name'.")

(defvar workmin-project-name-hist nil
  "The history of project names.")

;; I am surprised such a variable does not already exist in `frameset.el'
(defvar workmin--desktop-restored-frames nil
  "A list of the frames, which were restored for the most recent project.
Should not be modified by the user.")

(defvar workmin--desktop-current-project nil
  "Internal variable used to assign the correct project to newly restored buffers and frames.
Should not be modified by the user.")

(defvar workmin--frame-title-backup nil
  "Internal variable used to save the value of `frame-title-format'; to restore it later.
Should not be modified by the user.")



(define-minor-mode workmin-mode
  "Toggle workmin mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When workmin mode is enabled, all the features of workmin are
usable."
  ;; TODO overwork doc
  :global t
  :keymap nil
  ;; :lighter " wm"
  (if workmin-mode
      (progn
        (add-hook 'after-init-hook 'workmin--init)
        (add-hook 'kill-emacs-hook 'workmin--exit)
        (add-hook 'kill-buffer-hook 'workmin-remove-buffer-from-all-projects)
;;        (add-hook 'delete-frame-functions )

        (when after-init-time
          ;; This means the mode got enabled and the init phase is already over.
          ;; I.e. later, manually by the user.
          ;; In that case, `after-init-hook' is not run.
          (dolist (hook workmin-auto-add-in-hooks)
            (add-hook hook 'workmin--auto-add-buffer-to-project)))
        
        ;; The mode line knows to which mode this belongs to, and if the mode is not active, the entry is not shown.
        (unless (or (not workmin-mode-line-format) (assoc 'workmin-mode mode-line-misc-info))
          (if (functionp workmin-mode-line-format)
              (push '(workmin-mode (:eval (funcall workmin-mode-line-format (workmin--current-project))))
                    (cdr (last mode-line-misc-info)))
            (push '(workmin-mode (:eval (workmin-mode-line-indicator)))
                  (cdr (last mode-line-misc-info)))))
        (when workmin-frame-title-format
          (setq
           workmin--frame-title-backup
           frame-title-format
           frame-title-format
           (if (functionp workmin-frame-title-format)
               (funcall workmin-frame-title-format (workmin--current-project))
             '("" invocation-name "@" system-name
               (:eval (or (and (workmin--current-project)
                               (concat ":" (workmin--current-project)))))))))
    )

    ;; Remove the added hooks.
    (remove-hook 'after-init-hook 'workmin--init)
    (remove-hook 'kill-emacs-hook 'workmin--exit)
    (remove-hook 'kill-buffer-hook 'workmin-remove-buffer-from-all-projects)
    (dolist (hook workmin-auto-add-in-hooks)
            (remove-hook hook 'workmin--auto-add-buffer-to-project))
    (setq frame-title-format workmin--frame-title-backup
          workmin--frame-title-backup nil))
  )

;; TODO: implement reloading; maybe even put it in a new function
;; (because like this, the check for "already opened" is done 2 times)
;; At the moment, if there are multiple frames belonging to the project,
;; just a "random" one is selected. Could introduce another frame parameter
;; to let the user decide which frame is primary.
(defun workmin-open-project (arg)
  "Ask the user for a project, open it and switch to one of its corresponding frames.
The switching might not work, depending on the window manager.
This means that all its buffers are restored and frames are opened from the
corresponding desktop file.
When ARG is nil, the user is restricted to non-open projects.
Otherwise, the user can reopen (reload) a project.
To restore the buffers and frames belonging to no project, run
`workmin-load-non-project-buffers'."
  (interactive "P")
  (let ((name (workmin--get-saved-project
               "Open project: "
               "No project to open."
               (and (not arg)
                    (lambda (name)
                      (not (member name (workmin--active-projects)))))))
        (frame (make-frame)))
    ;; (with-selected-frame frame
    ;;   (workmin--desktop-read name))
    (select-frame frame)
    (workmin--desktop-read name)
    (delete-frame frame)
    ;; TODO: for some reason this does not have any effect on my wm (Kde Plasma)
    (when workmin--desktop-restored-frames
      (select-frame (car workmin--desktop-restored-frames)))))

(defun workmin-close-project (name &optional arg)
  "Close the project named NAME.
Remove the project from the list of active projects and close its frames.
In interactive use, the user is asked for NAME.
ARG determines what happens to the buffers belonging to the project.
If ARG is nil (in interactive use: no prefix argument), no buffers are killed.
If ARG is '(4) (a single prefix argument), the buffers, which do not belong to
any other project but the selected one, are killed.
Otherwise, all buffers belonging to the project are killed.
Depending on the value of the variable `workmin-close-project-save',
the project is saved to a desktop file or not.
If the variable `workmin-close-project-confirmation' is non-nil,
the user is asked for confirmation before the project is deleted.
Whether the user is asked for confirmation an additional time if
he wants to kill associated buffers, is controlled by the
variable `workmin-kill-buffers-confirmation'.
An error is thrown if there is no project to close or all the open frames belong
to the selected project."
  (interactive
   (list
    (workmin--get-active-project
     "Close project: " nil
     "There currently is no project to close.")
    current-prefix-arg))
  (when (or (not workmin-close-project-confirmation)
            (y-or-n-p (format "Closing project '%s'. Are you sure? " name)))
    (when (eq (length (workmin--get-project-frames name)) (length (frames-on-display-list)))
      (user-error "Can't close a project which belongs to all open frames."))
    (when (or (eq workmin-close-project-save t)
              (and (eq workmin-close-project-save 'ask)
                   (y-or-n-p (format "Save the project '%s'? " name))))
      (workmin--desktop-save name))
    (workmin--desktop-release-lock name)
    (let ((buffers (workmin--get-buffers name)))
      (setq workmin-projects (assoc-delete-all name workmin-projects))
      (dolist (frame (workmin--get-project-frames name))
        (delete-frame frame))
      (when (and arg
                 (or (not workmin-kill-buffers-confirmation)
                     (y-or-n-p (format "Kill buffers belonging to project '%s'?" name))))
        (if (equal arg '(4))
            ;; Determine the buffers, which are not associated with any other project,
            ;; but the selected one.
            (dolist (buffer (cl-intersection buffers (workmin--get-buffers-not-assoc)))
              (kill-buffer buffer))
          (dolist (buffer buffers)
            (kill-buffer buffer)))))))


;; Maybe allow opening in the same frame? But not sure what to do with the frame parameters.
(defun workmin-create-project (name)
  "Create a project named NAME and open a frame for it.
In interactive use, the user is asked for the name and the entered string must
pass `workmin-valid-project-name' and there must not be an existing project of
the same name. Otherwise, an error is thrown."
  (interactive
   (list
    (workmin--get-new-project-name "Create project: ")))
  (setq workmin-projects (cons (list name) workmin-projects))
  (workmin-new-frame name))


(defun workmin-delete-project (name &optional arg)
  "Delete the project named NAME.
This includes closing the project and deleting the corresponding desktop file.
In interactive use, the user is asked for NAME.
ARG determines what happens to the buffers belonging to the project.
If ARG is nil (in interactive use: no prefix argument), no buffers are killed.
If ARG is '(4) (a single prefix argument), the buffers, which do not belong to
any other project but the selected one, are killed.
Otherwise, all buffers belonging to the project are killed.
If the variable `workmin-delete-project-confirmation' is non-nil,
the user is asked for confirmation before the project is deleted.
Whether the user is asked for confirmation an additional time if
he wants to kill associated buffers, is controlled by the
variable `workmin-kill-buffers-confirmation'.
An error is thrown if there is no project to delete or all the open frames
belong to the selected project."
  (interactive
   (list
    (workmin--get-project
     "Delete project: " nil
     "There currently is no project to delete.")
    current-prefix-arg))
  (when (or (not workmin-delete-project-confirmation)
            (y-or-n-p (format "Deleting project '%s'. Are you sure? " name)))
    ;; If the project is active, close it.
    (when (member name (workmin--active-projects))
      (let ((workmin-close-project-save nil)
            (workmin-close-project-confirmation nil))
        (workmin-close-project name arg)))
    (when (file-exists-p (workmin--get-project-directory name))
      (delete-directory (workmin--get-project-directory name) t))))


;; (defun workmin-switch-project (name)
;;   "Switch the project of the current frame to the project named NAME.
;; In interactive use, the user is asked for NAME."
;;   ;; This currently does not allow the user to create a new project this way. Maybe add a variable? TODO
;;   (interactive
;;    (list
;;     (workmin--get-active-project
;;      "Switch to project: " nil
;;      "There currently is no project to switch to. Create one using `workmin-create-project'.")))
;;   (workmin--set-current-project name)
;;   (when workmin-switch-project-message
;;     (message "Switched to project '%s'." name)))



(defun workmin-new-frame (name)
  "Create a new frame for the project named NAME and switch to it.
In interactive use, if a prefix argument is supplied or the current
frame is not associated with any project, ask the user for NAME,
presenting the currently active projects as candidates.
Otherwise, use the current project."
  (interactive
   (list
    (if (or current-prefix-arg
            (not (workmin--current-project)))
        (workmin--get-active-project "Create new frame for project: " nil
                                     "No project to create a frame for.")
      (workmin--current-project))))
  (let ((frame (make-frame (list (cons 'workmin-project name)))))
    (select-frame frame)))
  

;; (defun workmin-switch-project-new-frame (name)
;;   "Create a new frame and set its project to NAME.
;; In interactive use, the user is asked for NAME."
;;   (interactive
;;    (list
;;     (workmin--get-active-project
;;      "Switch to project in new frame: " nil
;;      "There currently is no project to switch to. Create one using `workmin-create-project'.")))
;;   (make-frame (list (cons 'workmin-project name)))
;;   (when workmin-switch-project-message
;;     (message "Switched to project '%s'." name)))


(defun workmin-add-buffer-to-project (&optional arg)
  "Add the current buffer to the current project.
When ARG is non-nil or the current frame is not associated with any project,
ask the user to choose a project.
A new project can be created that way.
If the buffer is already associated with the project, display an error.
The command also prints a message when the variable `workmin-add-buffer-message'
is non-nil."
  (interactive "P")
  (let ((name (or
               (and (null arg) (workmin--current-project))
               (workmin--get-active-project
                "Add buffer to project: "
                (workmin--compose 'not (apply-partially 'workmin--is-assoc-with (current-buffer)))
                "There is no active project or all projects are already associated with the current buffer."))))
    (workmin--add-buffer-to-project (current-buffer) name workmin-add-buffer-message)))

(defun workmin--auto-add-buffer-to-project ()
  "Silently add the current buffer to the current project.
Does nothing if they are already associated with each other or
if there is no current project."
  (let ((buffer (current-buffer))
        (project (workmin--current-project)))
    (unless (workmin--is-assoc-with buffer project)
      (workmin--add-buffer-to-project buffer project))))

(defun workmin--add-buffer-to-project (buffer name &optional print)
  "Add the buffer BUFFER to the project named NAME.
If NAME is nil, do nothing.
If no such project exists, create one.
If the buffer is already associated with the project, display an error.
If PRINT is non-nil, also display a message."
  (when name
    (let* ((project (assoc name workmin-projects))
           (projects (cdr project)))
      (cond
       ((and project (member buffer projects))
        (user-error "Buffer '%s' is already associated with project '%s'."
                    (buffer-name buffer) name))
       (project
        (setcdr project (cons buffer projects)))
       (t
        (setq workmin-projects (cons (list name buffer) workmin-projects))))
      (when print
        (message "Added buffer '%s' to project '%s'." (buffer-name buffer) name)))))


(defun workmin-remove-buffer-from-project (&optional arg)
  "Remove the current buffer from the current project.
If the buffer was not associated with the current project, throw a user-error.
When ARG is non-nil or the current frame is not associated with any project,
ask the user to choose a project.
The command also prints a message when the variable `workmin-remove-buffer-message'
is non-nil.
The variable `workmin-empty-project-delete' determines what happens if the last
buffer of a project is removed."
  (interactive "P")
  (let ((name (or
               (and (null arg) (workmin--current-project))
               (workmin--get-active-project
                "Remove buffer from project: "
                (apply-partially 'workmin--is-assoc-with (current-buffer))
                "The buffer is currently not associated with any project."))))
    (workmin--remove-buffer-from-project (current-buffer) name workmin-remove-buffer-message)))

(defun workmin-remove-buffer-from-project-and-kill (&optional arg)
  "Remove the current buffer from the current project and kill the buffer.
See the documentation of `workmin-remove-buffer-from-project'."
  (interactive "P")
  (workmin-remove-buffer-from-project arg)
  (kill-buffer (current-buffer)))

(defun workmin--remove-buffer-from-project (buffer name &optional print)
  "Remove the buffer BUFFER from the project named NAME.
If no such project exists or the buffer is not associated with that project,
throw an error.
If PRINT is non-nil, also display a message.
The variable `workmin-empty-project-delete' determines what happens if the last
buffer of a project is removed."
  (let* ((project (assoc name workmin-projects))
         (projects (cdr project)))
    (unless project
      (error "There is no project named '%s'." name))
    (unless (member buffer projects)
      (user-error "Buffer '%s' is not associated with project '%s'."
                  (buffer-name buffer) name))
    (setcdr project (delete buffer projects))
    (when print
      (message "Removed buffer '%s' from project '%s'." (buffer-name buffer) name))
    (when
        (and (eq nil (cdr project))
             (or (eq workmin-empty-project-delete t)
                 (and (eq workmin-empty-project-delete 'ask)
                      (y-or-n-p (format "Project '%s' is not associated with any buffers anymore. Delete it? " name)))))
      (let ((workmin-delete-project-confirmation nil))
        (workmin-delete-project name)))))


(defun workmin-remove-buffer-from-all-projects ()
  "Remove the current buffer from all active projects.
For example, this is called when a buffer is killed."
  (interactive)
  (dolist (name (workmin--active-projects))
    (when (workmin--is-assoc-with (current-buffer) name)
      (workmin--remove-buffer-from-project (current-buffer) name))))

;; TODO
;; (defun workmin-clear-buffers (name &optional kill)
;;   "Remove all buffers, which are associated with the project named NAME.
;; If KILL is non-nil (in the interactive case, if a prefix argument is supplied),
;; kill them. After calling this function, the project has no associated buffers.
;; If there is no project named NAME, do nothing."
;;   (interactive
;;    (list
;;     (workmin--get-active-project
;;      "Select project to clear: " nil
;;      ;; could filter here for only projects that have a buffer, but not sure if needed (the string in the next line would need to be altered)
;;      "There currently is no project for which to clear buffers.")
;;     current-prefix-arg))
;;   (when kill
;;     (workmin--kill-assoc-buffers name))
;;   (setf (alist-get name workmin-projects nil nil 'equal) nil))


;; should this also allow merging? TODO
;; (defun workmin-rename-project (old-name new-name)
;;   "Rename the project named OLD-NAME to NEW-NAME.
;; In interactive use, the user is asked for both project names."
;;   (interactive
;;    (list
;;     (workmin--get-active-project
;;      "Select project to rename: " nil
;;      "There currently is no project to rename.")
;;     (workmin--get-new-project-name "New name: "))))

;; TODO: maybe deal with case if the corresponding project does not exist or NAME is nil
;; use this to save projectless "project"?
(defun workmin-save-project (name)
  "Save the project named NAME.
In interactive use, if a prefix argument is supplied or the current
frame is not associated with any project, ask the user for NAME,
presenting the currently active projects as candidates.
Otherwise, use the current project.
Intended for interactive use."
  (interactive
   (list
    (if (or current-prefix-arg
            (not (workmin--current-project)))
        (workmin--get-active-project "Save project: " nil
                                     "No project to save.")
      (workmin--current-project))))
  (workmin--desktop-save name))

(defun workmin-save-all-projects (arg)
  "Save all currently active projects.
If ARG is non-nil (in interactive use, if a prefix argument is supplied),
the value of `workmin-save-on-exit' is respected and only the projects
matching that variable's value are saved.
As a special case, if ARG is nil, this is interpreted to mean that
Emacs will exit shortly and therefore the corresponding locks of the
desktop files are removed.
This special case is completly irrelevant for the interactive use of this
function.
To save the buffers and frames not belonging to any project, run
`workmin-save-non-project'."
  (interactive "P")
  (let* ((all-projects (workmin--active-projects))
         (projects-to-save
          (if (or (not arg) (eq workmin-save-on-exit t))
              all-projects
            (let ((projects
                   (if (functionp workmin-save-on-exit)
                       (funcall workmin-save-on-exit all-projects)
                     workmin-save-on-exit)))
              (if (and projects (not (car projects)))
                  (cl-set-difference all-projects (cdr projects))
                projects)))))
    (dolist (name projects-to-save)
      (workmin--desktop-save name)
      ;; Release the locks.
      (and (eq arg t)
           (workmin--desktop-release-lock name)))))


;; (defun workmin-save-all ()
;;   "Save the currently active projects to be restored next time.
;; If `workmin-save-non-project' is non-nil, also save the buffers
;; which are not associated with any project.
;; Called by `workmin-mode' before exiting Emacs (using `kill-emacs-hook')."
;;   (interactive)
;;   (dolist (name (workmin--active-projects))
;;     (unless (member name workmin-save-on-exit)
;;       (workmin--desktop-save name)))
;;   (when workmin-save-non-project
;;     (workmin-save-non-project)))


;; Maybe integrate into open project or a new, more general function.
(defun workmin-load-non-project ()
  "Load the buffers, which were not associated with any project.
These are saved in a desktop file directly in the directory specified by
`workmin-directory'.
If no such desktop file exists, do nothing."
  (interactive)
  ;; I think it displays message when no desktop file exists.
  ;; TODO: Not sure if 'when' is needed.
  ;; (when (file-exists-p (desktop-full-file-name workmin-directory))
  (workmin--desktop-read nil))

(defun workmin-save-non-project ()
  "Save the buffers and frames which were not associated with any project.
These are saved in a desktop file directly in the directory specified by
`workmin-directory'."
  (interactive)
  (workmin--desktop-save nil))



(defun workmin--current-project (&optional frame)
  "Return the project currently associated with the frame FRAME.
If FRAME is nil, use the current frame."
  (frame-parameter frame 'workmin-project))

(defun workmin--set-current-project (name &optional frame)
  "Set the project of the frame FRAME to the project named NAME."
  (set-frame-parameter frame 'workmin-project name))

(defun workmin--get-buffers (name)
  "Return the list of buffers associated with the project named NAME."
  (alist-get name workmin-projects nil nil 'equal))

(defun workmin--is-assoc-with (obj name)
  "Return t if the object OBJ is associated with the project named NAME and nil otherwise.
OBJ may be a buffer or a frame."
  (cond ((bufferp obj) (member obj (workmin--get-buffers name)))
        ((framep obj) (equal (workmin--current-project obj) name))
        (t (error "workmin--is-assoc-with: object is not a buffer or a frame."))))

(defun workmin--get-project-directory (name)
  "Return the directory belonging to a (possibly non-existent) project with name NAME."
  (concat workmin-directory name))

(defun workmin--get-project-frames (name)
  "Return the currently open frames which belong to the project named NAME."
  (cl-remove-if-not (lambda (frame) (workmin--is-assoc-with frame name)) (frames-on-display-list)))

;; TODO, overworked, now too complicated
;; (defun workmin--kill-assoc-buffers (name &optional predicate message)
;;   "Kill the buffers associated with the project named NAME.
;; If PREDICATE is non-nil, it should be a function, which takes a single
;; argument - a buffer - and returns a non-nil value if the buffer should
;; be deleted and nil otherwise.
;; The user is asked for confirmation before the deletion happens,
;; unless the variable `workmin-kill-buffers-confirmation'is nil.
;; The string used to get confirmation from the user is MESSAGE,
;; if it is non-nil, and \"Kill all associated buffers? \" otherwise."
;;   ;; TODO: maybe a message? (but might not be visible for long caus the caller functions
;;   ;; of this function probably write something directly after the call)
;;   (when (or (not workmin-kill-buffers-confirmation)
;;             (y-or-n-p (or message "Kill all associated buffers? ")))
;;     (let ((buffers (if predicate
;;                            (cl-remove-if-not predicate (workmin--get-buffers name))
;;                          (workmin--get-buffers name))))
;;       (dolist (buffer buffers)
;;         (kill-buffer buffer)))))


;; TODOO still needed?
(defun workmin--kill-assoc-buffers (name)
  "Kill all buffers associated with the project named NAME.
Asks for confirmation from the user unless `workmin-kill-buffers-confirmation'
is nil."
  ;; TODO: maybe a message? (but might not be visible for long caus the caller functions
  ;; of this function probably write something directly after the call)
  (when (or (not workmin-kill-buffers-confirmation)
            (y-or-n-p "Kill all associated buffers? "))
    (dolist (buffer (workmin--get-buffers name))
      (kill-buffer buffer))))

(defun workmin--get-buffers-not-assoc ()
  "Return a list of all buffers not associated with any project."
  (let ((buffers (buffer-list)))
    (dolist (project workmin-projects)
      (dolist (buffer (cdr project))
        (setq buffers (delete buffer buffers))))
    buffers))

;; TODO: add a function to close a project if its last corresponding frame is closed?
;; problem: what to do with the buffers? ask the user?
;; (defun workmin--frame-closed (frame)
;;   "If the last frame belonging to a project is closed, close the project.")


(defun workmin-mode-line-indicator ()
  "Return a string used for the mode line indicator of workmin.
This function is only used when `workmin-mode-line-format' is t."
  (and (workmin--current-project)
       (propertize (workmin--current-project)
                   'face 'workmin-mode-line-face)))



;;; `desktop' integration
;; (setq desktop-restore-reuses-frames 'keep)


(defun workmin--desktop-save (name)
  "Save the buffers of the project named NAME to their corresponding desktop file.
NAME may be nil, and in that case, the desktop file representing the buffers
which were not associated with any project are loaded.
Very similar to the `desktop-save' function."
  (setq desktop-dirname
        (file-name-as-directory
         (expand-file-name
          (workmin--get-project-directory name))))
  (unless (file-exists-p desktop-dirname) (make-directory desktop-dirname))
  (save-excursion
    (let ((eager desktop-restore-eager)
	  (new-modtime (file-attribute-modification-time
			(file-attributes (desktop-full-file-name))))
          ;; Just save the buffers, which were not killed. Should not be needed,
          ;; because buffers are removed as soon as they are killed due to a function added to `kill-buffer-hook'.
          (buffer-list (if name
                           (cl-remove-if-not 'buffer-live-p (workmin--get-buffers name))
                         (workmin--get-buffers-not-assoc))))
      ;; (when
      ;;     (or (not new-modtime)		; nothing to overwrite
      ;;         (equal desktop-file-modtime new-modtime)
      ;;         (yes-or-no-p (if desktop-file-modtime
      ;;   		       (if (time-less-p desktop-file-modtime
      ;;   					new-modtime)
      ;;   			   "Desktop file is more recent than the one loaded.  Save anyway? "
      ;;   			 "Desktop file isn't the one loaded.  Overwrite it? ")
      ;;   		     "Current desktop was not loaded from a file.  Overwrite this desktop file? ")))

	;; If we're done with it, release the lock.
	;; Otherwise, claim it if it's unclaimed or if we created it.
      (unless (and new-modtime (desktop-owner)) (desktop-claim-lock))
      ;; )

        ;; What format are we going to write the file in?
        (setq desktop-io-file-version
              (cond
               ((null desktop-io-file-version) ; As yet, no desktop file exists.
                desktop-native-file-version)
               (t
                desktop-io-file-version)))

	(with-temp-buffer
          (let ((temp-buffer (current-buffer)))
	    (insert
	     ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
	     desktop-header
	     ";; Created " (current-time-string) "\n"
	     ";; Desktop file format version " (format "%d" desktop-io-file-version) "\n"
	     ";; Emacs version " emacs-version "\n")
	    (save-excursion (run-hooks 'desktop-save-hook))
	    (goto-char (point-max))
	    (insert "\n;; Global section:\n")
	    ;; Called here because we save the window/frame state as a global
	    ;; variable for compatibility with previous Emacsen.
	    (workmin--desktop-save-frameset name)
	    (unless (memq 'desktop-saved-frameset desktop-globals-to-save)
	      (desktop-outvar 'desktop-saved-frameset))
	    (mapc #'desktop-outvar desktop-globals-to-save)
	    (setq desktop-saved-frameset nil) ; after saving desktop-globals-to-save
	    (when (memq 'kill-ring desktop-globals-to-save)
	      (insert
	       "(setq kill-ring-yank-pointer (nthcdr "
	       (int-to-string (- (length kill-ring) (length kill-ring-yank-pointer)))
	       " kill-ring))\n"))
	    (insert "\n;; Buffer section -- buffers listed in same order as in buffer list:\n")
            ;; The following line for some reason switches to the buffers.
	    (dolist (l (mapcar #'desktop-buffer-info buffer-list))
              (set-buffer temp-buffer)
	      (let ((base (pop l)))
	        (when (apply #'desktop-save-buffer-p l)
		  (insert "("
			  (if (or (not (integerp eager))
				  (if (zerop eager)
				      nil
				    (setq eager (1- eager))))
			      "desktop-create-buffer"
			    "desktop-append-buffer-args")
			  " "
			  (format "%d" desktop-io-file-version))
		  ;; If there's a non-empty base name, we save it instead of the buffer name
		  (when (and base (not (string= base "")))
		    (setcar (nthcdr 1 l) base))
		  (dolist (e l)
		    (insert "\n  " (desktop-value-to-string e)))
		  (insert ")\n\n"))))
	    (setq default-directory desktop-dirname)
	    ;; When auto-saving, avoid writing if nothing has changed since the last write.
	    (let ((coding-system-for-write 'utf-8-emacs))
	      (write-region (point-min) (point-max) (desktop-full-file-name) nil 'nomessage))
	    (setq desktop-file-checksum nil)
	    ;; We remember when it was modified (which is presumably just now).
	    (setq desktop-file-modtime (file-attribute-modification-time
				        (file-attributes
				         (desktop-full-file-name)))))))))


;; TODO: experiment with reuse-frames to reuse some frames.
;; allow noprint option?
(defun workmin--desktop-read (name &optional reuse-frames cleanup-frames)
  "Load the buffers of the project named NAME from the corresponding desktop file.
The stored data is added to `workmin-projects'.
Note that NAME may be nil, in that case, the desktop file representing the
buffers which were not associated with any project, are loaded.
For REUSE-FRAMES and CLEANUP-FRAMES, consult the documentation of `frameset-restore'.
Similar to the `desktop-read' function."
  (unless noninteractive
    (setq desktop-dirname
          (file-name-as-directory
           (expand-file-name
            (workmin--get-project-directory name))))
    (if (file-exists-p (desktop-full-file-name))
	;; Desktop file found, but is it already in use?
	(let ((desktop-first-buffer nil)
	      (desktop-buffer-ok-count 0)
	      (desktop-buffer-fail-count 0)
	      (owner (desktop-owner))
	      ;; Avoid desktop saving during evaluation of desktop buffer.
	      (desktop-save nil)
	      (desktop-autosave-was-enabled))
;; 	  (if (and owner
;; 		   (or (null desktop-load-locked-desktop)
;; 		       (daemonp)
;; 		       (not (y-or-n-p (format "Warning: desktop file appears to be in use by PID %s.\n\
;; Using it may cause conflicts.  Use it anyway? " owner)))))
;; 	      (let ((default-directory desktop-dirname))
;; 		(setq desktop-dirname nil)
;; 		(run-hooks 'desktop-not-loaded-hook)
;; 		(unless desktop-dirname
;; 		  (message "Desktop file in use; not loaded.")))
	    (desktop-lazy-abort)
	    ;; Temporarily disable the autosave that will leave it
	    ;; disabled when loading the desktop fails with errors,
	    ;; thus not overwriting the desktop with broken contents.
	    (setq desktop-autosave-was-enabled
		  (memq #'desktop-auto-save-set-timer
                        ;; Use the global value of the hook, in case some
                        ;; feature makes window-configuration-change-hook
                        ;; buffer-local, and puts there stuff which
                        ;; doesn't include our timer.
                        (default-value
                          'window-configuration-change-hook)))
	    (desktop-auto-save-disable)
            (setq workmin--desktop-current-project name)
	    ;; Evaluate desktop buffer and remember when it was modified.
	    (setq desktop-file-modtime (file-attribute-modification-time
					(file-attributes
					 (desktop-full-file-name))))
	    (load (desktop-full-file-name) t t t)
            (setq workmin--desktop-current-project nil)

            ;; If no buffers were restored, we need to manually add the new project to `workmin-projects'.
            (unless (or (not name) (member name (workmin--active-projects)))
              (setq workmin-projects (cons (list name) workmin-projects)))
            
	    ;; If it wasn't already, mark it as in-use, to bother other
	    ;; desktop instances.
	    (unless (eq (emacs-pid) owner)
	      (condition-case nil
		  (desktop-claim-lock)
		(file-error (message "Couldn't record use of desktop file")
			    (sit-for 1))))

	    (unless (desktop-restoring-frameset-p)
	      ;; `desktop-create-buffer' puts buffers at end of the buffer list.
	      ;; We want buffers existing prior to evaluating the desktop (and
	      ;; not reused) to be placed at the end of the buffer list, so we
	      ;; move them here.
	      (mapc #'bury-buffer
		    (nreverse (cdr (memq desktop-first-buffer (nreverse (buffer-list))))))
	      (switch-to-buffer (car (buffer-list))))
	    (run-hooks 'desktop-delay-hook)
	    (setq desktop-delay-hook nil)

            ;; We don't want previously restored frames to be influenced.
            (setq workmin--desktop-restored-frames nil)
            
	    (workmin--desktop-restore-frameset reuse-frames cleanup-frames)
            ;; Assign the newly created frames to their projects.
            (dolist (frame workmin--desktop-restored-frames)
              (workmin--set-current-project name frame))
	    (run-hooks 'desktop-after-read-hook)
	    (message "Workmin:%s %s%d buffer%s restored%s%s."
                     (if name
                         (concat " project '" name "':")
                       "")
		     (if desktop-saved-frameset
			 (let ((fn (length (frameset-states desktop-saved-frameset))))
			   (format "%d frame%s, "
				   fn (if (= fn 1) "" "s")))
		       "")
		     desktop-buffer-ok-count
		     (if (= 1 desktop-buffer-ok-count) "" "s")
		     (if (< 0 desktop-buffer-fail-count)
			 (format ", %d failed to restore" desktop-buffer-fail-count)
		       "")
		     (if desktop-buffer-args-list
			 (format ", %d to restore lazily"
				 (length desktop-buffer-args-list))
		       ""))
	    (unless (desktop-restoring-frameset-p)
	      ;; Bury the *Messages* buffer to not reshow it when burying
	      ;; the buffer we switched to above.
	      (when (buffer-live-p (get-buffer "*Messages*"))
		(bury-buffer "*Messages*"))
	      ;; Clear all windows' previous and next buffers, these have
	      ;; been corrupted by the `switch-to-buffer' calls in
	      ;; `desktop-restore-file-buffer' (bug#11556).  This is a
	      ;; brute force fix and should be replaced by a more subtle
	      ;; strategy eventually.
	      (walk-window-tree (lambda (window)
				  (set-window-prev-buffers window nil)
				  (set-window-next-buffers window nil))))
 	    (setq desktop-saved-frameset nil)
	    (if desktop-autosave-was-enabled (desktop-auto-save-enable))
	    t) ;; )
      ;; No desktop file found.
      (let ((default-directory desktop-dirname))
        (run-hooks 'desktop-no-desktop-file-hook))
      (message "Workmin: No desktop file.")
      nil)))

(defun workmin--desktop-restore-frameset (&optional reuse-frames cleanup-frames)
  "Restore the state of a set of frames and assign them to their project.
This function depends on the value of `desktop-saved-frameset'
being set (usually, by reading it from the desktop).
Similar to `desktop-restore-frameset'.
See the documentation of `frameset-restore'."
  (when (desktop-restoring-frameset-p)
    (frameset-restore desktop-saved-frameset
		      :reuse-frames reuse-frames
		      :cleanup-frames (lambda (frame action)
                                        (when (memq action '(:reused :created))
                                          (push frame workmin--desktop-restored-frames))
                                        (when (functionp cleanup-frames)
                                          (funcall cleanup-frames frame action)))
		      :force-display desktop-restore-in-current-display
		      :force-onscreen desktop-restore-forces-onscreen)))


(defun workmin--desktop-save-frameset (name)
  "Save the state of existing frames in `desktop-saved-frameset'.
Only those frames are saved, which belong to the project named NAME.
If NAME is nil, only those frames belonging to no project are saved.
Similar to `desktop-save-frameset'."
  (setq desktop-saved-frameset
	(and desktop-restore-frames
	     (frameset-save nil
			    :app desktop--app-id
			    :name (concat user-login-name "@" (system-name))
			    :predicate (lambda (frame)
                                         (equal (workmin--current-project frame) name))))))

(defun workmin--desktop-release-lock (name)
  "Release the lock of the desktop file belonging to NAME.
NAME is a project name. The lock is only released if the current Emacs instance
had aquired it before. "
  ;; From the definition of `desktop-kill'.
  (when (eq (emacs-pid) (desktop-owner (workmin--get-project-directory name)))
    (desktop-release-lock (workmin--get-project-directory name))))

;; To change the behavior of `desktop-create-buffer', we use advice.
;; This makes it that a restored buffer will know which project they belong to.
;; TODO: This should probably go into the mode.
(advice-add 'desktop-create-buffer :filter-return 
              (lambda (buffer)
                (when (and buffer workmin--desktop-current-project)
                  ;; buffer restored correctly and there is a current project
                  (workmin--add-buffer-to-project buffer workmin--desktop-current-project))
                buffer))
  
  
(defun workmin--init ()
  "Load projects from the last session and set up hooks. 
The projects are stored in desktop files.
The variables `workmin-load-on-startup' and `workmin-load-non-project'
determine which projects are loaded automatically."
  (let* ((current-frame (selected-frame))
         (all-projects (workmin--saved-projects))
         (projects-to-load
          (if (eq workmin-load-on-startup t)
              all-projects
            (let ((projects
                   (if (functionp workmin-load-on-startup)
                       (funcall workmin-load-on-startup all-projects)
                     workmin-load-on-startup)))
              (if (and projects (not (car projects)))
                  (cl-set-difference all-projects (cdr projects))
                projects)))))
    (dolist (name projects-to-load)
      (workmin--desktop-read name))
    (when workmin-load-non-project
      (workmin-load-non-project))
    ;; There always is the "starting frame", which gets created upon launch.
    ;; We remove it, unless it is the only frame, of course.
    (unless (eq (length (frame-list)) 1)
      (delete-frame current-frame))
    ;; Add the hooks specified in `workmin-auto-add-in-hooks'.
    (dolist (hook workmin-auto-add-in-hooks)
      (add-hook hook 'workmin--auto-add-buffer-to-project))))

(defun workmin--exit ()
  "Save projects to be restored next time and prepare to exit Emacs.
The variables `workmin-save-on-exit' and `workmin-save-non-project'
determine which projects are saved. 
Called by `workmin-mode' before exiting Emacs (using `kill-emacs-hook')."
  (workmin-save-all-projects t)
  (when workmin-save-non-project
    (workmin-save-non-project)))


(defun workmin--active-projects ()
  "Return a list containing the names of all active projects."
  (mapcar 'car workmin-projects))


(defun workmin--saved-projects ()
  "Return a list of the names of all saved projects.
The saved projects are those which have a folder in `workmin-directory'."
  (and (file-exists-p workmin-directory)
       (remove ".."
               (remove "."
                       (mapcar 'car
                               (cl-remove-if-not
                                (lambda (elem)
                                  (eq (cadr elem) t))
                                (directory-files-and-attributes workmin-directory)))))))


;; Could also define a variable which holds the non-saved (new) projects.
;; However, this will then need to be checked in desktop loading, which I don't like.

(defun workmin--all-projects ()
  "Return a list of all projects.
This includes those which have a folder in `workmin-directory'
and those which were newly created and do not yet have such a folder."
  (let* ((saved-projects (workmin--saved-projects))
         (new-projects
          (cl-remove-if
           (lambda (name) (member name saved-projects))
           (workmin--active-projects))))
    (append saved-projects new-projects)))


(defun workmin--get-active-project (prompt predicate no-candidate-error)
  "Ask the user for an active project name using `completing-read'.
An active project is a project, whose frames and buffers were loaded
and thus are available to the user.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
PREDICATE should be nil or a function, which takes a project name as an
argument. In the latter case, only projects are displayed that satisfy that
function, i.e. for for which PREDICATE returns a non-nil value.
NO-CANDIDATE_ERROR is a string used in the `user-error', which is thrown if
there is no project to select from."
  (workmin--get-project-interface
   prompt (workmin--active-projects) predicate (workmin--current-project)
   (lambda (str)
     (concat str
             (or
              (and (equal (workmin--current-project) str)
                   " [current frame]")
              (and (cl-some
                    (lambda (frame)
                      (equal (workmin--current-project frame) str))
                    (frames-on-display-list))
                   " [other frame]"))))
   no-candidate-error
   (lambda () (user-error "The name of a project can not be empty."))))


(defun workmin--get-saved-project (prompt predicate no-candidate-error)
  "Ask the user for a saved project's name using `completing-read'.
The saved projects are those which have a folder in `workmin-directory'.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
PREDICATE should be nil or a function, which takes a project name as an
argument. In the latter case, only projects are displayed that satisfy that
function, i.e. for for which PREDICATE returns a non-nil value.
NO-CANDIDATE_ERROR is a string used in the `user-error', which is thrown if
there is no project to select from."
  (workmin--get-project-interface
   prompt (workmin--saved-projects) predicate (workmin--current-project)
   (lambda (str)
     (concat str
             (or
              (and (equal (workmin--current-project) str)
                   " [current frame]")
              (and (cl-some
                    (lambda (frame)
                      (equal (workmin--current-project frame) str))
                    (frames-on-display-list))
                   " [other frame]")
              (and (eq (desktop-owner (workmin--get-project-directory str))
                       (emacs-pid))
                   " [loaded]")
              (and (desktop-owner (workmin--get-project-directory str))
                   " [loaded in other instance]"))))
   no-candidate-error
   (lambda () (user-error "The name of a project can not be empty."))))


(defun workmin--get-project (prompt predicate no-candidate-error)
  "Ask the user for a project's name using `completing-read'.
This includes active and inactive projects.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
PREDICATE should be nil or a function, which takes a project name as an
argument. In the latter case, only projects are displayed that satisfy that
function, i.e. for for which PREDICATE returns a non-nil value.
NO-CANDIDATE_ERROR is a string used in the `user-error', which is thrown if
there is no project to select from."
  (workmin--get-project-interface
   prompt (workmin--all-projects) predicate (workmin--current-project)
   (lambda (str)
     (concat str
             (or
              (and (equal (workmin--current-project) str)
                   " [current frame]")
              (and (cl-some
                    (lambda (frame)
                      (equal (workmin--current-project frame) str))
                    (frames-on-display-list))
                   " [other frame]")
              (and (eq (desktop-owner (workmin--get-project-directory str))
                       (emacs-pid))
                   " [loaded]")
              (and (desktop-owner (workmin--get-project-directory str))
                   " [loaded in other instance]"))))
   no-candidate-error
   (lambda () (user-error "The name of a project can not be empty."))))


(defun workmin--get-project-interface
    (prompt projects predicate default pretty-printer no-candidate-error empty-string-function)
  "Interface for asking the user for a project's name using `completing-read'.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
PROJECTS is a list of project names, which will be valid candidates for the user
to choose from.
PREDICATE should be nil or a function, which takes a project name as an
argument. In the latter case, only projects are displayed that satisfy that
function, i.e. for for which PREDICATE returns a non-nil value.
DEFAULT is the default value or the list of default values.
PRETTY-PRINTER should be nil or a function that takes a string (a project name)
and returns a string. That return value is displayed for the user to select
instead of the project's name.
NO-CANDIDATE-ERROR is a string, which will be used in the error that is thrown if the
list of projects is empty.
EMPTY-STRING-FUNCTION is a function that takes no arguments and is called when
the user enters the empty string.
Note that this only takes effect when DEFAULT is nil, because otherwise the empty
string is interpreted to refer to the default value.
In that case, its return value is the return value of this function.
However, EMPTY-STRING-FUNCTION could, for example, throw an error."
  (let* ((projects
          (or (if (functionp predicate)
                  (cl-remove-if-not predicate projects)
                projects)
              (error no-candidate-error)))
         (pretty-printer (or pretty-printer #'identity))
         (collection
            (mapcar pretty-printer projects))
         (default-projects
           (if (listp default)
               (mapcar pretty-printer default)
             (funcall pretty-printer default)))
         (selection
          (completing-read
           prompt collection nil 0 nil workmin-project-name-hist default-projects))
         (index (cl-position selection collection :test 'equal))
         (name (if index (nth index projects)
                 selection)))
    (if (string-empty-p name)
        (funcall empty-string-function)
      name)))

(defun workmin--get-new-project-name (prompt)
  "Ask the user for a project name using `read-string' and return it.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
The string entered must be a valid project name, i.e. it must pass
`workmin--valid-project-name' and be non-empty.
Moreover, there may not already exist a project of the same name.
If one of those conditions is violated, an error is thrown."
  (let ((name (read-string prompt)))
    (workmin--nonempty-project-name name)
    (workmin--valid-project-name name)
    (when (member name (workmin--all-projects))
      (user-error "There already is a project named '%s'." name))
    name))

(defun workmin--valid-project-name (name)
  "Check if the string NAME is a potential project name.
This means that it only contains letters, digits or characters,
which are specified in the variable `workmin-valid-naming-chars'.
If a name does not match, throw a `user-error'.
This function does not check that NAME is not empty, but the function
`workmin--nonempty-project-name' does just that."
  (mapcar
   (lambda (char)
     (or (and (>= char 48) (<= char 57)) ;; digit
         (and (>= char 65) (<= char 90)) ;; uppercase letter
         (and (>= char 97) (<= char 122)) ;; lowercase letter
         (member char workmin-valid-naming-chars)
         (user-error "The character '%c' is not valid for naming a project. See the variable `workmin-valid-naming-chars'."
                   char)))
   name))

(defun workmin--nonempty-project-name (name)
  "Check if the string NAME is non-empty and thus a potential project name.
Otherwise, throw an error."
  (when (string-empty-p name)
    (user-error "The name of a project can not be empty.")))



;; TODOS:
;; - function that allows the user to manually load projects (from the desktop files)
;; - corner case: project was not loaded and a project of the same name was created
;; - integrate ibuffer (filter groups!) add hotkey to assign a project to the buffer
;; - add hooks
;; - maybe allow sorting?
;; - allow splitting off into new emacs processes (in particular need to be able to communicate with it, maybe use the existance of the desktop lock files?)
;; - see todos in desk.el
;; - recentf, bookmark support
;; - allow transfering of buffers/frames?
;; - can a frame be "copied"?
;; - check project.el
;; - maybe: add categories, each category consists of a list of projects.
;; - potentially check noninteractive variable (see `desktop.el')
;; - check if desktop-release-lock and desktop-claim-lock are used properly
;; - it could potentially be wise to have a special desktop file, which is loaded at the end and is used to set up stuff

(defun workmin--compose (f g)
  "Compose the two functions F and G.
It returns the function that first applies g and then f."
  `(lambda (x) (,f (,g x))))

(eval-after-load 'ivy
  (load "~/.emacs.d/local-pkg/workmin/workmin-ivy.el")) ;; how do other packages do this? (looked at ivy, not obvious there)

(provide 'workmin)
;;; workmin.el ends here
