;; Lexical bindings lead to errors in the desktop functions.
;; One could refractor them into their own file.


;; ;;; perject.el --- manage your projects  -*- lexical-binding: t; -*-
;; This package allows the user to manage multiple projects in a single Emacs instance.
;;
;; Each project consists of a list of buffers and a list of frames,
;; which have corresponding window configurations.
;; Every project but the 'nil' one (corresponding to the non-associated buffers) is assumed to have at least one frame.

;; List frames: frame-list or frames-on-display-list
;; (add-to-list 'window-persistent-parameters (cons 'workspace 'writable))

;; check which buffers are in first and second project:
;; (cl-remove-if-not (lambda (buffer) (member buffer (cdadr perject-projects))) (cdar perject-projects))


;; TODOS: Search for 'TODOO' for most important feature.
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
;; - modifying the buffer list from perject--get-buffers: does this change the metadata?
;; - Look through perspective.el and frame-bufs https://github.com/alpaker/Frame-Bufs
;; - remember previously open projects
;; - potentially test how this works in EXWM
;; - support "archived" projects (i.e. in new directory).
;; - give each screen the (optional) feature to manage its stack of recent (not recentf) buffers
;; - i have the following hotkey idea (mostly for exwm):
;;   for each project one can set hotkey to switch to
;; maybe have a general save hook instead of lots of other hooks
;; - move integration for perview from config to extra file
;; - auto save?

;; auto-add-buffers variable (project . buffer-name) where projects may be a project, a list of projects, t, or a function

;; allow restoring of previously opened projects
(require 'desktop)
(require 'cl-lib) ;; for remove-if-not and some other stuff
(require 'dash)
(require 'dash-functional)



(defconst perject-command-line-option-no-load "--perject-no-load"
  "Command line option, which will prevent Emacs from opening any perject projects.
This is the same as starting Emacs with `perject-load-on-startup' and
`perject-load-non-project' set to nil.
It influences the `perject--init' function.")



(defvar perject-projects nil
  "A list representing the projects.
Each element is a list, where the car (first element) is a string,
which is the name of the project and the cdr is the list of buffers
belonging to that project.
Every project also has a list of associated frames.")


;; (defvar perject-default-project nil
;;   "The name of the project, which the starting frame will be dedicated to.
;; If nil, the starting frame is dedicated to no project.")
;; use? initial-frame-alist


(defface perject-mode-line-face
  '((t :foreground "orange")) ;; gold also nice; do nothing: '((t (nil)))
  "The face used by the mode line indicator of perject."
  :group 'perject)

;; TODOO (not implemented, should be implemented also for created projects...)
;; (defvar perject-auto-add-buffers '(t . "*scratch*")
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
;; TODO: check if clone-indirect-buffer-hook works as intended.
(defvar perject-auto-add-in-hooks '(find-file-hook clone-indirect-buffer-hook dired-mode-hook help-mode-hook org-src-mode-hook)
  "A list of hooks, in which the current buffer is added to the current project.
This is used to automatically add newly created buffers to the current project.
The following hooks could be interesting to the user:
`change-major-mode-hook', `find-file-hook', `clone-indirect-buffer-hook',
`buffer-list-update-hook' and many mode hooks.
Modifying this variable only takes effect after (re)enabling `perject-mode'.
Internally, the function `perject--auto-add-buffer-to-project' is used.")

(defvar perject-close-project-save 'ask
  "This variable controls if a project is saved when being closed using `perject-close-project'.
It can have one of three values:
- t: The project is saved.
- 'ask: The user is asked if the project should be saved.
- nil: The project is not saved.")

(defvar perject-switch-project-message t
  "If non-nil, the command `perject-switch-project' will print a message upon completion.")

(defvar perject-add-buffer-message t
  "If non-nil, a message will be printed when a buffer is successfully added to a project.
This influences the commands `perject-add-buffer-to-project' and
`perject-ibuffer-add-to-project'.")

(defvar perject-remove-buffer-message t
  "If non-nil, a message will be printed when a buffer is successfully removed from a project.
This influences the commands `perject-remove-buffer-from-project',
`perject-remove-buffer-from-project-and-kill' and
`perject-ibuffer-remove-from-project'.")

(defvar perject-close-project-confirmation nil
  "If non-nil, the user is asked for confirmation before closing a project
with `perject-close-project'.
Otherwise, the user is not asked for confirmation.")

(defvar perject-delete-project-confirmation t
  "If non-nil, the user is asked for confirmation before deleting a project
with `perject-delete-project'.
Otherwise, the user is not asked for confirmation.")

(defvar perject-kill-buffers-confirmation t
  "If non-nil, the user is asked for confirmation before killing all the buffers
associated with a project.
Otherwise, the user is not asked for confirmation.
This affects `perject-clear-buffers', `perject-close-project' and
`perject-delete-project'.")

(defvar perject-empty-project-delete 'ask
  "This variable controls what happens when the last buffer is removed from a project.
It can have one of three values:
- t: The project is deleted.
- 'ask: The user is asked whether to delete the project or keep it.
- nil: The project is kept.")

(defvar perject-load-on-startup nil
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
variable `perject-load-non-project'.")

(defvar perject-save-on-exit t
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
variable `perject-save-non-project'.")

(defvar perject-load-non-project t
  "When non-nil, the non-project buffers are loaded at startup.
Otherwise, they are not loaded at startup, but may be manually loaded later by invoking
`perject-load-non-project'.
The non-project buffers are those which were not associated with any project.")

(defvar perject-load-non-project-new-instance t
  "When non-nil, the non-project buffers are loaded when opening a project in a new Emacs instance.
Otherwise, they are not loaded at startup, but may be manually loaded later by invoking
`perject-load-non-project'.
The non-project buffers are those which were not associated with any project.
This influences the command `perject-open-project-in-new-instance'.")

(defvar perject-save-non-project t
  "When non-nil, the buffers, which were associate with no project, are saved when exiting Emacs.
Otherwise, they are not saved when exiting Emacs, but may be manually saved by invoking
`perject-save-non-project'.")


(defvar perject-mode-line-format t
  "This variable determines the format of the mode line indicator of perject.
Its value may be of the following type:
- nil: No mode line entry is shown for perject.
- A function: Call that function with a single string as an argument,
  which is the name of the current project.
  The function should return a string, which is then displayed in the mode line.
  Note that this function will get nil as an argument if the current frame is
  not associated with any project.
- t: Use the default format, which is simply the name of the project with face
`perject-mode-line-face'.")

(defvar perject-frame-title-format t
  "This variable determines the format of the title of a frame.
Its value may be of the following type:
- nil: The title of a frame is not altered by perject.
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

(defvar perject-directory "~/.emacs.d/perject/"
  "The directory used by perject to save the data.")
(unless (file-exists-p perject-directory)
  (make-directory perject-directory))

(defvar perject-valid-naming-chars '(?_ ?- ? )
  "A list of characters, that may be used when naming a project.
All letters and digits are always allowed.
Note that the name of the project has to also be a valid (possibly non-existent)
directory name, so be careful.
By default, this variable allows '_', '-' and ' '.
This variable is used in the function `perject--valid-project-name'.")

(defvar perject-after-desktop-create-buffer-hook nil
  "Hook run after a buffer has been successfully restored by desktop.
The functions should take one argument, which is the newly created buffer.
The return value is ignored.
This hook should be used with care.")

(defvar perject-after-init-hook nil
  "Hook run after perject has initialized.
This means that all the buffers and frames from the projects configured to
automatically load, have been restored.")

(defvar perject-before-open-hook nil
  "Hook run before perject has opened a project.
In particular, the buffers and frames from the project have not yet been
restored.")

;; Maybe a hook before, what about new frames?
(defvar perject-after-open-hook nil
  "Hook run after perject has opened a project.
In particular, all the buffers and frames from the project have been restored.
The variable `perject--desktop-restored-frames' is the list of newly created
frames and might be useful.")

(defvar perject-before-close-hook nil
  "Hook run before perject closes a project.
The functions should take one argument, which is the name of the project to be
closed.")

(defvar perject-after-close-hook nil
  "Hook run after perject has closed a project.
The functions should take one argument, which is the name of the closed project. ")

(defvar perject-project-name-hist nil
  "The history of project names.")

;; I am surprised such a variable does not already exist in `frameset.el'
(defvar perject--desktop-restored-frames nil
  "A list of the frames, which were restored for the most recent project.
Should not be modified by the user.")

(defvar perject--desktop-current-project nil
  "Internal variable used to assign the correct project to newly restored buffers and frames.
Should not be modified by the user.")

(defvar perject--frame-title-backup nil
  "Internal variable used to save the value of `frame-title-format'; to restore it later.
Should not be modified by the user.")



(define-minor-mode perject-mode
  "Toggle perject mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When perject mode is enabled, the user can manage projects using perject's
features."
  ;; TODO overwork doc
  :global t
  :keymap nil
  ;; :lighter " wm"
  (if perject-mode
      (progn
        (add-hook 'after-init-hook 'perject--init)
        (add-hook 'kill-emacs-hook 'perject--exit)
        (add-hook 'kill-buffer-hook 'perject-remove-buffer-from-all-projects)
;;        (add-hook 'delete-frame-functions )

        (when after-init-time
          ;; This means the mode got enabled and the init phase is already over.
          ;; I.e. later, manually by the user.
          ;; In that case, `after-init-hook' is not run.
          (dolist (hook perject-auto-add-in-hooks)
            (add-hook hook 'perject--auto-add-buffer-to-project)))
        
        ;; The mode line knows to which mode this belongs to, and if the mode is not active, the entry is not shown.
        (unless (or (not perject-mode-line-format) (assoc 'perject-mode mode-line-misc-info))
          (if (functionp perject-mode-line-format)
              (push '(perject-mode (:eval (funcall perject-mode-line-format (perject--current-project))))
                    (cdr (last mode-line-misc-info)))
            (push '(perject-mode (:eval (perject-mode-line-indicator)))
                  (cdr (last mode-line-misc-info)))))
        (when perject-frame-title-format
          (setq
           perject--frame-title-backup
           frame-title-format
           frame-title-format
           (if (functionp perject-frame-title-format)
               (funcall perject-frame-title-format (perject--current-project))
             '("" invocation-name "@" system-name
               (:eval (or (and (perject--current-project)
                               (concat ":" (perject--current-project)))))))))
    )

    ;; Remove the added hooks.
    (remove-hook 'after-init-hook 'perject--init)
    (remove-hook 'kill-emacs-hook 'perject--exit)
    (remove-hook 'kill-buffer-hook 'perject-remove-buffer-from-all-projects)
    (dolist (hook perject-auto-add-in-hooks)
            (remove-hook hook 'perject--auto-add-buffer-to-project))
    (setq frame-title-format perject--frame-title-backup
          perject--frame-title-backup nil))
  )

;; TODO: implement reloading; maybe even put it in a new function
;; (because like this, the check for "already opened" is done 2 times)
;; At the moment, if there are multiple frames belonging to the project,
;; just a "random" one is selected. Could introduce another frame parameter
;; to let the user decide which frame is primary.
(defun perject-open-project (arg)
  "Ask the user for a project, open it and switch to one of its corresponding frames.
The switching might not work, depending on the window manager.
This means that all its buffers are restored and frames are opened from the
corresponding desktop file.
When ARG is nil, the user is restricted to non-open projects.
Otherwise, the user can reopen (reload) a project.
To restore the buffers and frames belonging to no project, run
`perject-load-non-project-buffers'."
  (interactive "P")
  (let ((name (perject--get-saved-project
               "Open project: "
               (and (not arg)
                    (perject--compose 'not 'perject--is-active-project))
               "No project to open."))
        (frame (make-frame)))
    (run-hooks 'perject-before-open-hook)
    ;; Maybe try with selected-frame
    ;; (with-selected-frame frame
    ;;   (perject--desktop-read name))
    (select-frame frame)
    (perject--desktop-read name)
    (delete-frame frame)
    ;; TODO: for some reason this does not have any effect on my wm (Kde Plasma)
    (when perject--desktop-restored-frames
      (select-frame (car perject--desktop-restored-frames)))
    (run-hooks 'perject-after-open-hook)))


(defun perject-open-project-in-new-instance (arg)
  "Ask the user for a project and open a new Emacs instance for it.
This means that a new Emacs process is created and it restores the buffers and
frames from the corresponding desktop file.
When ARG is nil, the user is restricted to non-open projects.
Otherwise, the user can reopen (reload) a project.
To restore the buffers and frames belonging to no project, run
`perject-load-non-project-buffers'."
  (interactive "P")
  (let* ((name (perject--get-saved-project
                "Open project in new Emacs instance: "
                (and (not arg)
                     (perject--compose 'not 'perject--is-active-project))
                "No project to open."))
         (parameter
          (concat
           perject-command-line-option-no-load
           " --eval '(let ((current-frame (selected-frame))) (perject--desktop-read \""
           name
           "\") "
           (when perject-load-non-project-new-instance
             "(perject-load-non-project) ")
           "(unless (eq (length (frame-list)) 1) (delete-frame current-frame)))'")))
    ;; Start the new Emacs instance, see
    ;; https://emacs.stackexchange.com/questions/5428/restart-emacs-from-within-emacs.
    (call-process "sh" nil nil nil "-c" (concat "emacs " parameter " &"))))

;; Maybe make sure to close ibuffer buffer belonging to the project?
(defun perject-close-project (name &optional arg)
  "Close the project named NAME.
Remove the project from the list of active projects and close its frames.
In interactive use, the user is asked for NAME.
ARG determines what happens to the buffers belonging to the project.
If ARG is nil (in interactive use: no prefix argument), the buffers, which do
not belong to any other project but the selected one, are killed.
If ARG is '(4) (a single prefix argument), all buffers belonging to the project
are killed.
Otherwise, no buffers are killed.
Depending on the value of the variable `perject-close-project-save',
the project is saved to a desktop file or not.
If the variable `perject-close-project-confirmation' is non-nil,
the user is asked for confirmation before the project is closed.
Whether the user is asked for confirmation an additional time if
he wants to kill associated buffers, is controlled by the
variable `perject-kill-buffers-confirmation'.
An error is thrown if there is no project to close or all the open frames belong
to the selected project."
  (interactive
   (list
    (perject--get-active-project
     "Close project: " nil
     "There currently is no project to close.")
    current-prefix-arg))
  (when (or (not perject-close-project-confirmation)
            (y-or-n-p (format "Closing project '%s'. Are you sure? " name)))
    (when (eq (length (perject--get-project-frames name)) (length (frames-on-display-list)))
      (user-error "Can't close a project which belongs to all open frames."))
    (run-hook-with-args 'perject-before-close-hook name) ;; TODO: temp solution, probably better to overwork hook system. <2020-05-22 Fri>
    ;; Seems to slow down the kill buffer question??
    (when (or (eq perject-close-project-save t)
              (and (eq perject-close-project-save 'ask)
                   (y-or-n-p (format "Save the project '%s'? " name))))
      (perject--desktop-save name))
    (perject--desktop-release-lock name)
    (let ((buffers (perject--get-buffers name)))
      (setq perject-projects (assoc-delete-all name perject-projects))
      (dolist (frame (perject--get-project-frames name))
        (delete-frame frame))
      (when (and (or (not arg) (equal arg '(4)))
                 (or (not perject-kill-buffers-confirmation)
                     (y-or-n-p (format "Kill buffers belonging to project '%s'?" name))))
        (if (not arg)
            ;; Determine the buffers, which are not associated with any other project,
            ;; but the selected one.
            (dolist (buffer (cl-intersection buffers (perject--get-buffers-not-assoc)))
              (kill-buffer buffer))
          (dolist (buffer buffers)
            (kill-buffer buffer))))
      (run-hook-with-args 'perject-after-close-hook name))))


;; Maybe allow opening in the same frame? But not sure what to do with the frame parameters.
(defun perject-create-project (name)
  "Create a project named NAME and open a frame for it.
In interactive use, the user is asked for the name and the entered string must
pass `perject-valid-project-name' and there must not be an existing project of
the same name. Otherwise, an error is thrown."
  (interactive
   (list
    (perject--get-new-project-name "Create project: ")))
  (setq perject-projects (cons (list name) perject-projects))
  (perject-new-frame name))


(defun perject-delete-project (name &optional arg)
  "Delete the project named NAME.
This includes closing the project and deleting the corresponding desktop file.
In interactive use, the user is asked for NAME.
ARG determines what happens to the buffers belonging to the project.
If ARG is nil (in interactive use: no prefix argument), no buffers are killed.
If ARG is '(4) (a single prefix argument), the buffers, which do not belong to
any other project but the selected one, are killed.
Otherwise, all buffers belonging to the project are killed.
If the variable `perject-delete-project-confirmation' is non-nil,
the user is asked for confirmation before the project is deleted.
Whether the user is asked for confirmation an additional time if
he wants to kill associated buffers, is controlled by the
variable `perject-kill-buffers-confirmation'.
An error is thrown if there is no project to delete or all the open frames
belong to the selected project."
  (interactive
   (list
    (perject--get-project
     "Delete project: " nil
     "There currently is no project to delete.")
    current-prefix-arg))
  (when (or (not perject-delete-project-confirmation)
            (y-or-n-p (format "Deleting project '%s'. Are you sure? " name)))
    ;; If the project is active, close it.
    (when (perject--is-active-project name)
      (let ((perject-close-project-save nil)
            (perject-close-project-confirmation nil))
        (perject-close-project name arg)))
    (when (file-exists-p (perject--get-project-directory name))
      (delete-directory (perject--get-project-directory name) t))))


;; (defun perject-switch-project (name)
;;   "Switch the project of the current frame to the project named NAME.
;; In interactive use, the user is asked for NAME."
;;   ;; This currently does not allow the user to create a new project this way. Maybe add a variable? TODO
;;   (interactive
;;    (list
;;     (perject--get-active-project
;;      "Switch to project: " nil
;;      "There currently is no project to switch to. Create one using `perject-create-project'.")))
;;   (perject--set-current-project name)
;;   (when perject-switch-project-message
;;     (message "Switched to project '%s'." name)))



(defun perject-new-frame (name)
  "Create a new frame for the project named NAME and switch to it.
In interactive use, if a prefix argument is supplied or the current
frame is not associated with any project, ask the user for NAME,
presenting the currently active projects as candidates.
Otherwise, use the current project."
  (interactive
   (list
    (if (or current-prefix-arg
            (not (perject--current-project)))
        (perject--get-active-project "Create new frame for project: " nil
                                     "No project to create a frame for.")
      (perject--current-project))))
  (let ((frame (make-frame (list (cons 'perject-project name)))))
    (select-frame frame)))
  

;; (defun perject-switch-project-new-frame (name)
;;   "Create a new frame and set its project to NAME.
;; In interactive use, the user is asked for NAME."
;;   (interactive
;;    (list
;;     (perject--get-active-project
;;      "Switch to project in new frame: " nil
;;      "There currently is no project to switch to. Create one using `perject-create-project'.")))
;;   (make-frame (list (cons 'perject-project name)))
;;   (when perject-switch-project-message
;;     (message "Switched to project '%s'." name)))


(defun perject-add-buffer-to-project (&optional arg)
  "Add the current buffer to the current project.
When ARG is non-nil or the current frame is not associated with any project,
ask the user to choose a project.
A new project can be created that way.
If the buffer is already associated with the project, display an error.
The command also prints a message when the variable `perject-add-buffer-message'
is non-nil."
  (interactive "P")
  (perject--add-buffer-to-project
   (current-buffer) (and (null arg) (perject--current-project)) perject-add-buffer-message))

(defun perject--auto-add-buffer-to-project ()
  "Silently add the current buffer to the current project.
Does nothing if they are already associated with each other or
if there is no current project."
  (let ((buffer (current-buffer))
        (project (perject--current-project)))
    (when (and (not (perject--is-assoc-with buffer project))
               project)
      (perject--add-buffer-to-project buffer project))))


;; TODO: add live-p checks to more callers: probably not caus we check in desktop-save
;; and i'd rather check once then regularly.
(defun perject--add-buffer-to-project (buffer name &optional print)
  "Add the buffer BUFFER to the project named NAME.
NAME may be a string or nil, in the latter case, the user is asked for the
project.
If no project named NAME exists, create one.
If the buffer is already associated with the project, display an error.
If PRINT is non-nil, also display a message upon completion.
Note that this function does not check whether BUFFER is still live or has
already been killed, so caller functions should make sure that the buffer in
question has not been killed."
  (let* ((name (or
                name
                (perject--get-active-project
                 "Add buffer to project: "
                 (perject--compose 'not (apply-partially 'perject--is-assoc-with (current-buffer)))
                 "There is no active project or all projects are already associated with the current buffer.")))
         (project (assoc name perject-projects))
         (projects (cdr project)))
    (cond
     ((and project (member buffer projects))
      (user-error "Buffer '%s' is already associated with project '%s'."
                  (buffer-name buffer) name))
     (project
      (setcdr project (cons buffer projects)))
     (t
      (setq perject-projects (cons (list name buffer) perject-projects))))
    (when print
      (message "Added buffer '%s' to project '%s'." (buffer-name buffer) name))))


(defun perject-remove-buffer-from-project (&optional arg)
  "Remove the current buffer from the current project.
If the buffer was not associated with the current project, throw a user-error.
When ARG is non-nil or the current frame is not associated with any project,
ask the user to choose a project.
The command also prints a message when the variable `perject-remove-buffer-message'
is non-nil.
The variable `perject-empty-project-delete' determines what happens if the last
buffer of a project is removed."
  (interactive "P")
  (perject--remove-buffer-from-project
   (current-buffer) (and (null arg) (perject--current-project)) perject-remove-buffer-message))

(defun perject-remove-buffer-from-project-and-kill (&optional arg)
  "Remove the current buffer from the current project and kill the buffer.
See the documentation of `perject-remove-buffer-from-project'."
  (interactive "P")
  (perject-remove-buffer-from-project arg)
  (kill-buffer (current-buffer)))


(defun perject--remove-buffer-from-project (buffer name &optional print)
  "Remove the buffer BUFFER from the project named NAME.
NAME may be a string or nil, in the latter case, the user is asked for the
project.
If no such project exists or the buffer is not associated with that project,
throw an error.
If PRINT is non-nil, also display a message upon completion.
The variable `perject-empty-project-delete' determines what happens if the last
buffer of a project is removed."
  (let* ((name (or
                name
                (perject--get-active-project
                 "Remove buffer from project: "
                 (apply-partially 'perject--is-assoc-with buffer)
                 "The buffer is currently not associated with any project.")))
         (project (assoc name perject-projects))
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
             (or (eq perject-empty-project-delete t)
                 (and (eq perject-empty-project-delete 'ask)
                      (y-or-n-p (format "Project '%s' is not associated with any buffers anymore. Delete it? " name)))))
      (let ((perject-delete-project-confirmation nil))
        (perject-delete-project name)))))


(defun perject-remove-buffer-from-all-projects (&optional print)
  "Remove the current buffer from all active projects.
When PRINT is non-nil, also display a message upon completion.
In interactive use, a message is printed unless a prefix-argument is supplied.
For example, this is called when a buffer is killed."
  (interactive
   (list (not current-prefix-arg)))
  (dolist (name (perject--active-projects))
    (when (perject--is-assoc-with (current-buffer) name)
      (perject--remove-buffer-from-project (current-buffer) name)))
  (when print
    (message "Removed buffer from all projects.")))

;; TODO
;; (defun perject-clear-buffers (name &optional kill)
;;   "Remove all buffers, which are associated with the project named NAME.
;; If KILL is non-nil (in the interactive case, if a prefix argument is supplied),
;; kill them. After calling this function, the project has no associated buffers.
;; If there is no project named NAME, do nothing."
;;   (interactive
;;    (list
;;     (perject--get-active-project
;;      "Select project to clear: " nil
;;      ;; could filter here for only projects that have a buffer, but not sure if needed (the string in the next line would need to be altered)
;;      "There currently is no project for which to clear buffers.")
;;     current-prefix-arg))
;;   (when kill
;;     (perject--kill-assoc-buffers name))
;;   (setf (alist-get name perject-projects nil nil 'equal) nil))


;; should this also allow merging? TODO
;; (defun perject-rename-project (old-name new-name)
;;   "Rename the project named OLD-NAME to NEW-NAME.
;; In interactive use, the user is asked for both project names."
;;   (interactive
;;    (list
;;     (perject--get-active-project
;;      "Select project to rename: " nil
;;      "There currently is no project to rename.")
;;     (perject--get-new-project-name "New name: "))))

;; TODO: maybe deal with case if the corresponding project does not exist or NAME is nil
;; use this to save projectless "project"?
(defun perject-save-project (name)
  "Save the project named NAME.
In interactive use, if a prefix argument is supplied or the current
frame is not associated with any project, ask the user for NAME,
presenting the currently active projects as candidates.
Otherwise, use the current project.
Intended for interactive use."
  (interactive
   (list
    (if (or current-prefix-arg
            (not (perject--current-project)))
        (perject--get-active-project "Save project: " nil
                                     "No project to save.")
      (perject--current-project))))
  (perject--desktop-save name))

(defun perject-save-all-projects (arg)
  "Save all currently active projects.
If ARG is non-nil (in interactive use, if a prefix argument is supplied),
the value of `perject-save-on-exit' is respected and only the projects
matching that variable's value are saved.
As a special case, if ARG is nil, this is interpreted to mean that
Emacs will exit shortly and therefore the corresponding locks of the
desktop files are removed.
This special case is completly irrelevant for the interactive use of this
function.
To save the buffers and frames not belonging to any project, run
`perject-save-non-project'."
  (interactive "P")
  (let* ((all-projects (perject--active-projects))
         (projects-to-save
          (if (or (not arg) (eq perject-save-on-exit t))
              all-projects
            (let ((projects
                   (if (functionp perject-save-on-exit)
                       (funcall perject-save-on-exit all-projects)
                     perject-save-on-exit)))
              (if (and projects (not (car projects)))
                  (cl-set-difference all-projects (cdr projects))
                projects)))))
    (dolist (name projects-to-save)
      (perject--desktop-save name)
      ;; Release the locks.
      (and (eq arg t)
           (perject--desktop-release-lock name)))))


;; (defun perject-save-all ()
;;   "Save the currently active projects to be restored next time.
;; If `perject-save-non-project' is non-nil, also save the buffers
;; which are not associated with any project.
;; Called by `perject-mode' before exiting Emacs (using `kill-emacs-hook')."
;;   (interactive)
;;   (dolist (name (perject--active-projects))
;;     (unless (member name perject-save-on-exit)
;;       (perject--desktop-save name)))
;;   (when perject-save-non-project
;;     (perject-save-non-project)))


;; Maybe integrate into open project or a new, more general function.
(defun perject-load-non-project ()
  "Load the buffers, which were not associated with any project.
These are saved in a desktop file directly in the directory specified by
`perject-directory'.
If no such desktop file exists, do nothing."
  (interactive)
  ;; I think it displays message when no desktop file exists.
  ;; TODO: Not sure if 'when' is needed.
  ;; (when (file-exists-p (desktop-full-file-name perject-directory))
  (perject--desktop-read nil))

(defun perject-save-non-project ()
  "Save the buffers and frames which were not associated with any project.
These are saved in a desktop file directly in the directory specified by
`perject-directory'."
  (interactive)
  (perject--desktop-save nil))



(defun perject--current-project (&optional frame)
  "Return the project currently associated with the frame FRAME.
If FRAME is nil, use the current frame."
  (frame-parameter frame 'perject-project))

(defun perject--set-current-project (name &optional frame)
  "Set the project of the frame FRAME to the project named NAME."
  (set-frame-parameter frame 'perject-project name))

(defun perject--get-buffers (name)
  "Return the list of buffers associated with the project named NAME."
  (alist-get name perject-projects nil nil 'equal))

;; The special case is used in the ibuffer integration.
(defun perject--is-assoc-with (obj name)
  "Return a non-nil value if object OBJ is associated with the project named NAME.
Otherwise, nil is returned.
OBJ may be a buffer or a frame.
As a special case, if NAME is nil and OBJ is a buffer, the function always
returns t. This is because a frame without a project is represented with a nil
value in the corresponding variable, whereas a buffer without a project simply
has no associated entry in `perject-projects'."
  (cond ((bufferp obj) (if name (member obj (perject--get-buffers name)) t))
        ((framep obj) (equal (perject--current-project obj) name))
        (t (error "perject--is-assoc-with: object is not a buffer or a frame."))))

(defun perject--is-active-project (name)
  "Return a non-nil value if there is a active project called NAME.
Otherwise, nil is returned."
  (member name (perject--active-projects)))

(defun perject--get-project-directory (name)
  "Return the directory belonging to a (possibly non-existent) project with name NAME."
  (concat perject-directory name))

(defun perject--get-project-frames (name)
  "Return the currently open frames which belong to the project named NAME."
  (cl-remove-if-not (lambda (frame) (perject--is-assoc-with frame name)) (frames-on-display-list)))


(defun perject--get-buffers-not-assoc ()
  "Return a list of all buffers not associated with any project."
  (let ((buffers (buffer-list)))
    (dolist (project perject-projects)
      (dolist (buffer (cdr project))
        (setq buffers (delete buffer buffers))))
    buffers))

;; TODO: add a function to close a project if its last corresponding frame is closed?
;; problem: what to do with the buffers? ask the user?
;; (defun perject--frame-closed (frame)
;;   "If the last frame belonging to a project is closed, close the project.")


(defun perject-mode-line-indicator ()
  "Return a string used for the mode line indicator of perject.
This function is only used when `perject-mode-line-format' is t."
  (and (perject--current-project)
       (propertize (concat (perject--current-project) " ")
                   'face 'perject-mode-line-face)))



;;; `desktop' integration
;; (setq desktop-restore-reuses-frames 'keep)


(defun perject--desktop-save (name)
  "Save the buffers of the project named NAME to their corresponding desktop file.
NAME may be nil, and in that case, the desktop file representing the buffers
which were not associated with any project are loaded.
Very similar to the `desktop-save' function."
  (setq desktop-dirname
        (file-name-as-directory
         (expand-file-name
          (perject--get-project-directory name))))
  (unless (file-exists-p desktop-dirname) (make-directory desktop-dirname))
  (save-excursion
    (let ((eager desktop-restore-eager)
	  (new-modtime (file-attribute-modification-time
			(file-attributes (desktop-full-file-name))))
          ;; Just save the buffers, which were not killed. Should not be needed,
          ;; because buffers are removed as soon as they are killed due to a function added to `kill-buffer-hook'.
          (buffer-list (if name
                           (cl-remove-if-not 'buffer-live-p (perject--get-buffers name))
                         (perject--get-buffers-not-assoc))))
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
	    (save-excursion (run-hooks 'desktop-save-hook)) ;; TODO: maybe make perject-desktop-save-hook
	    (goto-char (point-max))
	    (insert "\n;; Global section:\n")
	    ;; Called here because we save the window/frame state as a global
	    ;; variable for compatibility with previous Emacsen.
	    (perject--desktop-save-frameset name)
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
(defun perject--desktop-read (name &optional reuse-frames cleanup-frames)
  "Load the buffers of the project named NAME from the corresponding desktop file.
The stored data is added to `perject-projects'.
Note that NAME may be nil, in that case, the desktop file representing the
buffers which were not associated with any project, are loaded.
For REUSE-FRAMES and CLEANUP-FRAMES, consult the documentation of `frameset-restore'.
Similar to the `desktop-read' function."
  (unless noninteractive
    (setq desktop-dirname
          (file-name-as-directory
           (expand-file-name
            (perject--get-project-directory name))))
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
            (setq perject--desktop-current-project name)
	    ;; Evaluate desktop buffer and remember when it was modified.
	    (setq desktop-file-modtime (file-attribute-modification-time
					(file-attributes
					 (desktop-full-file-name))))
	    (load (desktop-full-file-name) t t t)
            (setq perject--desktop-current-project nil)

            ;; If no buffers were restored, we need to manually add the new project to `perject-projects'.
            (unless (or (not name) (perject--is-active-project name))
              (setq perject-projects (cons (list name) perject-projects)))
            
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
            (setq perject--desktop-restored-frames nil)
            
	    (perject--desktop-restore-frameset reuse-frames cleanup-frames)
            ;; Assign the newly created frames to their projects.
            (dolist (frame perject--desktop-restored-frames)
              (perject--set-current-project name frame))
	    (run-hooks 'desktop-after-read-hook)
	    (message "Perject:%s %s%d buffer%s restored%s%s."
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
      (message "Perject: No desktop file.")
      nil)))

(defun perject--desktop-restore-frameset (&optional reuse-frames cleanup-frames)
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
                                          (push frame perject--desktop-restored-frames))
                                        (when (functionp cleanup-frames)
                                          (funcall cleanup-frames frame action)))
		      :force-display desktop-restore-in-current-display
		      :force-onscreen desktop-restore-forces-onscreen)))


(defun perject--desktop-save-frameset (name)
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
                                         (equal (perject--current-project frame) name))))))

(defun perject--desktop-release-lock (name)
  "Release the lock of the desktop file belonging to NAME.
NAME is a project name. The lock is only released if the current Emacs instance
had aquired it before. "
  ;; From the definition of `desktop-kill'.
  (when (eq (emacs-pid) (desktop-owner (perject--get-project-directory name)))
    (desktop-release-lock (perject--get-project-directory name))))

;; To change the behavior of `desktop-create-buffer', we use advice.
;; This makes it that a restored buffer will know which project they belong to.
;; TODO: This should probably go into the mode.
(advice-add 'desktop-create-buffer :filter-return 
              (lambda (buffer)
                (when buffer
                  (when perject--desktop-current-project
                    ;; buffer restored correctly and there is a current project
                    (perject--add-buffer-to-project buffer perject--desktop-current-project))
                  (run-hook-with-args 'perject-after-desktop-create-buffer-hook buffer)
                  buffer)))

  
  
(defun perject--init ()
  "Load projects from the last session and set up hooks. 
The projects are stored in desktop files.
If Emacs was launched with the command line option
`perject-command-line-option-no-load', no projects are loaded automatically.
The variables `perject-load-on-startup' and `perject-load-non-project'
determine which projects are loaded automatically."
  (let* ((current-frame (selected-frame))
         (all-projects (perject--saved-projects))
         (projects-to-load
          (if (eq perject-load-on-startup t)
              all-projects
            (let ((projects
                   (if (functionp perject-load-on-startup)
                       (funcall perject-load-on-startup all-projects)
                     perject-load-on-startup)))
              (if (and projects (not (car projects)))
                  (cl-set-difference all-projects (cdr projects))
                projects))))
         (load-non-project perject-load-non-project))
    (when (member perject-command-line-option-no-load command-line-args)
      (setq projects-to-load nil
            load-non-project nil
            command-line-args
            (delete perject-command-line-option-no-load command-line-args)))
    ;; When restoring a frame, Emacs will change which buffer is displayed in the selected frame.
    ;; Therefore, we always want the "starting frame" (which we use as a temporary frame)
    ;; to be selected when loading a desktop file.
    (dolist (name projects-to-load)
      (perject--desktop-read name)
      ;; (select-frame current-frame)
      )
    (when load-non-project
      (perject-load-non-project))
    ;; There always is the "starting frame", which gets created upon launch.
    ;; We remove it, unless it is the only frame, of course.
    (unless (eq (length (frame-list)) 1)
      (delete-frame current-frame))
    ;; Add the hooks specified in `perject-auto-add-in-hooks'.
    (dolist (hook perject-auto-add-in-hooks)
      (add-hook hook 'perject--auto-add-buffer-to-project))
    (run-hooks 'perject-after-init-hook)))


(defun perject--exit ()
  "Save projects to be restored next time and prepare to exit Emacs.
The variables `perject-save-on-exit' and `perject-save-non-project'
determine which projects are saved. 
Called by `perject-mode' before exiting Emacs (using `kill-emacs-hook')."
  (perject-save-all-projects t)
  (when perject-save-non-project
    (perject-save-non-project)))


(defun perject--active-projects ()
  "Return a list containing the names of all active projects."
  (mapcar 'car perject-projects))


(defun perject--saved-projects ()
  "Return a list of the names of all saved projects.
The saved projects are those which have a folder in `perject-directory'."
  (and (file-exists-p perject-directory)
       (remove ".."
               (remove "."
                       (mapcar 'car
                               (cl-remove-if-not
                                (lambda (elem)
                                  (eq (cadr elem) t))
                                (directory-files-and-attributes perject-directory)))))))


;; Could also define a variable which holds the non-saved (new) projects.
;; However, this will then need to be checked in desktop loading, which I don't like.

(defun perject--all-projects ()
  "Return a list of all projects.
This includes those which have a folder in `perject-directory'
and those which were newly created and do not yet have such a folder."
  (let* ((saved-projects (perject--saved-projects))
         (new-projects
          (cl-remove-if
           (lambda (name) (member name saved-projects))
           (perject--active-projects))))
    (append saved-projects new-projects)))


(defun perject--get-active-project (prompt predicate no-candidate-error)
  "Ask the user for an active project name using `completing-read'.
An active project is a project, whose frames and buffers were loaded
and thus are available to the user.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
PREDICATE should be nil or a function, which takes a project name as an
argument. In the latter case, only projects are displayed that satisfy that
function, i.e. for for which PREDICATE returns a non-nil value.
NO-CANDIDATE_ERROR is a string used in the `user-error', which is thrown if
there is no project to select from."
  (perject--get-project-interface
   prompt (perject--active-projects) predicate
   (when (or (not (functionp predicate))
             (funcall predicate (perject--current-project)))
     (perject--current-project))
   (lambda (str)
     (concat str
             (or
              (and (equal (perject--current-project) str)
                   " [current frame]")
              (and (cl-some
                    (lambda (frame)
                      (equal (perject--current-project frame) str))
                    (frames-on-display-list))
                   " [other frame]"))))
   no-candidate-error
   (lambda () (user-error "The name of a project can not be empty."))))


(defun perject--get-saved-project (prompt predicate no-candidate-error)
  "Ask the user for a saved project's name using `completing-read'.
The saved projects are those which have a folder in `perject-directory'.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
PREDICATE should be nil or a function, which takes a project name as an
argument. In the latter case, only projects are displayed that satisfy that
function, i.e. for for which PREDICATE returns a non-nil value.
NO-CANDIDATE_ERROR is a string used in the `user-error', which is thrown if
there is no project to select from."
  (perject--get-project-interface
   prompt (perject--saved-projects) predicate
   ;; Only supply the current project as a default argument, if it passes predicate.
   (when (or (not (functionp predicate))
             (funcall predicate (perject--current-project)))
     (perject--current-project))
   (lambda (str)
     (concat str
             (or
              (and (equal (perject--current-project) str)
                   " [current frame]")
              (and (cl-some
                    (lambda (frame)
                      (equal (perject--current-project frame) str))
                    (frames-on-display-list))
                   " [other frame]")
              (and (eq (desktop-owner (perject--get-project-directory str))
                       (emacs-pid))
                   " [loaded]")
              (and (desktop-owner (perject--get-project-directory str))
                   " [loaded in other instance]"))))
   no-candidate-error
   (lambda () (user-error "The name of a project can not be empty."))))


(defun perject--get-project (prompt predicate no-candidate-error)
  "Ask the user for a project's name using `completing-read'.
This includes active and inactive projects.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
PREDICATE should be nil or a function, which takes a project name as an
argument. In the latter case, only projects are displayed that satisfy that
function, i.e. for for which PREDICATE returns a non-nil value.
NO-CANDIDATE_ERROR is a string used in the `user-error', which is thrown if
there is no project to select from."
  (perject--get-project-interface
   prompt (perject--all-projects) predicate
   ;; Only supply the current project as a default argument, if it passes predicate.
   (when (or (not (functionp predicate))
             (funcall predicate (perject--current-project)))
     (perject--current-project))
   (lambda (str)
     (concat str
             (or
              (and (equal (perject--current-project) str)
                   " [current frame]")
              (and (cl-some
                    (lambda (frame)
                      (equal (perject--current-project frame) str))
                    (frames-on-display-list))
                   " [other frame]")
              (and (eq (desktop-owner (perject--get-project-directory str))
                       (emacs-pid))
                   " [loaded]")
              (and (desktop-owner (perject--get-project-directory str))
                   " [loaded in other instance]"))))
   no-candidate-error
   (lambda () (user-error "The name of a project can not be empty."))))


(defun perject--get-project-interface
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
              (user-error no-candidate-error)))
         (pretty-printer (or pretty-printer #'identity))
         (collection
            (mapcar pretty-printer projects))
         (default-projects
           (if (listp default)
               (mapcar pretty-printer default)
             (funcall pretty-printer default)))
         (selection
          (completing-read
           prompt collection nil 0 nil perject-project-name-hist default-projects))
         (index (cl-position selection collection :test 'equal))
         (name (if index (nth index projects)
                 selection)))
    (if (string-empty-p name)
        (funcall empty-string-function)
      name)))

(defun perject--get-new-project-name (prompt)
  "Ask the user for a project name using `read-string' and return it.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
The string entered must be a valid project name, i.e. it must pass
`perject--valid-project-name' and be non-empty.
Moreover, there may not already exist a project of the same name.
If one of those conditions is violated, an error is thrown."
  (let ((name (read-string prompt)))
    (perject--nonempty-project-name name)
    (perject--valid-project-name name)
    (when (member name (perject--all-projects))
      (user-error "There already is a project named '%s'." name))
    name))

(defun perject--valid-project-name (name)
  "Check if the string NAME is a potential project name.
This means that it only contains letters, digits or characters,
which are specified in the variable `perject-valid-naming-chars'.
If a name does not match, throw a `user-error'.
This function does not check that NAME is not empty, but the function
`perject--nonempty-project-name' does just that."
  (mapcar
   (lambda (char)
     (or (and (>= char 48) (<= char 57)) ;; digit
         (and (>= char 65) (<= char 90)) ;; uppercase letter
         (and (>= char 97) (<= char 122)) ;; lowercase letter
         (member char perject-valid-naming-chars)
         (user-error "The character '%c' is not valid for naming a project. See the variable `perject-valid-naming-chars'."
                   char)))
   name))

(defun perject--nonempty-project-name (name)
  "Check if the string NAME is non-empty and thus a potential project name.
Otherwise, throw an error."
  (when (string-empty-p name)
    (user-error "The name of a project can not be empty.")))




(defun perject--compose (f g)
  "Compose the two functions F and G.
It returns the function that first applies g and then f."
  `(lambda (x) (,f (,g x))))


;; TODO: Make this indpenedent of the directory!
;; (with-eval-after-load 'ivy
;;   (require 'perject-ivy)) ;; "~/m/3/1/dev/elisp/emacs-pkg/perject/perject-ivy.el"

;; (with-eval-after-load 'ibuffer
;;   (require 'perject-ibuffer)) ;; "~/m/2/1/dev/elisp/emacs-pkg/perject/perject-ibuffer.el"


(provide 'perject)
;;; perject.el ends here
