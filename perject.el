;;; perject.el - Session-persistent project management -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: overideal
;; Maintainer: overideal
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (dash "2.12"))
;; Homepage: https://gitlab.com/overideal/perject

;;; Commentary:
;; This package allows the user to manage multiple projects in a single Emacs instance.
;; A project consists of a list of buffers and a list of frames, which have
;; corresponding window configurations.
;; By leveraging the built-in package `desktop.el', the projects are
;; automatically saved and restored upon restarting Emacs.


;;; Code

(require 'desktop)
(require 'cl-lib)
(require 'seq)
(require 'dash)


;;;; Constants

(defconst perject-command-line-option "--perject"
  "Command line option which if present overwrites `perject-load-at-startup'.
For example, if Emacs is launched with '--perject \"project 1,project 2\"', the
projects \"project 1\" and \"project 2\" are loaded.
Write '--perject \"\"' if no projects should be opened.")


;;;; Customization

(defgroup perject nil
  "Session-persistent project separation"
  :group 'convenience
  :prefix "perject-")

(defface perject-mode-line-face '((t :foreground "dark orange"))
  "The face used by the mode line indicator of perject.")

(defface perject-project-annotator-main '((t :inherit font-lock-keyword-face))
  "The face used when selecting projects for displaying the main annotation.")

(defface perject-project-annotator-buffers '((t :inherit completions-annotations))
  "The face is used for displaying the number of buffers when selecting a project.")

(defface perject-project-annotator-frames '((t :inherit completions-annotations))
  "The face is used for displaying the number of frames when selecting a project.")

(defcustom perject-directory "~/.emacs.d/perject/"
  "The directory used by perject to save the data."
  :type 'directory)

(defcustom perject-load-at-startup nil
  "The variable controls which projects are automatically loaded at startup.

It may have one of the following values:
- nil: Load no project.
- all: Load all projects but not the anonymous one.
- all-and-anonymous: Load all projects including the anonymous one.
- A list of project names: Load the specified projects (if existent).
  If a name in the list does not correspond to an existing project, do nothing.
  As a special case, the value nil in the list is interpreted to refer to the
  anonymous project, which corresponds to the buffers not belonging to any
  project. Furthermore, if the first element of the list is t, all projects but
  the ones specified in the list are loaded.
- 'previous: Load the projects that were open at the end of the previous
  session. The anonymous project is loaded if there is at least one anonymous
  buffer or frame.
- A function: The function is called with two arguments, namely the list of
  projects opened in the previous Emacs session and the list of saved projects.
  The function should return a list of project names which is then loaded (with
  the same convention for nil as above). It could for example ask the user.

The order in which the projects are loaded is determined as follows: In the list
or function case, the projects are loaded \"from left to right\". Therefore, it
usually makes sense to put the anonymous project at the front of the list and
your \"main\" project at the back.
If the value is 'previous, the recently opened projects are restored later, so
that the most recent project will be restored last. For convenience, perject
provides the function `perject-sort-projects' in case the user wants to manually
change the \"recency\" of the projects."
  :type '(choice
		  (const :tag "Load no project" nil)
		  (const :tag "Load all projects but not the anonymous one" all)
		  (const :tag "Load all projects including the anonymous one" all-and-anonymous)
		  (repeat :tag "Load the specified projects (if existent)"
				  (choice string (const nil) (const t)))
		  (const :tag "Load previously open projects" previous)
		  (function :tag "Custom function")))

(defcustom perject-save-on-exit 'all-and-anonymous
  "The variable controls which projects are automatically saved when exiting Emacs.
It may have one of the following values:
- nil: Don't save any projects on exit.
- all: Save all projects but not the anonymous one.
- all-and-anonymous: Save all projects including the anonymous one.
- A list of project names: Save the specified projects (if existent).
  As a special case, the value nil in the list is interpreted to refer to the
  anonymous project, which corresponds to the buffers not belonging to any
  project. Furthermore, if the first element of the list is t, all projects but
  the ones in the list are saved upon exiting Emacs.
- A function: The function is called with two arguments, namely the list of
  projects opened in the previous Emacs session and the list of active projects.
  The function should return the list of project names to be saved (with the
  same convention for nil as above). It could for example ask the user."
  :type '(choice
		  (const :tag "Don't save any projects on exit" nil)
		  (const :tag "Save all projects but not the anonymous one" all)
		  (const :tag " Save all projects including the anonymous one" all-and-anonymous)
		  (repeat :tag "Save the specified projects (if existent)"
				  (choice string (const nil) (const t)))
		  (function :tag "Custom function")))

(defcustom perject-buffers-not-to-save-function nil
  "Function identifying buffers that are to be excluded when saving a project to its desktop file.
Buffers not belonging to the current project are never saved. In case of the
anonymous project, only the anonymous buffers may be saved (i.e. those not
belonging to any project).
If this variable is nil, no additional filtering takes place.
The function is called with five arguments: the current project name (which is
nil for the anonymous project), the name of the visited file, the buffer name,
the major mode and the list of active minor modes. It should return nil if the
buffer should not have its state saved in the desktop file.
This variable corresponds to `desktop-buffers-not-to-save-function'."
  :type '(choice function (const nil)))

(defcustom perject-auto-add-function nil
  "A function used to control which buffers are automatically associated with projects.
When a hook in `perject-auto-add-hooks' runs, this function is called in order
to decide to which projects the current buffer should be added to. It is called
with two arguments, namely the project name and the current buffer. The project
name may also be nil. The function must return a list of project names to which
the buffer should be added. Any nil entries in the list are ignored. In
particular, by returning nil (the empty list) the buffer is not added to any
project.
Setting this variable to nil means always add the buffer to the current project.
This variable is intended to allow for advanced customization and thus for the
majority of use cases, the value nil should suffice."
  :type '(choice function (const nil)))

;; There is no hook that is run after an arbitrary buffer is created.
;; See: https://stackoverflow.com/questions/7899949/is-there-an-emacs-hook-that-runs-after-every-buffer-is-created
(defcustom perject-auto-add-hooks '(find-file-hook clone-indirect-buffer-hook dired-mode-hook)
  "A list of hooks in which the current buffer is added to the current project.
This is used to automatically add newly created buffers to the current project.
The following hooks could be interesting to the user:
`find-file-hook', `clone-indirect-buffer-hook',
`buffer-list-update-hook' and many mode hooks.
Modifcations of this variable only take effect after (re)enabling
`perject-mode'.
Internally, the function `perject--auto-add-buffer' is used."
  :type '(list variable))

(defcustom perject-close-project-save 'ask
  "This variable controls if a project is saved when closed using `perject-close-project'.
It may have one of the following values:
- t: Always save the project.
- 'ask: Ask the user if the project should be saved.
- nil: Never save the project.
- A function: The function is called with the project name as a single argument.
It should return non-nil if and only if the project is to be saved."
  :type '(choice
		  (const :tag "Always save the project" t)
		  (const :tag "Ask the user if the project should be saved" ask)
		  (const :tag "Never save the project" nil)
		  (function :tag "Custom function")))

(defcustom perject-frame-to-project-message t
  "If non-nil, print a message when adding or removing a frame from a project.
This influences the commands `perject-add-frame-to-project' and
`perject-remove-frame-from-project'."
  :type '(choice
		  (const :tag "Print message" t)
		  (const :tag "Don't print message" nil)))

(defcustom perject-switch-to-new-frame '(open create)
  "Whether to switch to a newly created frame.
The value of this variable is a list which may contain any of the following
symbols, whose presence in the list has the mentioned effect.
- 'open: Switch after opening a project using `perject-open-project'.
- 'create: Switch after creating a new project using `perject-open-project'.
- 'reload: Switch after reloading a project using `perject-reload-project'.
In particular, if this variable is nil (i.e. the empty list), never switch to a
newly created frame."
  :type '(set
		  (const :tag "Switch after opening a project using `perject-open-project'" open)
		  (const :tag "Switch after creating a new project using `perject-open-project'" create)
		  (const :tag "Switch after reloading a project using `perject-reload-project'" reload)))

(defcustom perject-buffer-to-project-message t
  "If non-nil, print a message when adding or removing a buffer from a project.
This influences the commands `perject-add-buffer-to-project',
`perject-remove-buffer-from-project',
`perject-remove-buffer-from-project-and-kill' and
`perject-remove-buffer-from-all-projects'."
  :type '(choice
		  (const :tag "Print message" t)
		  (const :tag "Don't print message" nil)))

(defcustom perject-close-project-confirmation nil
  "If non-nil, ask for confirmation before closing a project with `perject-close-project'."
  :type '(choice
		  (const :tag "Ask for confirmation" t)
		  (const :tag "Don't ask for confirmation" nil)))

(defcustom perject-reload-project-confirmation t
  "If non-nil, ask for confirmation before reloading a project with `perject-reload-project'."
  :type '(choice
		  (const :tag "Ask for confirmation" t)
		  (const :tag "Don't ask for confirmation" nil)))

(defcustom perject-delete-project-confirmation t
  "If non-nil, ask for confirmation before deleting a project with `perject-delete-project'."
  :type '(choice
		  (const :tag "Ask for confirmation" t)
		  (const :tag "Don't ask for confirmation" nil)))

(defcustom perject-kill-buffers-confirmation 'all
  "Whether to ask for confirmation before killing buffers when closing or deleting a project.
This influences the commands `perject-close-project' and
`perject-delete-project'.
It may have one of the following values:
- t: Ask for confirmation.
- 'all: Ask for confirmation only when killing all buffers of the project.
- nil: Don't ask for confirmation."
  :type '(choice
		  (const :tag "Ask for confirmation" t)
		  (const :tag "Ask for confirmation only when killing all buffers of the project" all)
		  (const :tag "Don't ask for confirmation" nil)))

(defcustom perject-empty-project-delete 'ask
  "This variable controls what happens when the last buffer is removed from a project.
It may have one of the following values:
- t: Delete the project.
- 'ask: Ask the user whether the project should be deleted.
- nil: Don't delete the project.
- A function: The function is called with the project name as a single argument.
  It should return non-nil if and only if the project is to be kept."
  :type '(choice
		  (const :tag "Delete the project" t)
		  (const :tag "Ask the user whether the project should be deleted" ask)
		  (const :tag "Don't delete the project" nil)
		  (function :tag "Custom function")))

(defcustom perject-mode-line-format t
  "This variable determines the format of the mode line indicator of `perject'.
It may have one of the following values:
- nil: No mode line entry is shown for perject.
- A function: Call that function with a single string as an argument, which is
  the name of the current project. The function should return the string to
  display in the mode line. Note that this function will get nil as an argument
  if the current frame is not associated with any project.
- t: Use the default format, which is simply the name of the project with the
  face `perject-mode-line-face'."
  :type '(choice
		  (const :tag "No mode line entry is shown for perject" nil)
		  (function :tag "Custom function")
		  (const :tag "Use the default format" t)))

(defcustom perject-frame-title-format t
  "This variable determines the format of the title of a frame.
It may have one of the following values:
- nil: The title of a frame is not altered by perject.
- A function: Call that function with a single string as an argument, which is
  the name of the current project. The function should return a valid value for
  `frame-title-format', which is used to alter the frame title. See the
  documentation of that variable. Note that this function will get nil as an
  argument if the current frame is not associated with any project.
- t: Use the default format, which looks like \"emacs@computer:project\".
Note that this variable has no effect on manually named sessions,
because it is used with `frame-title-format'. See its documentation."
  :type '(choice
		  (const :tag "Don't alter the title of the frame")
		  (function :tag "Custom function")
		  (const "Use the default format" t)))

(defcustom perject-valid-naming-chars '(?_ ?- ? )
  "A list of characters that may be used to name a project.
All letters and digits are always allowed.
Note that the name of the project has to also be a valid (possibly non-existent)
directory name, so be careful. By default, this variable allows '_', '-' and
' '. This variable is used in the function `perject--get-new-project-name'.
Do not add a commata (,) to this list, as that character is used in the command
line parameter `perject-command-line-option' of perject to separate project
names."
  :type '(repeat character))


;;;; Hooks

(defcustom perject-after-init-hook nil
  "Hook run after perject has initialized.
This means that all the buffers and frames from the projects that were
configured to load automatically have been restored.
The functions are called with one argument, namely the list of project names
that were restored. This list is ordered (leftmost project was restored first)
and nil represents the anonymous project."
  :type 'hook)

(defcustom perject-before-open-hook nil
  "Hook run before perject opens a project using `perject-open-project'.
In particular, the buffers and frames from the project have not yet been
restored.
The functions are called with one argument, namely the name of the project to be
opened."
  :type 'hook)

(defcustom perject-after-open-hook nil
  "Hook run after perject has opened a project using `perject-open-project'.
In particular, all the buffers and frames from the project have been restored.
The functions are called with one argument, namely the name of the newly opened
project.
The variable `perject--desktop-restored-frames' is the list of newly created
frames and might be useful."
  :type 'hook)

(defcustom perject-before-create-hook nil
  "Hook run before perject creates a new project using `perject-open-project'.
The functions are called with one argument, namely the name of the closed
project."
  :type 'hook)

(defcustom perject-after-create-hook nil
  "Hook run after perject has created a new project using `perject-open-project'.
The functions are called with one argument, namely the name of the closed
project."
  :type 'hook)

(defcustom perject-before-close-hook nil
  "Hook run before perject closes a project using `perject-close-project'.
The functions are called with one argument, namely the name of the project to be
closed."
  :type 'hook)

(defcustom perject-after-close-hook nil
  "Hook run after perject has closed a project using `perject-close-project'.
The functions are called with one argument, namely the name of the closed
project."
  :type 'hook)

(defcustom perject-before-reload-hook nil
  "Hook run before perject reloads a project using `perject-reload-project'.
The functions are called with one argument, namely the name of the project to be
reloaded."
  :type 'hook)

(defcustom perject-after-reload-hook nil
  "Hook run after perject has reloaded a project using `perject-reload-project'.
The functions are called with one argument, namely the name of the reloaded
project."
  :type 'hook)

(defcustom perject-desktop-after-load-hook nil
  "Hook run after perject has loaded a project from its desktop file.
The functions are called with one argument, namely the name of the project whose
desktop file was just loaded.
This hook corresponds to `desktop-after-read-hook'."
  :type 'hook)

(defcustom perject-desktop-save-hook nil
  "Hook run before perject has saved a project to its desktop file.
The functions are called with one argument, namely the name of the project whose
desktop file will be saved.
This hook corresponds to `desktop-save-hook'."
  :type 'hook)

(defcustom perject-after-desktop-create-buffer-hook nil
  "Hook run after a buffer has been successfully restored by desktop.
The functions should take one argument, which is the newly created buffer and
their return values are ignored.
This hook should be used with care."
  :type 'hook)


;;;; Internal Variables

(defvar perject-projects nil
  "A list representing the projects.
Each element is a list, where the car (first element) is a string,
which is the name of the project and the cdr is the list of buffers
belonging to that project.
Every project also has a list of associated frames.")

(defvar perject-project-name-history nil
  "The history of project names.")

;; I am surprised such a variable does not already exist in `frameset.el'
(defvar perject--desktop-restored-frames nil
  "The list of the frames which were restored for the most recent project.
Should not be modified by the user.")

(defvar perject--desktop-current-project nil
  "Internal variable that is set to the current project name while saving or loading a project.
This is e.g. used to assign the correct project to newly restored buffers and
frames. Should not be modified by the user.")

(defvar perject--desktop-reuse-frames nil
  "Internal parameter for `perject-desktop-restore-frameset-advice'.
Should not be modified by the user.")

(defvar perject--desktop-cleanup-frames nil
  "Internal parameter for `perject-desktop-restore-frameset-advice'.
Should not be modified by the user.")

(defvar perject--frame-title-backup nil
  "Internal variable used to save the value of `frame-title-format'; to restore it later.
Should not be modified by the user.")

(defvar perject--previous-projects nil
  "Internal variable that stores the projects that were opened in the last session.
The anonymous project (represented by the value nil) is contained if and only if
there was at least one anonymous buffer or frame at the end of the previous
session.
Should not be modified by the user.")


;;;; The Mode

(define-minor-mode perject-mode
  "Group buffers and frames into projects which are preserved when restarting."
  :global t
  :keymap (make-sparse-keymap)
  (if perject-mode
      (progn
		;; Create main directory if not already existent.
		(unless (file-exists-p perject-directory)
		  (make-directory perject-directory))

        (add-hook 'after-init-hook #'perject--init)
        (add-hook 'kill-emacs-hook #'perject--exit)
        (add-hook 'kill-buffer-hook #'perject-remove-buffer-from-all-projects)

		;; To change the behavior of `desktop-create-buffer', we use advice.
		;; This makes it that a restored buffer will know which project they belong to.
		(advice-add 'desktop-create-buffer :filter-return #'perject-desktop-create-buffer-advice)
		(advice-add 'desktop-restore-frameset :override #'perject-desktop-restore-frameset-advice)

        (when after-init-time
          ;; This means the mode got enabled and the init phase is already over.
          ;; I.e. later, manually by the user.
          ;; In that case, `after-init-hook' is not run.
          (dolist (hook perject-auto-add-hooks)
            (add-hook hook 'perject--auto-add-buffer)))
        
        ;; The mode line knows to which mode this belongs to, and if the mode is not active, the entry is not shown.
        ;; We don't add anything to the misc mode line if `perject-mode-line-format' is nil,
        ;; we already have added something to it or if `mode-line-misc-info' is nil (which should never happen).
        (unless (or (not perject-mode-line-format)
                    (assoc 'perject-mode mode-line-misc-info)
                    (not mode-line-misc-info))
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
                               (concat ":" (perject--current-project))))))))))

	;; Remove the added hooks.
    (remove-hook 'after-init-hook #'perject--init)
    (remove-hook 'kill-emacs-hook #'perject--exit)
    (remove-hook 'kill-buffer-hook #'perject-remove-buffer-from-all-projects)
	;; Remove the advice.
	(advice-remove 'desktop-create-buffer #'perject-desktop-create-buffer-advice)
	(advice-remove 'desktop-restore-frameset #'perject-desktop-restore-frameset-advice)
    (dolist (hook perject-auto-add-hooks)
      (remove-hook hook #'perject--auto-add-buffer))
    (setq frame-title-format perject--frame-title-backup
          perject--frame-title-backup nil)))


(defun perject-mode-line-indicator ()
  "Return a string for the mode line indicator of perject.
This function is used only if `perject-mode-line-format' is t."
  (and (perject--current-project)
       (propertize (concat (perject--current-project) " ")
                   'face 'perject-mode-line-face)))


(defun perject--init ()
  "Load projects from the last session, set up hooks and select the last restored frame.
The projects are stored in desktop files. The variable `perject-load-at-startup'
determines which projects are loaded automatically. However, if specified, the
command line option `perject-command-line-option' takes priority."
  (let* ((current-frame (selected-frame))
         (all-projects (perject--list-projects))
		 (all-projects-anonymous (cons nil all-projects))
         (projects-to-load
		  (pcase perject-load-at-startup
			('nil nil)
			('all all-projects)
			('all-and-anonymous all-projects-anonymous)
			((pred listp) (if (eq (car perject-load-at-startup) t)
							  (cl-set-difference all-projects-anonymous
												 (cdr perject-load-at-startup)
												 :test #'string-equal)
							perject-load-at-startup))
			('previous perject--previous-projects)
			((pred functionp) (funcall perject-load-at-startup
									   perject--previous-projects
									   all-projects)))))
	;; Read the command line arguments.
	;; We cannot use `command-switch-alist' since those functions are processed after `after-init-hook'.
	(when-let ((index (cl-position perject-command-line-option command-line-args :test #'string-equal))
			   (list (split-string (nth (1+ index) command-line-args) ","))
			   (projects (-separate (lambda (name)
									  (or (perject--is-project name) (string-equal name "anonymous")))
									list)))
	  (when (cadr projects)
		(message "Perject: Warning: The following projects do not exist: %s" (string-join (cadr projects) ", ")))
	  (setq projects-to-load
			(mapcar (lambda (name) (if (string-equal name "anonymous") nil name))
					(car projects))
            command-line-args
			(append (seq-take command-line-args index) (seq-drop command-line-args (+ index 2)))))
	;; At the beginning, there is a single frame; namely the selected "starting frame".
	;; We may reuse this frame for one of our projects, but as soon as it is "claimed",
	;; we cannot use it again. This behavior is obtained by setting `perject--desktop-reuse-frames'.
	(let ((perject-switch-to-new-frame nil)
		  (perject--desktop-reuse-frames (lambda (frame)
										   (and
											(eq frame current-frame)
											(not (perject--current-project frame))))))
      (dolist (name projects-to-load)
		(if (member current-frame perject--desktop-restored-frames)
			;; Starting frame was claimed.
			(let (perject-before-open-hook perject-after-open-hook)
			  (perject-open-project name))
		  (perject-desktop-load name))))
    ;; Add the hooks specified in `perject-auto-add-hooks'.
    (dolist (hook perject-auto-add-hooks)
      (add-hook hook 'perject--auto-add-buffer))
	;; Select the last restored frame.
	(when-let ((frames (car (last (remove 'nil (mapcar #'perject--get-frames projects-to-load))))))
	  (select-frame-set-input-focus (car frames))
	  (raise-frame (car frames)))
    (run-hook-with-args 'perject-after-init-hook projects-to-load)))

(defun perject--exit ()
  "Save projects to be restored next time and prepare to exit Emacs.
The variabe `perject-save-on-exit' determines which projects are saved.
This function is called by `perject-mode' before exiting Emacs (using
`kill-emacs-hook')."
  (perject-save-projects 'exit t t)
  ;; Reverse the list of active projects, so that the newest project is last.
  (let ((projects (nreverse (perject--list-projects 'active))))
	(setq perject--previous-projects
		  (if (or
			   (perject--get-frames nil)
			   (cl-remove-if-not
				(lambda (buffer) (apply #'desktop-save-buffer-p (cdr (desktop-buffer-info buffer))))
				(perject--get-anonymous-buffers)))
			  (cons nil projects)
			projects))))


;;;; Opening Projects

;; The `perject-open-project' command is very subtle, due to the following:
;; When loading a desktop, `desktop.el' selects the loaded buffers in the
;; currently selected frame. This will not only mess up the window configuration
;; but also due to the various hooks alter the list of buffers belonging to
;; other projects. In order to prevent this from happening, we simply disable
;; the hooks in `perject-auto-add-hooks' and reenable them after loading. If the
;; user has set up additional such hooks, they need to be disabled (and
;; reenabled later) by adding an appropriate function to
;; `perject-before-open-hook' and `perject-after-open-hook'.
;; An alternative approach would be to remember the previous value and only add
;; the new buffers to the new project. However, this feels less clean since in
;; that case every buffer is added "two times" if there is a `find-file' hook.
;; In particular, the alternative approach would lead to "Buffer is already
;; associated with project" errors, if the project in which this function is
;; called is already associated with the buffer.
(defun perject-open-project (name)
  "Open the project named NAME and switch to one of its corresponding frames.
This means that all its buffers are restored and frames are opened from the
corresponding desktop file. Whether the frame switch happens depends on the value
of `perject-switch-to-new-frame'.
Note that the switching might not work, depending on the window manager. If
there is no project named NAME yet, create it and open a frame for it.
Before creating the project, run `perject-before-create-hook' and afterwards run
`perject-after-create-hook'.
If no new project is created, run the hooks `perject-before-open-hook' and
`perject-after-open-hook' instead.
In interactive use, the user is asked for the project name.
If NAME is nil or the empty string (in interactive use, if the user exits with
the empty string), restore the anonymous project (the buffers not belonging to
any project).
If you want to further change the list of projects, do so in
`perject-before-open-hook' or `perject-after-open-hook'. Hooks that e.g. add a
buffer to a project have no effect while the desktop is being loaded."
  (interactive (list (perject--get-project-name "Open project: " 'inactive nil nil nil nil #'ignore)))
  (when (perject--is-active-project name)
	(user-error "The project '%s' is already open" name))
  (if (or (perject--is-project name) (not name))
	  (progn
		(dolist (hook perject-auto-add-hooks)
		  (remove-hook hook #'perject--auto-add-buffer))
		(run-hook-with-args 'perject-before-open-hook name)
		;; Ensure that `desktop-read' does not change the window configuration of the current frame.
		;; By using `desktop-after-read-hook', we give the functions in
		;; `perject-desktop-after-load-hook' the possibility to modify the
		;; window configuration.
		;; If we e.g. used `save-window-excursion', then those functions would
		;; not be able to change the window configuration of the current frame.
		(let ((wc (current-window-configuration)))
		  (add-hook 'desktop-after-read-hook (lambda () (set-window-configuration wc t)))
		  (perject-desktop-load name)
		  (remove-hook 'desktop-after-read-hook (lambda () (set-window-configuration wc t))))
		(dolist (hook perject-auto-add-hooks)
		  (add-hook hook #'perject--auto-add-buffer))
		(when (and perject--desktop-restored-frames
				   (memq 'open perject-switch-to-new-frame))
		  (select-frame-set-input-focus (car perject--desktop-restored-frames)))
		(run-hook-with-args 'perject-after-open-hook name))
	(run-hook-with-args 'perject-before-create-hook name)
	(make-directory (perject--get-project-directory name) t)
	;; Create a desktop file.
	(perject-save-project name)
	(setq perject-projects (cons (list name) perject-projects))
	(perject-create-new-frame name (not (memq 'create perject-switch-to-new-frame)))
	(run-hook-with-args 'perject-after-create-hook name)))


(defun perject-open-project-in-new-instance (name &optional anonymous)
  "Open a new Emacs instance for the project named NAME.
This means that a new Emacs process is created and in it, the buffers and frames
of the corresponding desktop file are restored. If ANONYMOUS is non-nil, also
load the anonymous project in that instance.
In interactive use, the user is asked for the project name and the anonymous
project is loaded if and only if a prefix argument is supplied."
  (interactive
   (list (perject--get-project-name "Open project in new instance: " 'inactive)
		 current-prefix-arg))
  (let ((parameter
		 (concat
          perject-command-line-option
          " \""
          (if anonymous (concat name "," "anonymous") name)
          "\"")))
    ;; Start the new Emacs instance, see
    ;; https://emacs.stackexchange.com/questions/5428/restart-emacs-from-within-emacs.
    (call-process "sh" nil nil nil "-c" (concat "emacs " parameter " &"))))


(defun perject-close-project (name &optional kill-buffers keep-frames)
  "Close the project named NAME.
This removes the project from the list of active projects and closes its frames.
In interactive use, the user is asked for NAME.
The optional argument KILL-BUFFERS determines what happens to the buffers
belonging to the project. If it is 'nil, don't kill any buffers. If it is 'some,
kill those buffers which do not belong to any other project but the selected
one and if its value is t, kill all buffers associated with the project.
In interactive use, the prefix argument determines KILL-BUFFERS: No prefix
argument corresponds to 'some, a single prefix argument corresponds to t and
anything else corresponds to nil.
Depending on the value of the variable `perject-close-project-save', the project
is saved to a desktop file. If the variable `perject-close-project-confirmation'
is non-nil, the user is asked for confirmation before the project is closed.
Furthermore, the variable `perject-kill-buffers-confirmation' decides if the
user is asked for confirmation an additional time before any buffers are killed.
An error is thrown if there is no project to close or all the open frames belong
to the selected project."
  (interactive
   (list
    (perject--get-project-name
     "Close project: " 'active nil t (perject--current-project)
     "There currently is no project to close"
	 "No project specified")
    (pcase current-prefix-arg
	  ('nil 'some)
	  (`(4) t)
	  (_ nil))))
  (when (or (not perject-close-project-confirmation)
            (y-or-n-p (format "Closing project '%s'. Are you sure?" name)))
    (when (and
		   (eq (length (perject--get-frames name)) (length (frame-list)))
		   (not keep-frames))
      (user-error "Can't close a project which belongs to all open frames"))
    (run-hook-with-args 'perject-before-close-hook name)
    (if (or (eq perject-close-project-save t)
            (and (eq perject-close-project-save 'ask)
                 (y-or-n-p (format "Save the project '%s'? " name)))
			(and (functionp perject-close-project-save) (funcall perject-close-project-save name)))
		(perject-save-project name t t)
	  ;; If we don't save, we need to manually remove the lock file.
	  (desktop-release-lock (file-name-as-directory (perject--get-project-directory name))))
    (let ((buffers (perject--get-buffers name)))
      (setq perject-projects (assoc-delete-all name perject-projects))
      (unless keep-frames
		(dolist (frame (perject--get-frames name))
          (delete-frame frame)))
	  (setq buffers
			(cond
			 ((and (eq kill-buffers 'some)
				   (or (not (eq perject-kill-buffers-confirmation t))
					   (y-or-n-p (format "Kill buffers only belonging to project '%s'?" name))))
			  ;; Determine the buffers which are not associated with any project
              ;; but the selected one.
			  (cl-intersection buffers (perject--get-anonymous-buffers)))
			 ((and (eq kill-buffers t)
				   (or (not perject-kill-buffers-confirmation)
					   (y-or-n-p (format "Kill all buffers belonging to project '%s'?" name))))
			  buffers)
			 (t nil)))
	  (dolist (buffer buffers)
		(kill-buffer buffer))
      (run-hook-with-args 'perject-after-close-hook name))))

(defun perject-reload-project (name &optional keep-buffers)
  "Reload the project named NAME from its desktop file.
This discards any changes to the project and reverts it to the state from the
previous save. It also kills the buffers that belong only to that project and to
no other one. If KEEP-BUFFERS is non nil, no buffers are killed (but new ones
may be loaded).
The variable `perject-switch-to-new-frame' decides if we switch to one of the
created frames.
In interactive use, the user is asked to select NAME and KEEP-BUFFERS
corresponds to the prefix argument."
  (interactive
   (list
	(perject--get-project-name "Reload project: " 'active nil t (perject--current-project)
							   "There is no active project to reload"
							   "No project specified")
	current-prefix-arg))
  (when (or (not perject-reload-project-confirmation)
			(y-or-n-p (format "Reload project '%s'?" name)))
	(run-hook-with-args 'perject-before-reload-hook name)
	;; Allow reusing frames that belong to the project.
	(let ((perject--desktop-reuse-frames
		   (-compose (apply-partially #'string-equal name) #'perject--current-project))
		  (frames (perject--get-frames name))
		  (perject-switch-to-new-frame)
		  perject-close-project-save
		  perject-close-project-confirmation perject-kill-buffers-confirmation
		  perject-before-close-hook perject-after-close-hook
		  perject-before-open-hook perject-after-open-hook)
	  (perject-close-project name (if keep-buffers nil 'some) t)
	  (perject-open-project name)
	  ;; Cleanup the frames that were not reused.
	  (dolist (frame (cl-set-difference frames perject--desktop-restored-frames))
		(delete-frame frame)))
	(run-hook-with-args 'perject-after-reload-hook name)))


;;;; Managing Frames

(defun perject-add-frame-to-project (name &optional frame msg)
  "Switch the project of the frame FRAME to the project named NAME.
If FRAME is nil, it defaults to the current frame. This is also the case in
interactive use, where the user is asked for NAME. If MSG is non-nil, also print
a message. In interactive use, this is controlled by the variable
`perject-frame-to-project-message'.
Note that a frame can just belong to a single project, so if the frame was
previously assigned to another project, it changes ownership."
  (interactive
   (list
    (perject--get-project-name "Add current frame to project: " 'active
							   (-compose #'not (apply-partially #'perject--is-assoc-with (selected-frame)))
							   t nil "No project to add frame to" "No project specified")
    (selected-frame)
	perject-frame-to-project-message))
  (perject--set-current-project name frame)
  (when msg
	(message "Added %sframe to project '%s'."
			 (if (or (not name) (equal frame (selected-frame)))
				 "current "
			   "")
			 name)))

(defun perject-remove-frame-from-project (&optional frame msg)
  "Remove the current project from the frame FRAME.
This means that afterwards, the frame is no longer associated with any project.
If FRAME was not associated with any project to begin with, do nothing. If FRAME
is nil, it defaults to the current frame, which also happens in interactive use.
If MSG is non-nil, also print a message. In interactive use, this is controlled
by the variable `perject-frame-to-project-message'."
  (interactive (list (selected-frame) perject-frame-to-project-message))
  (let ((name (perject--current-project frame)))
	(when name
	  (perject--set-current-project nil frame)
	  (when msg
		(message "Removed %sframe from project '%s'."
				 (if (or (not name) (equal frame (selected-frame)))
					 "current "
				   "")
				 name)))))

(defun perject-create-new-frame (name &optional no-select)
  "Create a new frame for the project named NAME and select it, unless NO-SELECT is non-nil.
In interactive use, NAME defaults to the current project and the newly created
frame is always selected. If the current frames is not associated with any
project or if a prefix argument is supplied, ask the user for NAME."
  (interactive
   (list
    (if (or current-prefix-arg
            (not (perject--current-project)))
        (perject--get-project-name "Create new frame for project: " 'active nil t
								   (perject--current-project)
								   "No project to create a frame for."
								   "No project specified")
      (perject--current-project))))
  (let ((frame (make-frame (list (cons 'perject-project name)))))
    (unless no-select (select-frame-set-input-focus frame))))


;;;; Managing Buffers

(defun perject-add-buffer-to-project (buffer name &optional msg)
  "Add the buffer BUFFER to the project named NAME.
NAME may be a string or nil, in the latter case, the user is asked for the
project. If no project named NAME exists, create one.
If MSG is non-nil, also display a message upon completion.
In interactive use, the current buffer is added to the current project. If a
prefix argument was supplied or if the current frame is not associated with any
project, the user is asked to choose the project. A message is printed if
`perject-buffer-to-project-message' is non-nil.
If the buffer is already associated with the project, an error is thrown. This
function does not check whether BUFFER is still live or has already been killed,
so caller functions should make sure that the buffer in question has not been
killed."
  (interactive
   (list
	(current-buffer)
	(and (not current-prefix-arg) (perject--current-project))
	perject-buffer-to-project-message))
  (let* ((name (or
                name
                (perject--get-project-name
				 "Add buffer to project: " 'active
				 (-compose 'not (apply-partially #'perject--is-assoc-with buffer))
				 t nil
                 "There is no active project or all projects are already associated with the current buffer"
				 "No project specified")))
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
    (when msg
      (message "Added buffer '%s' to project '%s'." (buffer-name buffer) name))))


(defun perject--auto-add-buffer (&optional ignore)
  "Silently add the current buffer to projects as specified by `perject-auto-add-function'.
If IGNORE is non-nil, simply add the current buffer to the current project;
i.e. act as if `perject-auto-add-function' was nil.
Does nothing to projects that are already associated with the buffer."
  (let ((buffer (current-buffer)))
	(dolist (project (if (or ignore (not perject-auto-add-function))
						 (perject-current-project)
					   (funcall perject-auto-add-function buffer (perject--current-project))))
	  (when (and project
				 (not (perject--is-assoc-with buffer project)))
		(perject-add-buffer-to-project buffer project)))))


(defun perject-remove-buffer-from-project (buffer name &optional msg)
  "Remove the buffer BUFFER from the project named NAME.
NAME may be a string or nil, in the latter case, the user is asked for the
project. If no such project exists or the buffer is not associated with that
project, throw an error.
If MSG is non-nil, also display a message upon completion.
In interactive use, the current buffer is removed from the current project. If a
prefix argument was supplied or if the current frame is not associated with any
project, the user is asked to choose the project. A message is printed if
`perject-buffer-to-project-message' is non-nil.
The variable `perject-empty-project-delete' determines what happens if the last
buffer of a project is removed."
  (interactive
   (list
	(current-buffer)
	(and (not current-prefix-arg) (perject--current-project))
	perject-buffer-to-project-message))
  (let* ((name (or
                name
                (perject--get-project-name
                 "Remove buffer from project: " 'active
				 (apply-partially 'perject--is-assoc-with buffer)
				 t nil
                 "The buffer is currently not associated with any project"
				 "No project specified")))
         (project (assoc name perject-projects))
         (projects (cdr project)))
    (unless project
      (error "There is no project named '%s'" name))
    (unless (member buffer projects)
      (user-error "Buffer '%s' is not associated with project '%s'"
                  (buffer-name buffer) name))
    (setcdr project (delete buffer projects))
    (when msg
      (message "Removed buffer '%s' from project '%s'." (buffer-name buffer) name))
    (when
        (and
		 (not (cdr project))
         (or (eq perject-empty-project-delete t)
             (and (eq perject-empty-project-delete 'ask)
                  (y-or-n-p
				   (format
					"Project '%s' is not associated with any buffers anymore. Delete it?"
					name)))
			 (and (functionp perject-empty-project-delete (funcall perject-empty-project-delete name)))))
      (let ((perject-delete-project-confirmation nil))
        (perject-delete-project name)))))

(defun perject-remove-buffer-from-project-and-kill (buffer name &optional msg)
  "Like `perject-remove-buffer-from-project', but also kills the buffer.
See its documentation."
  (interactive
   (list
	(current-buffer)
	(and (not current-prefix-arg) (perject--current-project))
	perject-buffer-to-project-message))
  (perject-remove-buffer-from-project buffer name msg)
  (kill-buffer buffer))

(defun perject-remove-buffer-from-all-projects (&optional msg)
  "Remove the current buffer from all active projects.
If MSG is non-nil, also display a message upon completion,
For example, this is called by perject before a buffer is killed using
`kill-buffer-hook'."
  (interactive)
  (dolist (name (perject--list-projects 'active))
    (when (perject--is-assoc-with (current-buffer) name)
      (perject-remove-buffer-from-project (current-buffer) name)))
  (when msg
    (message "Removed buffer from all projects.")))


;;;; Managing Projects

(defun perject-rename-project (old-name new-name)
  "Rename the project named OLD-NAME to NEW-NAME.
In interactive use, the user is asked for both project names."
  (interactive
   (list
    (perject--get-project-name
     "Select project to rename: " 'active nil t (perject--current-project)
     "There currently is no project to rename"
	 "No project specified")
    (perject--get-new-project-name "New name: ")))
  (perject--set-current-project new-name)
  (rename-file (perject--get-project-directory old-name)
			   (perject--get-project-directory new-name)))

(defun perject-delete-project (name &optional kill-buffers)
  "Delete the project named NAME.
This includes closing the project and deleting the corresponding desktop file.
In interactive use, the user is asked for NAME.
The optional argument KILL-BUFFERS behaves like in `perject-close-project'.
If the variable `perject-delete-project-confirmation' is non-nil, the user is
asked for confirmation before the project is deleted. Furthermore, the variable
`perject-kill-buffers-confirmation' decides if the user is asked for
confirmation an additional time before any buffers are killed.
An error is thrown if there is no project to delete or all the open frames
belong to the selected project."
  (interactive
   (list
    (perject--get-project-name
     "Delete project: " nil nil t (perject--current-project)
     "There currently is no project to delete")
    (pcase current-prefix-arg
	  ('nil 'some)
	  (`(4) t)
	  (_ nil))))
  (when (or (not perject-delete-project-confirmation)
            (y-or-n-p (format "Deleting project '%s'. Are you sure?" name)))
    ;; If the project is active, close it.
    (when (perject--is-active-project name)
      (let ((perject-close-project-save nil)
            (perject-close-project-confirmation nil))
        (perject-close-project name kill-buffers)))
    (when (file-exists-p (perject--get-project-directory name))
      (delete-directory (perject--get-project-directory name) t))))


(defun perject-save-project (name &optional release-lock no-msg)
  "Save the project named NAME.
In interactive use, if a prefix argument is supplied or the current frame is not
associated with any project, ask the user for NAME, presenting the currently
active projects as candidates. Otherwise, use the current project.
If NAME is nil or the empty string (in interactive use, if the user exits with
the empty string), save the anonymous project (the buffers not belonging to any
project).
If the optional argument RELEASE-LOCK is non-nil, Emacs will release the lock of
the corresponding desktop file. This option is always nil in interactive use. If
the optional argument NO-MSG is non-nil, don't print any messages."
  (interactive
   (list
    (if (or current-prefix-arg
            (not (perject--current-project)))
		;; Default value must be nil for the empty string to appear.
        (perject--get-project-name "Save project: " 'active nil t nil
								   "No project to save."
								   #'ignore)
      (perject--current-project))))
  (perject-desktop-save name release-lock no-msg))

(defun perject-save-projects (names &optional release-lock no-msg)
  "Save the list of projects NAMES.
Each entry in NAMES should be an active project name. As a special case, a value
of nil corresponds to the anonymous project. If NAMES is 'all, save all active
projects excluding the anonymous one. If it is 'exit, save the active projects
in accordance with `perject-save-on-exit'.
In interactive use, no prefix argument corresponds to 'all, one prefix argument
corresponds to 'exit and any other prefix argument allows the user to manually
select the list of projects to save using `completing-read-multiple'.
If the optional argument RELEASE-LOCK is non-nil, Emacs will release the lock of
the corresponding desktop file. This option is always nil in interactive use. If
the optional argument NO-MSG is non-nil, don't print any messages."
  (interactive
   (list
	(pcase current-prefix-arg
	  ('nil 'all)
	  ('(4) 'exit)
	  (_ (completing-read-multiple
		  "Save projects: " (perject--list-projects 'active) nil t)))))
  (let* ((active-projects (perject--list-projects 'active))
		 (active-projects-anonymous (append active-projects (list nil)))
		 (projects-to-save
		  (pcase names
			((pred listp) names)
			('all active-projects)
			('exit (pcase perject-save-on-exit
					 ('nil nil)
					 ('all active-projects)
					 ('all-and-anonymous active-projects-anonymous)
					 ((pred listp) (if (eq (car perject-save-on-exit) t)
									   (cl-set-difference active-projects-anonymous
														  (cdr perject-save-on-exit)
														  :test #'string-equal)
									 perject-save-on-exit))
					 ('previous perject--previous-projects)
					 ((pred functionp) (funcall perject-save-on-exit
												perject--previous-projects
												active-projects)))))))
    (dolist (name projects-to-save)
	  (perject-desktop-save name release-lock (not (eq (length projects-to-save) 1))))
	(when (and projects-to-save (not no-msg))
	  (let ((strings (mapcar (lambda (str) (or str "anonymous project")) projects-to-save)))
		(message "Perject: Saved projects: %s" (string-join strings ", "))))))

(defun perject-sort-projects ()
  "Let the user sort the currently active projects.
This has no effect on the current Emacs session and only changes the order in
which the projects are loaded the next time Emacs is started if
`perject-load-at-startup' is 'previous."
  (interactive)
  (let ((new-order
		 (completing-read-multiple "Sort projects [select the most recent (last to load) projects first]: "
								   (perject--list-projects 'active))))
	(unless (= (length new-order) (length (perject--list-projects 'active)))
	  (user-error "You must order all the projects not just a subset"))
	(sort perject-projects (lambda (x y)
							 (< (or (cl-position (car x) new-order :test #'string-equal) 0)
								(or (cl-position (car y) new-order :test #'string-equal) 0))))
	(message "Sorted projects [from more recent (left) to less recent (right)]: %s."
			 (string-join (perject--list-projects 'active) ", "))))

(defun perject-print-buffer-projects (&optional buffer)
  "Print the names of the projects with which the buffer BUFFER is associated.
If nil, BUFFER defaults to the current buffer, which is also its value in
interactive use."
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
		(buffer-name (buffer-name buffer)))
	(pcase (perject--get-buffer-projects buffer)
	  ('nil
	   (message "The buffer '%s' is not associated with any projects." buffer-name))
	  (`(,project)
	   (message "The buffer '%s' is associated with the project '%s'." buffer-name project))
	  (projects
	   (message "The buffer '%s' is associated with the projects: %s."
				buffer-name (string-join projects ", "))))))

;;;; Internal Interface

(defun perject--current-project (&optional frame)
  "Return the project currently associated with the frame FRAME.
If FRAME is nil, use the current frame."
  (frame-parameter frame 'perject-project))

(defun perject--set-current-project (&optional name frame)
  "Set the project of the frame FRAME to the project named NAME.
If NAME is nil, the frame will be no longer belong to a project."
  (set-frame-parameter frame 'perject-project name))

(defun perject--get-buffers (name)
  "Return the list of buffers associated with the project named NAME."
  (alist-get name perject-projects nil nil #'equal))

(defun perject--is-assoc-with (obj name)
  "Return a non-nil value if object OBJ is associated with the project named NAME.
Otherwise, nil is returned. OBJ may be a buffer or a frame.
As a special case, if NAME is nil and OBJ is a buffer, the function always
returns t. This is because a frame without a project is represented with a nil
value in the corresponding variable, whereas a buffer without a project simply
has no associated entry in `perject-projects'."
  (cond ((bufferp obj) (if name (member obj (perject--get-buffers name)) t))
        ((framep obj) (equal (perject--current-project obj) name))
        (t (error "perject--is-assoc-with: object is not a buffer or a frame."))))

(defun perject--is-project (name)
  "Return a non-nil value if there exists a project called NAME."
  (member name (perject--list-projects)))

(defun perject--is-active-project (name)
  "Return a non-nil value if there is a active project called NAME.
Otherwise, nil is returned."
  (member name (perject--list-projects 'active)))

(defun perject--get-project-directory (name)
  "Return the directory belonging to a (possibly non-existent) project with name NAME."
  (expand-file-name (concat perject-directory name)))

(defun perject--get-frames (name)
  "Return the currently open frames which belong to the project named NAME."
  (cl-remove-if-not (lambda (frame) (perject--is-assoc-with frame name)) (frame-list)))

(defun perject--get-buffer-projects (buffer)
  "Return the list of project names with which the buffer BUFFER is associated."
  (mapcar #'car (cl-remove-if-not (-compose (apply-partially #'member buffer) #'cdr) perject-projects)))

(defun perject--is-buffer-anonymous (buffer)
  "Return non-nil if buffer BUFFER is anonymous.
This means that it does not belong to any active project."
  (not (cl-some (-compose (apply-partially #'member buffer) #'cdr) perject-projects)))

(defun perject--get-anonymous-buffers ()
  "Return the list of all buffers not associated with any project."
  (let ((buffers (buffer-list)))
    (dolist (project perject-projects)
      (dolist (buffer (cdr project))
        (setq buffers (delete buffer buffers))))
    buffers))


(defun perject--get-project-name
	(prompt &optional projects predicate require-match def no-candidate empty-string)
  "Ask the user for a project name using `completing-read' and return it.
The collection to choose from is PROJECTS or the list of all projects if
PROJECTS is nil. As a special value, PROJECTS may be equal to 'active or
'inactive, in which case the collection is restricted to the active or inactive
projects.
PROMPT, PREDICATE, REQUIRE-MATCH and DEF have the same meaning as for
`completing-read'.
In some cases, it might be desirable to check if DEF passes PREDICATE.
NO-CANDIDATE and EMPTY-STRING may be a function, a string or nil. They decide
which action is taken in case there is no candidate or in case the user enters
the empty string, respectively. If it is a function, it is called. For
NO-CANDIDATE, its return value is used as the list of projects. For
EMPTY-STRING, its return value acts as if it was entered by the user instead of
the empty string. In the string case, an error is thrown with that string
serving as the error message. If nil, no special action is taken. Note that
EMPTY-STRING only takes effect when DEF is nil, because otherwise the empty
string is interpreted to refer to the default value."
  (let* ((projects
		  (pcase projects
			((pred not) (perject--list-projects))
			('active (perject--list-projects 'active))
			('inactive (perject--list-projects 'inactive))
			(_ projects)))
		 (collection
		  (or
		   (if (functionp predicate)
			   (cl-remove-if-not predicate projects)
			 projects)
		   (pcase no-candidate
			 ((pred functionp) (funcall no-candidate))
			 ((pred stringp) (user-error no-candidate)))))
		 (completion-extra-properties
		  `(:annotation-function
			,(lambda (str)
			   (concat
				(propertize " " 'display '(space :align-to (- center 30)))
				(propertize
				 (or
				  (and (equal (perject--current-project) str) " current frame")
				  (and (perject--is-active-project str) " other frame")
				  (format-time-string "%Y %b %d %a" (file-attribute-access-time
													 (file-attributes
													  (desktop-full-file-name
													   (perject--get-project-directory str)))))
				  "")
				 'face 'perject-project-annotator-main)
				(when (perject--is-active-project str)
				  (concat
				   (propertize " " 'display '(space :align-to center))
				   (propertize
					(concat (int-to-string (length (perject--get-buffers str)))
							" buffer"
							(and (not (eq (length (perject--get-buffers str)) 1)) "s"))
					'face 'perject-project-annotator-buffers)
				   (propertize " " 'display '(space :align-to (+ center 30)))
				   (propertize
					(concat (int-to-string (length (perject--get-frames str)))
							" frame"
							(and (not (eq (length (perject--get-frames str)) 1)) "s"))
					'face 'perject-project-annotator-frames)))))))
         (name
		  (completing-read
		   prompt collection nil require-match nil 'perject-project-name-history def)))
    (if (and (string-empty-p name) empty-string)
        (if (functionp empty-string)
			(funcall empty-string)
		  (user-error empty-string))
	  name)))


(defun perject--get-new-project-name (prompt)
  "Ask the user for a project name using `read-string' and return it.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
The string entered must be a valid nonexistent project name; i.e. it may only
contain letters, digits and any characters specified by
`perject-valid-naming-chars'. Moreover, there may not already exist a project of
the same name."
  (let ((name (read-string prompt)))
    (mapc
	 (lambda (char)
       (or (and (>= char 48) (<= char 57)) ;; digit
           (and (>= char 65) (<= char 90)) ;; uppercase letter
           (and (>= char 97) (<= char 122)) ;; lowercase letter
           (member char perject-valid-naming-chars)
           (user-error
			"The character '%c' is not valid for naming a project. See the variable `perject-valid-naming-chars'."
			char))))
	(when (string-empty-p name)
      (user-error "The name of a project can not be empty."))
    (when (member name (perject--list-projects))
      (user-error "There already is a project named '%s'." name))
    name))


(defun perject--list-projects (&optional scope)
  "Return a list of the names of all projects.
Each project is represented as a directory in `perject-directory'.
If SCOPE is 'active, only return the active projects; i.e. those which are
currently loaded. If SCOPE is 'inactive, return all projects that are not active
at the moment."
  (if (eq scope 'active)
	  (mapcar 'car perject-projects)
	(let ((projects
		   (and
			(file-exists-p perject-directory)
			(remove ".."
					(remove "."
							(mapcar 'car
									(cl-remove-if-not
									 (lambda (elem)
									   (eq (cadr elem) t))
									 (directory-files-and-attributes perject-directory))))))))
	  (if (eq scope 'inactive)
		  (cl-set-difference projects (perject--list-projects 'active) :test #'string-equal)
		projects))))


;;;; Interface to desktop.el

(defun perject-desktop-load (name &optional no-msg)
  "Using `desktop-read' load the project named NAME from the corresponding desktop file.
If NAME is nil, the desktop file representing the buffers which were not
associated with any project is loaded.
If the optional argument NO-MSG is non-nil, don't print any messages.
This function also adds NAME to the alist of active projects `perject-projects'."
  (let ((perject--desktop-current-project name)
		(previous-frames (frame-list))
		(desktop-load-locked-desktop t)
		(no-msg no-msg)
		;; Suppress messages from `desktop-read'.
		(inhibit-message t)
		(message-log-max nil))
	;; Temporarily set the variable to NO-MSG, so we have the information
	;; available in `perject-desktop-restore-frameset-advice'.
	(setq perject--desktop-restored-frames no-msg)
	;; Unlike `desktop-save', `desktop-load' seems to have a weird
	;; implementation, so we need the following line since otherwise desktop
	;; thinks that it is loading the already loaded desktop again and refuses to
	;; do so.
	(setq desktop-dirname (file-name-as-directory (perject--get-project-directory name)))
	(desktop-read desktop-dirname))
  (run-hook-with-args 'perject-desktop-after-load-hook name)
  (when name
	;; If no buffers were restored, the project is not active yet and needs to manually be added.
	(unless (perject--is-active-project name)
	  (setq perject-projects (cons (list name) perject-projects)))
	;; Assign the newly created frames to their projects.
	(dolist (frame perject--desktop-restored-frames)
	  (perject--set-current-project name frame))))

(defun perject-desktop-save (name &optional release-lock no-msg)
  "Using `desktop-save' save the project named NAME to the corresponding desktop file.
If NAME is nil, the desktop file representing the buffers which were not
associated with any project, is saved.
If the optional argument RELEASE-LOCK is non-nil, Emacs will release the lock of
the corresponding desktop file. If the optional argument NO-MSG is non-nil,
don't print any messages."
  ;; Frames that do not belong to the project named NAME should not be
  ;; saved, so we put them into a list and save the other frames.
  (let ((ignored-frames
		 (cl-remove-if
		  (lambda (frame)
			(or (perject--is-assoc-with frame name)
				(frame-parameter frame 'desktop-dont-save)))
		  (frame-list)))
		;; Only save those buffers belonging to the current project and respect
		;; the value of `perject-buffers-not-to-save-function'.
		(desktop-buffers-not-to-save-function
		 (cond
		  ((and name perject-buffers-not-to-save-function)
		   (lambda (file-name buffer-name major-mode minor-modes)
			 (and (perject--is-assoc-with (get-buffer buffer-name) name)
				  (funcall perject-buffers-not-to-save-function
						   name file-name buffer-name major-mode minor-modes))))
		  (name
		   (lambda (_ buffer-name _ _)
			 (perject--is-assoc-with (get-buffer buffer-name) name)))
		  (perject-buffers-not-to-save-function
		   (lambda (file-name buffer-name major-mode minor-modes)
			 (and (perject--is-buffer-anonymous (get-buffer buffer-name))
				  (funcall perject-buffers-not-to-save-function
						   name file-name buffer-name major-mode minor-modes))))
		  (t
		   (lambda (_ buffer-name _ _)
			 (perject--is-buffer-anonymous (get-buffer buffer-name))))))
		;; Hack: Pretend the desktop file is from the same time, so that desktop does not
		;; complain that the desktop file is more recent than the one loaded.
		(desktop-file-modtime (file-attribute-modification-time
							   (file-attributes
								(desktop-full-file-name
								 (file-name-as-directory (perject--get-project-directory name)))))))
	(run-hook-with-args 'perject-desktop-save-hook name)
	(dolist (frame ignored-frames)
	  (set-frame-parameter frame 'desktop-dont-save t))
	(desktop-save (file-name-as-directory
				   (perject--get-project-directory name))
				  release-lock)
	(dolist (frame ignored-frames)
	  (set-frame-parameter frame 'desktop-dont-save nil))
	(unless no-msg
	  (message "Perject: Saved %s"
			   (if name (format "project %s." name) "anonymous project.")))))


(defun perject-desktop-restore-frameset-advice ()
  "Like `desktop-restore-frameset', but allow for more parameters from `frameset-restore'.
Namely, the variables `perject--desktop-reuse-frames' and
`perject--desktop-cleanup-frames' may be nil or a function as described at
`frameset-restore'.
This function also sets `perject--desktop-restored-frames'."
  (when (desktop-restoring-frameset-p)
	(let ((no-msg perject--desktop-restored-frames))
	  ;; Reset `perject--desktop-restored-frames'.
	  (setq perject--desktop-restored-frames nil)
      (frameset-restore
	   desktop-saved-frameset
	   :reuse-frames perject--desktop-reuse-frames
	   ;; Use the cleanup to set the list of restored frames.
	   :cleanup-frames (lambda (frame action)
						 (when (memq action '(:reused :created))
                           (push frame perject--desktop-restored-frames))
						 (when (functionp perject--desktop-cleanup-frames)
						   (funcall cleanup-frames frame action)))
	   :force-display desktop-restore-in-current-display
	   :force-onscreen desktop-restore-forces-onscreen)
	  ;; Print information.
	  ;; Code adapted from `desktop-read'.
	  ;; We access the variables from `desktop-read'.
	  ;; Since we suppress messages when calling `desktop-read',
	  ;; we need to make sure they are printed here.
	  (unless no-msg
		(let ((inhibit-message nil)
			  (message-log-max t))
		  (message "Perject:%s %s%d buffer%s restored%s%s."
				   (if perject--desktop-current-project
					   (concat " project '" perject--desktop-current-project "':")
					 " anonymous project:")
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
					 "")))))))

(defun perject-desktop-create-buffer-advice (buffer)
  "Add buffer BUFFER to the project specified by `perject--desktop-current-project'.
Then run the hook `perject-after-desktop-create-buffer-hook'. If BUFFER is nil,
only that hook is run. Returns BUFFER."
  (when buffer
    (when perject--desktop-current-project
	  ;; The buffer restored correctly and there is a current project.
	  (perject-add-buffer-to-project buffer perject--desktop-current-project))
    (run-hook-with-args 'perject-after-desktop-create-buffer-hook buffer)
    buffer))

(provide 'perject)
;;; perject.el ends here
