;;; perject.el - Session-persistent project management -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: overideal
;; Maintainer: overideal
;; Version: 2.0
;; Package-Requires: ((emacs "27.1") (dash "2.12"))
;; Homepage: https://gitlab.com/overideal/perject

;;; Commentary:

;; This package allows the user to manage multiple projects in a single Emacs instance.
;; Each Emacs frame is associated with a collection, which constitute a set of
;; projects and a list of frames.
;; Each project consists of buffers and window configurations.
;; By leveraging the built-in package `desktop.el', the collections (and thus
;; the projects) are automatically saved and restored upon restarting Emacs.


;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'dash)
(require 'transient)
(require 'desktop)


;;;; Constants

(defconst perject-command-line-option "--perject"
  "Command line option which if present overwrites `perject-load-at-startup'.
For example, if Emacs is launched with '--perject \"project 1,project 2\"', the
projects \"project 1\" and \"project 2\" are loaded.
Write '--perject \"\"' if no projects should be opened.")


;;;; Customization

(defgroup perject nil
  "Session-persistent project management"
  :group 'convenience
  :prefix "perject-")

(defface perject-mode-line-face '((t :foreground "dark orange"))
  "The face used by the mode line indicator of perject.")

(defface perject-project-annotator-main '((t :inherit font-lock-keyword-face))
  "The face used for displaying the main annotation when selecting a project.")

(defface perject-project-annotator-buffers '((t :inherit completions-annotations))
  "The face used for displaying the number of buffers when selecting a collection or project.")

(defface perject-project-annotator-frames '((t :inherit completions-annotations))
  "The face used for displaying the number of frames when selecting a collection or project.")

(defcustom perject-directory (concat user-emacs-directory "perject/")
  "The directory used by perject to save its data (in particular the desktop files)."
  :type 'directory)

(defface perject-sort-collections-current '((t :inherit font-lock-keyword-face))
  "The face used for displaying the name of the current collection when ordering collections.
This influences the command `perject-sort-collections'.")

(defface perject-sort-collections-other '((t :inherit font-lock-comment-face))
  "The face used for displaying the name of the non-current collection when ordering collections.
This influences the command `perject-sort-collections'.")

(defface perject-sort-projects-current '((t :inherit font-lock-keyword-face))
  "The face used for displaying the name of the current project when ordering projects.
This influences the command `perject-sort-projects'.")

(defface perject-sort-projects-other '((t :inherit font-lock-comment-face))
  "The face used for displaying the name of the non-current project when ordering projects.
This influences the command `perject-sort-projects'.")


(defcustom perject-load-at-startup nil
  "The variable controls which collections are automatically loaded at startup.

It may have one of the following values:
- nil: Load no collection.
- all: Load all collections.
- A list of collection names: Load the specified collections (if existent). If a
  name in the list does not correspond to an existing project, do nothing. As a
  special case, if the first element of the list is 'not, all collections but
  the ones specified in the list are loaded.
- 'previous: Load the collections that were open at the end of the previous
  session.
- A function: The function is called with two arguments, namely the list of
  collections opened in the previous Emacs session and the list of all
  collections. The function should return a list of collection names to be
  loaded. It could for example ask the user.

The order in which the collections are loaded is determined as follows: In the list
or function case, the collections are loaded \"from left to right\".
If the value is 'previous, use the current order of the collections, which by default
goes from \"older\" to \"newer\". Using the command `perject-sort-collections',
they can be manually sorted, which will be remembered over restarts of Emacs."
  :type '(choice
		  (const :tag "Load no collection" nil)
		  (const :tag "Load all collections" all)
		  (repeat :tag "Load the specified collections (if existent)"
				  (choice string (const not)))
		  (const :tag "Load previously open collections" previous)
		  (function :tag "Custom function")))

(defcustom perject-save-on-exit 'all
  "The variable controls which collections are automatically saved when exiting Emacs.

It may have one of the following values:
- nil: Don't save any collections.
- 'all: Save all active collections.
- 'recent: Save all collections that were open in the previous Emacs session.
- A list of collection names: Save the specified collection (if active). As a
  special case, if the first element of the list is 'not, all active collections
  but the ones specified in the list are saved upon exiting Emacs.
- A function: The function is called with two arguments, namely the list of
  collections opened in the previous Emacs session and the list of active
  collections. The function should return the list of collection names to be
  saved. It could for example ask the user."
  :type '(choice
		  (const :tag "Don't save any projects" nil)
		  (const :tag "Save all active collections" all)
		  (repeat :tag "Save the specified collections (if existent)"
				  (choice string (const not)))
		  (function :tag "Custom function")))

(defcustom perject-buffers-not-to-save-function nil
  "Function identifying buffers that are to be excluded when saving a collection to its desktop file.
Buffers not belonging to the current collection are never saved.
If this variable is nil, no additional filtering takes place.
The function is called with five arguments: the dotted pair consisting of the
current collection name as a car and the current project name as a cdr (the
latter one could be nil), the name of the visited file, the buffer name, the
major mode and the list of active minor modes. It should return nil if the
buffer should not have its state saved in the desktop file.
This variable corresponds to `desktop-buffers-not-to-save-function'."
  :type '(choice function (const nil)))

(defcustom perject-auto-add-function nil
  "A function used to control which buffers are automatically associated with projects.
When a hook in `perject-auto-add-hooks' runs, this function is called in order
to decide to which projects the current buffer should be added to.
It is called with two arguments. The first argument is the current buffer. The
second is a cons cell with car a collection name and cdr a project name. The
project name may also be nil.
The function should return a list of collection project pairs to which the
buffer should be added. By returning nil (the empty list) the buffer is not
added to any project.
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
`buffer-list-update-hook', `change-major-mode-hook' and many mode hooks.
Modifcations of this variable only take effect after (re)enabling
`perject-mode'.
Internally, the function `perject--auto-add-buffer' is used."
  :type '(list variable))

(defcustom perject-project-format "%s|%s"
  "How to print projects and their respective collections in text.
This can be a format string (like `format' uses) with two '%s', the first of
which will be replaced by the collection name, the second by the project name.
It may also be a custom function which is called with two string arguments,
namely the collection name and the project name and should return the name to be
displayed.
Note that two projects may have the same name but be in different projects, and
the supplied function should produce different strings for them."
  :type '(choice string function))

(defcustom perject-save-on-close t
  "This variable controls if a collection is saved when closing it using `perject-close-collection'.
It may have one of the following values:
- t: Always save the collection.
- 'ask: Ask the user if the collection should be saved.
- nil: Never save the collection.
- A function: The function is called with the collection name and the list of
all its associated buffers as arguments. It should return non-nil if and only if
the collection is to be saved. Note that this function should not kill any of
those buffers."
  :type '(choice
		  (const :tag "Always save the collection" t)
		  (const :tag "Ask the user if the collection should be saved" ask)
		  (const :tag "Never save the collection" nil)
		  (function :tag "Custom function")))

(defcustom perject-switch-to-new-frame '(open create)
  "Whether to switch to a newly created frame.
The value of this variable is a list which may contain any of the following
symbols, whose presence in the list has the mentioned effect:
- 'open: Switch after opening a project using `perject-open-collection'.
- 'create: Switch after creating a new project using `perject-open-collection'.
- 'reload: Switch after reloading a project using `perject-reload-collection'.
In particular, if this variable is nil (i.e. the empty list), never switch to a
newly created frame."
  :type '(set
		  (const :tag "Switch after opening a project using `perject-open-collection'" open)
		  (const :tag "Switch after creating a new project using `perject-open-collection'" create)
		  (const :tag "Switch after reloading a project using `perject-reload-collection'" reload)))

(defcustom perject-messages
  '(add-buffer remove-buffer switch-collection next-project previous-project)
  "Whether to print informative messages when performing certain actions.
The value of this variable is a list which may contain any of the following
symbols, whose presence in the list leads to a message being printed in the
indicated command:
- 'add-buffer: `perject-add-buffer-to-project',
- 'remove-buffer: `perject-remove-buffer-from-project',
- 'switch-collection: `perject-switch-collection',
- 'switch-project: `perject-switch-project',
- 'next-project: `perject-switch-to-next-project',
- 'previous-project: `perject-switch-to-previous-project'."
  :type '(set
		  (const :tag "`perject-add-buffer-to-project'" add-buffer)
		  (const :tag "`perject-remove-buffer-from-project'" remove-buffer)
		  (const :tag "`perject-switch-collection'" switch-collection)
		  (const :tag "`perject-switch-project'" switch-project)
		  (const :tag "`perject-switch-to-next-project'" next-project)
		  (const :tag "`perject-switch-to-previous-project'." previous-project)))

(defcustom perject-confirmation '(delete delete-project)
  "Whether to ask for confirmation before performing certain actions.
The value of this varaible is a list which may contain any of the following
symbols, whose presence in the list has the mentioned effect:
- 'close: Ask before closing a collection using `perject-close-collection'.
- 'reload: Ask before reloading a collection using `perject-reload-collection'.
- 'delete: Ask before deleting a collection using `perject-delete-collection'.
- 'delete-project: Ask before deleting a project using `perject-delete-project'."
  :type '(set
		  (const :tag "Ask before closing a collection using `perject-close-collection'" close)
		  (const :tag "Ask before reloading a collection using `perject-reload-collection'" reload)
		  (const :tag "Ask before deleting a collection using `perject-delete-collection'" delete)
		  (const :tag "Ask before deleting a project using `perject-delete-project'" delete-project)))

(defcustom perject-confirmation-kill-buffers '(close delete delete-project)
  "Whether to ask for confirmation before killing buffers in certain contexts.
The value of this variable is a list which may contain any of the following
symbols, whose presence in the list has the mentioned effect:
- 'close: Ask before killing buffers in `perject-close-collection'.
- 'reload: Ask before killing buffers in `perject-reload-collection'.
- 'delete: Ask before killing buffers in `perject-delete-collection'.
- 'delete-project: Ask before killing buffers in `perject-delete-project'."
  :type '(set
		  (const :tag "Ask before killing buffers in `perject-close-collection'" close)
		  (const :tag "Ask before killing buffers in `perject-reload-collection'" reload)
		  (const :tag "Ask before killing buffers in `perject-delete-collection'" delete)
		  (const :tag "Ask before killing buffers in `perject-delete-project'" delete-project)))

(defcustom perject-kill-buffers-by-default '(close delete delete-project)
  "Whether to kill buffers by default in certain contexts.
The value of this variable is a list which may contain certain symbols, each of
which corresponds to a command as indiciated below. Its presence in the list
implies that the corresponding command will kill buffers as described in the
command's documentation. When the command is called with a prefix argument, no
buffers will be killed. In contrast, if the symbol belonging to a command is not
in the list, the roles are reversed; i.e. no buffers are killed unless the
command is called with a prefix argument.
The possible symbols (and the corresponding commands) that may appear in the
list are as follows:
- 'close: `perject-close-collection',
- 'reload: `perject-reload-collection',
- 'delete: `perject-delete-collection',
- 'delete-project: `perject-delete-project'."
  :type '(set
		  (const :tag "`perject-close-collection'" close)
		  (const :tag "`perject-reload-collection'" reload)
		  (const :tag "`perject-delete-collection'" delete)
		  (const :tag "`perject-delete-project'" delete-project)))

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

(defcustom perject-mode-line-format #'perject-mode-line-indicator
  "This variable determines the mode line indicator of perject.
It may have one of the following two values:
- nil: No mode line entry is shown for perject.
- A function: Call that function with two strings as an argument, namely the
  name of the current collection and project (both of which may be nil). The
  function should return the string to display in the mode line.
By default, the function `perject-mode-line-indicator' is used, which which
displays the name of the current collection and project with the face
`perject-mode-line-face'."
  :type '(choice
		  (const :tag "No mode line entry is shown for perject" nil)
		  (function :tag "Custom function")))

(defcustom perject-frame-title-format #'perject-frame-title
  "This variable determines the format of the title of a frame.
It may have one of the following two values:
- nil: The title of a frame is not altered by perject.
- A function: Call the function with a dotted pair as its only argument,
  whose car is a collection name and whose cdr is a project name. The collection
  name is guaranteed to be non a proper string but the project name could be
  nil. The function should return a string to be used as the frame title.
  It may also contain nil to decline naming the frame.
By default, the function `perject-frame-title' is used."
  :type '(choice
		  (const :tag "Don't alter the title of the frame" nil)
		  (function :tag "Custom function")))

(defcustom perject-reuse-starting-frame t
  "Whether perject should reuse the starting frame when loading collections at startup.
When starting Emacs, a single frame is produced. If this variable is non-nil,
that frame will be reused when the first collection that has at least one frame
is loaded. If this variable is nil, that frame is not altered."
  :type '(choice
		  (const :tag "Reuse starting frame" t)
		  (const :tag "Don't reuse starting frame" nil)))

(defcustom perject-valid-naming-chars '(?_ ?- ? )
  "A list of characters that may be used to name a project.
All letters and digits are always allowed.
Note that the name of the project has to also be a valid (possibly non-existent)
directory name, so be careful. By default, this variable allows '_', '-' and
' '. This variable is used in the functions `perject--get-new-collection-name'
and `perject--get-new-project-name'.
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
  "Hook run before perject opens a project using `perject-open-collection'.
In particular, the buffers and frames from the project have not yet been
restored.
The functions are called with one argument, namely the name of the project to be
opened."
  :type 'hook)

(defcustom perject-after-open-hook nil
  "Hook run after perject has opened a project using `perject-open-collection'.
In particular, all the buffers and frames from the project have been restored.
The functions are called with one argument, namely the name of the newly opened
project.
The variable `perject--desktop-restored-frames' is the list of newly created
frames and might be useful."
  :type 'hook)

(defcustom perject-before-create-hook nil
  "Hook run before perject creates a new project using `perject-open-collection'.
The functions are called with one argument, namely the name of the closed
project."
  :type 'hook)

(defcustom perject-after-create-hook nil
  "Hook run after perject has created a new project using `perject-open-collection'.
The functions are called with one argument, namely the name of the closed
project."
  :type 'hook)

(defcustom perject-before-close-hook nil
  "Hook run before perject closes a collection using `perject-close-collection'.
The functions are called with two arguments, namely the name of the collection
to be closed and a list containing all buffers currently associated with some
project of this collection.
Do not kill any of those buffers in this hook. Use `perject-after-close-hook'
for that purpose."
  :type 'hook)

(defcustom perject-after-close-hook nil
  "Hook run after perject has closed a collection using `perject-close-collection'.
The functions are called with two arguments, namely the name of the closed
collection and a list containing all buffers that were associated with some
project of this collection and that have not been killed."
  :type 'hook)

(defcustom perject-before-reload-hook nil
  "Hook run before perject reloads a collection using `perject-reload-collection'.
The functions are called with one argument, namely the name of the project to be
reloaded."
  :type 'hook)

(defcustom perject-after-reload-hook nil
  "Hook run after perject has reloaded a collection using `perject-reload-collection'.
The functions are called with one argument, namely the name of the reloaded
project."
  :type 'hook)

(defcustom perject-before-delete-project-hook nil
  "Hook run before perject deletes a project using `perject-delete-project'.
The functions are called with two arguments, namely the name of the project to
be deleted and a list containing all buffers that are currently associated with
this project.
Do not kill any of those buffers in this hook. Use
`perject-after-delete-project-hook' for that purpose."
  :type 'hook)

(defcustom perject-after-delete-project-hook nil
  "Hook run after perject has deleted a project using `perject-delete-project'.
The functions are called with two arguments, namely the name of the deleted
project and a list containing all buffers that were associated with some project
of this project and that have not been killed."
  :type 'hook)

(defcustom perject-desktop-after-load-hook nil
  "Hook run after perject has loaded a collection from its desktop file.
The functions are called with one argument, namely a list with car the
collection name and remaining elements the corresponding projects.
This hook corresponds to `desktop-after-read-hook'."
  :type 'hook)

(defcustom perject-desktop-save-hook nil
  "Hook run before perject has saved a collection to its desktop file.
The functions are called with one argument, namely a list with car the
collection name and remaining elements the corresponding projects.
This hook corresponds to `desktop-save-hook'."
  :type 'hook)


;;;; Internal Variables

(defvar-local perject-buffer nil
  "The list of projects to which the current buffer belongs.
Each entry is a cons cell with the car being the collection name and the cdr
being the project name.")

(defvar perject-collections nil
  "A list representing the perject collections.
Each element is a list, whose first entry is the collection name and the
remaining entries are the corresponding project names.")

(defvar perject-collection-name-history nil
  "The history of collection names.")

(defvar perject-project-name-history nil
  "The history of project names.")

(defvar perject--sort-collections-index 0
  "Index of the current collection in `perject-sort-collections'.")

(defvar perject--sort-projects-index 0
  "Index of the current project in `perject-sort-projects'.")

;; I am surprised such a variable does not already exist in `frameset.el'
(defvar perject--desktop-restored-frames nil
  "The list of the frames which were restored for the most recent project.
Should not be modified by the user.")

(defvar perject--desktop-current nil
  "Internal variable that is set to the current collection while saving or loading a project.
More precisely, it is a list with car the collection name and remaining values
its corresponding projects.
Should not be modified by the user.")

(defvar perject--desktop-reuse-frames nil
  "Internal parameter for `perject-desktop-restore-frameset-advice'.
Should not be modified by the user.")

(defvar perject--desktop-cleanup-frames nil
  "Internal parameter for `perject-desktop-restore-frameset-advice'.
Should not be modified by the user.")

(defvar perject--previous-collections nil
  "Internal variable that stores the collections that were opened in the last session.
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

		(advice-add 'desktop-restore-frameset :override #'perject-desktop-restore-frameset-advice)

        (when after-init-time
          ;; This means the mode got enabled and the init phase is already over.
          ;; I.e. later, manually by the user.
          ;; In that case, `after-init-hook' is not run.
          (dolist (hook perject-auto-add-hooks)
            (add-hook hook #'perject--auto-add-buffer)))

        ;; The mode line knows to which mode this belongs to, and if the mode is not active, the entry is not shown.
        ;; We don't add anything to the misc mode line if `perject-mode-line-format' is nil,
        ;; we already have added something to it or if `mode-line-misc-info' is nil (which should never happen).
        (unless (or (not perject-mode-line-format)
                    (assoc 'perject-mode mode-line-misc-info)
                    (not mode-line-misc-info))
          (push '(perject-mode
				  (:eval
				   (funcall perject-mode-line-format
							 (car (perject--current)) (cdr (perject--current)))))
                (cdr (last mode-line-misc-info)))))

	;; Remove the added hooks.
    (remove-hook 'after-init-hook #'perject--init)
    (remove-hook 'kill-emacs-hook #'perject--exit)
	;; Remove the advice.
	(advice-remove 'desktop-restore-frameset #'perject-desktop-restore-frameset-advice)
    (dolist (hook perject-auto-add-hooks)
      (remove-hook hook #'perject--auto-add-buffer))
	;; Reset the frame title, but keep the information about which frame belongs
	;; to which project, since that information might still be useful if the
	;; mode is enabled again.
	(when perject-frame-title-format
	  (dolist (frame (frame-list))
		(when (perject--current frame)
		  (set-frame-parameter frame 'name nil))))))


(defun perject-mode-line-indicator (col proj)
  "Return a string for the mode line indicator of perject.
COL is the current collection and PROJ is the current project name."
  (and col
       (propertize (concat col
						   (when proj
							 (concat " | " proj))
						   " ")
                   'face 'perject-mode-line-face)))

(defun perject-frame-title (perj)
  "Return a string for the frame title of a frame associated with PERJ.
PROJ is a dotted pair with car a collection and cdr a project name.
This function is used only if `perject-frame-title-format' is t."
  (concat invocation-name "@" system-name ":" (car perj)
		  (when (cdr perj)
			(concat "|" (cdr perj)))))


(defun perject--init ()
  "Load collections from the last session, set up hooks and select the last restored frame.
The collections are stored in desktop files. The variable `perject-load-at-startup'
determines which collections are loaded automatically. However, if specified, the
command line option `perject-command-line-option' takes priority."
  (let* ((current-frame (selected-frame))
         (all-collections (perject--list-collections))
         (cols-to-load
		  (pcase perject-load-at-startup
			('all all-collections)
			((pred listp) (if (eq (car perject-load-at-startup) 'not)
							  (cl-set-difference all-collections
												 (cdr perject-load-at-startup)
												 :test #'string-equal)
							perject-load-at-startup))
			('previous perject--previous-collections)
			((pred functionp) (funcall perject-load-at-startup
									   perject--previous-collections
									   all-collections)))))
	;; Read the command line arguments.
	;; We cannot use `command-switch-alist' since those functions are processed after `after-init-hook'.
	(when-let ((index (cl-position perject-command-line-option command-line-args :test #'string-equal))
			   (list (split-string (nth (1+ index) command-line-args) ","))
			   (cols (-separate #'perject--collection-p list)))
	  (when (cadr cols)
		(message "Perject: Warning: The following collections do not exist: %s" (string-join (cadr cols) ", ")))
	  (setq cols-to-load (car cols)
            command-line-args
			(append (seq-take command-line-args index) (seq-drop command-line-args (+ index 2)))))
	;; At the beginning, there is a single frame; namely the selected "starting
	;; frame".
	;; If `perject-reuse-starting-frame' is non-nil, we may reuse this frame for
	;; one of our projects, but as soon as it is "claimed" (or somehow deleted),
	;; we cannot use it again.
	;; This behavior is obtained by setting `perject--desktop-reuse-frames'.
	(let ((perject-switch-to-new-frame nil)
		  (perject--desktop-reuse-frames (lambda (frame)
										   (and
											(eq frame current-frame)
											(not (perject--current frame)))))
		  (starting-frame-claimed (not perject-reuse-starting-frame)))
      (dolist (name cols-to-load)
		(when (or (member current-frame perject--desktop-restored-frames)
				  (not (frame-live-p current-frame)))
		  (setq starting-frame-claimed t))
		(if starting-frame-claimed
			;; Starting frame was claimed.
			(let (perject-before-open-hook perject-after-open-hook)
			  (perject-open-collection name))
		  (perject-desktop-load name))))
    ;; Add the hooks specified in `perject-auto-add-hooks'.
    (dolist (hook perject-auto-add-hooks)
      (add-hook hook 'perject--auto-add-buffer))
	;; Select the last restored frame.
	(when-let ((frames (car (last (remove 'nil (mapcar #'perject--get-frames cols-to-load))))))
	  (select-frame-set-input-focus (car frames))
	  (raise-frame (car frames)))
    (run-hook-with-args 'perject-after-init-hook cols-to-load)))

(defun perject--exit ()
  "Save collections to be restored next time and prepare to exit Emacs.
The variabe `perject-save-on-exit' determines which collections are saved.
This function is called by `perject-mode' before exiting Emacs (using
`kill-emacs-hook')."
  (perject-save-collections 'exit t t)
  ;; Reverse the list of active collections, so that the newest collection is last.
  (setq perject--previous-collections (perject--list-collections 'active)))


;;;; Opening and Closing Collections

(defun perject-switch-collection (name &optional frame msg)
  "Switch the collection of the frame FRAME to the collection named NAME.
If FRAME is nil, it defaults to the current frame and if NAME is nil, remove the
current collection (if any) from the frame, so that it does not belong to any
collection anymore. If MSG is non-nil, also print a message.
In interactive use, the current frame is used, the user is asked for NAME
and MSG is determined by the variable `perject-messages'."
  (interactive
   (list
	(perject--get-collection-name
	 "Add current frame to collection: " 'active
	 (-compose #'not (apply-partially #'perject--is-assoc-with (selected-frame)))
	 t nil "No collection to add frame to" #'ignore)
	(selected-frame)
	(member 'switch-collection perject-messages)))
  (let ((frame (or frame (selected-frame)))
		(old-name (car (perject--current frame))))
	(perject--set-current name frame)
	(when msg
	  (let ((is-current (equal frame (selected-frame))))
	  (if name
		  (message "Added %sframe to collection '%s'."
				   (if is-current "current " "") name)
		(if old-name
			(message "Removed %sframe from collection '%s'."
					 (if is-current "current " "") old-name)
		  (message "%s is not associated with a collection."
				   (if is-current "Current frame" "Frame"))))))))

(defun perject-create-new-frame (proj &optional no-select)
  "Create a new frame for PROJ and select it, unless NO-SELECT is non-nil.
PROJ may be a collection name or a dotted pair with car a collection and cdr a
project name.
In interactive use, PROJ defaults to the current collection. If a single prefix
argument is supplied, the user may select PROJ from the projects of the current
collection. In any other case, the user may select from all projects."
  (interactive
   (list
    (if (and (not current-prefix-arg)
             (perject--current))
		(car (perject--current))
	  (perject--get-project-name "Create new frame for project: "
								 (if (equal current-prefix-arg '(4)) 'current 'all)
								 nil t (perject--current)
								 "No project to create a frame for"
								 "No project specified"))))
  (let ((frame (make-frame)))
	(with-selected-frame frame
	  (perject--set-current proj))
	(unless no-select (select-frame-set-input-focus frame))))

;; The `perject-open-collection' command is very subtle, due to the following:
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
(defun perject-open-collection (name)
  "Open the collection named NAME and switch to one of its corresponding frames (if existent).
This means that all its projects (which in turn correspond to buffers and window
configurations) and frames are restored from the corresponding desktop file.
Whether the frame switch happens depends on the value of
`perject-switch-to-new-frame'.
Note that the switching might not work, depending on the window manager. If
there is no collection named NAME yet, create it and open a frame for it.
Before creating the collection, run `perject-before-create-hook' and afterwards
run `perject-after-create-hook'.
If no new collection is created, run the hooks `perject-before-open-hook' and
`perject-after-open-hook' instead.
In interactive use, the user is asked for the project name.
If you want to further change the list of projects, do so in
`perject-before-open-hook' or `perject-after-open-hook'. Hooks that e.g. add a
buffer to a project have no effect while the desktop is being loaded."
  (interactive
   (list
	(perject--get-collection-name "Open collection: "
								  'inactive nil nil nil nil
								  "No collection specified")))
  (when (perject--collection-p name 'active)
	(user-error "The collection '%s' is already open" name))
  (if (perject--collection-p name)
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
		(let* ((wc (current-window-configuration))
			   (desktop-after-read-hook (cons (lambda () (set-window-configuration wc t))
											  desktop-after-read-hook)))
		  (perject-desktop-load name))
		(dolist (hook perject-auto-add-hooks)
		  (add-hook hook #'perject--auto-add-buffer))
		(when (and perject--desktop-restored-frames
				   (memq 'open perject-switch-to-new-frame))
		  (select-frame-set-input-focus (car perject--desktop-restored-frames)))
		(run-hook-with-args 'perject-after-open-hook name))
	(run-hook-with-args 'perject-before-create-hook name)
	(make-directory (perject--get-collection-dir name) t)
	;; Create a desktop file.
	(perject-save-collection name)
	;; The order of the collections matters and we want the new collection to
	;; be at the rightmost position.
	(setq perject-collections (append perject-collections (list (list name))))
	(perject-create-new-frame name (not (memq 'create perject-switch-to-new-frame)))
	(run-hook-with-args 'perject-after-create-hook name)))


(defun perject-open-collection-in-new-instance (name)
  "Open a new Emacs instance for the collection named NAME.
This means that a new Emacs process is created and in it, the collection is loaded.
In interactive use, the user is asked for the collection name."
  (interactive
   (list (perject--get-collection-name "Open collection in new instance: "
							'inactive nil t nil
							"No collection available" "No collection specified")))
  (let ((parameter
		 (concat
          perject-command-line-option " \"" name "\"")))
    ;; Start the new Emacs instance, see
    ;; https://emacs.stackexchange.com/questions/5428/restart-emacs-from-within-emacs.
    (call-process "sh" nil nil nil "-c" (concat "emacs " parameter " &"))))

(defun perject-close-collection (name &optional kill-buffers keep-frames)
  "Close the collection named NAME.
This closes all projects and frames belonging to this collection. In interactive
use, the user is asked for NAME.
If the optional argument KILL-BUFFERS is non-nil, kill all buffers that belong to that collection
and to no other collection or project. In interactive use, the prefix argument
determines this variable as specified by `perject-kill-buffers-by-default'.
The variable `perject-confirmation-kill-buffers' decides if the user is asked
for confirmation before any buffers are killed.
If the optional argument KEEP-FRAMES is non-nil, the frames belonging to the
collection are not deleted.
Depending on the value of the variable `perject-save-on-close', the collection
is saved to a desktop file. The variable `perject-confirmation' determines whether
the user is asked for confirmation before the collection is closed.
An error is thrown if there is no collections to close or all the open frames
belong to the selected collection.
This function runs the hooks `perject-before-close-hook' and
`perject-after-close-hook'."
  (interactive
   (list
    (perject--get-collection-name
     "Close collection: " 'active nil t (car (perject--current))
     "There currently is no collection to close"
	 "No collection specified")
	(or (and (member 'close perject-kill-buffers-by-default) (not current-prefix-arg))
		(and (not (member 'close perject-kill-buffers-by-default)) current-prefix-arg))))
  (when (or (not (member 'close perject-confirmation))
            (y-or-n-p (format "Closing collection '%s'. Are you sure?" name)))
    (when (and (eq (length (perject--get-frames name)) (length (frame-list)))
			   (not keep-frames))
      (user-error "Cannot close a collection which belongs to all open frames"))
	(let ((buffers (perject--get-buffers name))
		  buffers-to-kill)
      (run-hook-with-args 'perject-before-close-hook name buffers)
      (if (or (eq perject-save-on-close t)
              (and (eq perject-save-on-close 'ask)
                   (y-or-n-p (format "Save the collection '%s'? " name)))
			  (and (functionp perject-save-on-close)
				   (funcall perject-save-on-close name) buffers))
		  (perject-save-collection name t t)
		;; If we don't save, we need to manually remove the lock file.
		(desktop-release-lock (file-name-as-directory (perject--get-collection-dir name))))
	  ;; Remove the collection from the list of active collections.
	  (setq perject-collections (assoc-delete-all name perject-collections))
	  ;; Remove all buffers from the collection.
	  (dolist (buffer buffers)
		(with-current-buffer buffer
		  (setq perject-buffer
				(cl-delete-if (-compose (apply-partially #'string-equal name) #'car)
							  perject-buffer))
		  (unless perject-buffer
			(push buffer buffers-to-kill))))
	  ;; Delete the frames unless requested otherwise.
      (unless keep-frames
		(dolist (frame (perject--get-frames name))
          (delete-frame frame)))
	  (when (and kill-buffers
				 buffers-to-kill
				 (or (not (member 'close perject-confirmation-kill-buffers))
					 (y-or-n-p (format "Kill buffers belonging only to collection '%s'?" name))))
		(dolist (buffer buffers-to-kill)
		  (kill-buffer buffer))
		(setq buffers (cl-set-difference buffers buffers-to-kill)))
      (run-hook-with-args 'perject-after-close-hook name buffers))))

(defun perject-reload-collection (name &optional kill-buffers)
  "Reload the collection named NAME from its desktop file.
This discards any changes to the collection and reverts it to the state from the
previous save.
The variable `perject-confirmation' determines whether the user is asked for
confirmation before reloading the collection.
If the optional argument KILL-BUFFERS is non-nil, kill all
buffers that belong to that collection and to no other collection or project. In
interactive use, the prefix argument determines this variable as specified by
`perject-kill-buffers-by-default'.
The variable `perject-switch-to-new-frame' decides if the focus is switched to
one of the created frames.
This function runs the hooks `perject-before-reload-hook' and
`perject-after-reload-hook'."
  (interactive
   (list
	(perject--get-collection-name "Reload collection: " 'active nil t (car (perject--current))
								  "There is no active collection to reload"
								  "No collection specified")
	(or (and (member 'reload perject-kill-buffers-by-default) (not current-prefix-arg))
		(and (not (member 'reload perject-kill-buffers-by-default)) current-prefix-arg))))
  (when (or (not (member 'reload perject-confirmation))
			(y-or-n-p (format "Reload collection '%s'?" name)))
	(run-hook-with-args 'perject-before-reload-hook name)
	;; Allow reusing frames that belong to the collection.
	(let ((perject--desktop-reuse-frames
		   (-compose (apply-partially #'string-equal name) #'car #'perject--current))
		  (frames (perject--get-frames name))
			  (perject-confirmation-kill-buffers
		   (and (member 'reload perject-confirmation-kill-buffers) (list 'close)))
		  perject-switch-to-new-frame perject-save-on-close
		  perject-before-close-hook perject-after-close-hook
		  perject-before-open-hook perject-after-open-hook)
	  (perject-close-collection name kill-buffers t)
	  (perject-open-collection name)
	  ;; Cleanup the frames that were not reused.
	  (dolist (frame (cl-set-difference frames perject--desktop-restored-frames))
		(delete-frame frame)))
	(run-hook-with-args 'perject-after-reload-hook name)))


;;;; Managing Buffers

(defun perject-add-buffer-to-project (buffer proj &optional msg)
  "Add the buffer BUFFER to the project PROJ.
PROJ is a dotted pair with car a collection name and cdr a project name. If MSG
is non-nil, also display a message upon completion.
In interactive use, the current buffer is added to the current project. If a
single prefix argument is supplied or if the current frame is not associated
with any project, the user is asked to choose the project from the current
collection. In any other case the user may choose from the list of all projects
from all active collections.
In interactive use, depending on the value of `perject-messages', a message is
printed upon successfully adding the buffer to the project.
If the buffer is already associated with the project, an error is thrown.
Note that this function does not check whether PROJ is an existent project and
whether BUFFER has already been killed, so caller functions should take care of
that."
  (interactive
   (list
	(current-buffer)
	(if (and (not current-prefix-arg) (cdr (perject--current)))
		(perject--current)
	  ;; Let the user select from the projects of the current collection only if
	  ;; not all of them are already associated with the buffer.
	  (if-let (((or (not current-prefix-arg) (eq current-prefix-arg '(4))))
			   (col (car (perject--current)))
			   (projects (cl-remove-if
						  (apply-partially #'perject--is-assoc-with (current-buffer))
						  (perject--list-projects col))))
		  (perject--get-project-name
		   "Add buffer to project: " projects nil t nil nil
		   "No project specified")
		(perject--get-project-name
		 "Add buffer to project: " 'all
		 (-compose 'not (apply-partially #'perject--is-assoc-with (current-buffer)))
		 t nil
		 "All projects are already associated with the current buffer"
		 "No project specified")))
    (member 'add-buffer perject-messages)))
  (when (perject--is-assoc-with buffer proj)
	(user-error "Buffer '%s' is already associated with project '%s'."
                (buffer-name buffer) (perject-project-to-string proj)))
  (with-current-buffer buffer
	(push proj perject-buffer))
  (when msg
    (message "Added buffer '%s' to project '%s'."
			 (buffer-name buffer) (perject-project-to-string proj))))


(defun perject--auto-add-buffer (&optional ignore)
  "Silently add the current buffer to projects honoring `perject-auto-add-function'.
If IGNORE is non-nil, simply add the current buffer to the current project;
i.e. act as if `perject-auto-add-function' was nil.
Does nothing to projects that are already associated with the buffer."
  (let ((buffer (current-buffer)))
	(if (or ignore (not perject-auto-add-function))
		(let ((project (perject--current)))
		  (when (and (cdr project) (not (perject--is-assoc-with buffer project)))
			(perject-add-buffer-to-project buffer project)))
	  (dolist (project (funcall perject-auto-add-function buffer (perject--current)))
		(when (and (perject--project-p project)
				   (not (perject--is-assoc-with buffer project)))
		  (perject-add-buffer-to-project buffer project))))))


(defun perject-remove-buffer-from-project (buffer proj &optional msg)
  "Remove the buffer BUFFER from the project PROJ.
PROJ is a dotted pair with car a collection name and cdr a project name. If MSG
is non-nil, also display a message upon completion.
In interactive use, the current buffer is removed from the current project. If a
prefix argument is supplied or if the current frame is not associated with any
project, the user is asked to choose the project from the list of all projects
that are currently associated with BUFFER.
In interactive use, depending on the value of `perject-messages', a message is
printed upon successfully removing the buffer from the project.
If the buffer is not associated with the project, an error is thrown.
Note that this function does not check whether PROJ is an existent project and
whether BUFFER has already been killed, so caller functions should take care of
that.
The variable `perject-empty-project-delete' determines what happens if the last
buffer of a project is removed."
  (interactive
   (list
	(current-buffer)
	(if (and (not current-prefix-arg) (cdr (perject--current)))
		(perject--current)
	  ;; Let the user select from the projects of the current collection only if
	  ;; not all of them are already associated with the buffer.
	  (if-let (((or (not current-prefix-arg) (eq current-prefix-arg '(4))))
			   (col (car (perject--current)))
			   (projects (cl-remove-if-not
						  (apply-partially #'perject--is-assoc-with (current-buffer))
						  (perject--list-projects col))))
		  (perject--get-project-name
		   "Remove buffer from project: " projects nil t nil nil
		   "No project specified")
		(perject--get-project-name
		 "Remove buffer from project: " 'all
		 (apply-partially #'perject--is-assoc-with (current-buffer))
		 t nil
		 "The buffer is currently not associated with any project"
		 "No project specified")))
	(member 'remove-buffer perject-messages)))
  (unless (perject--is-assoc-with buffer proj)
	(user-error "Buffer '%s' is not associated with project '%s'."
                (buffer-name buffer) (perject-project-to-string proj)))
  (with-current-buffer buffer
	(setq perject-buffer (delete proj perject-buffer)))
  (when msg
    (message "Removed buffer '%s' from project '%s'."
			 (buffer-name buffer) (perject-project-to-string proj)))
  (when (and
		 (null (perject--get-buffers proj))
		 (or (eq perject-empty-project-delete t)
			 (and (eq perject-empty-project-delete 'ask)
                  (y-or-n-p
				   (format
					"Project '%s' is not associated with any buffers anymore. Delete it?"
					(perject-project-to-string proj))))
			 (and (functionp perject-empty-project-delete)
				  (funcall perject-empty-project-delete proj))))
    (let (perject-confirmation)
      (perject-delete-collection name))))


;;;; Managing Collections

(defun perject-rename-collection (old-name new-name)
  "Rename the collection named OLD-NAME to NEW-NAME.
In interactive use, the user is asked for both collection names."
  (interactive
   (list
    (perject--get-collection-name
     "Select collection to rename: " 'active nil t (car (perject--current))
     "There currently is no collection to rename"
	 "No collection specified")
    (perject--get-new-collection-name "New name: ")))
  (dolist (frame (frame-list))
	(when (perject--is-assoc-with frame old-name)
	  (perject--set-current (cons new-name (cdr (perject--current frame))) frame)))
  (setcar (assoc old-name perject-collections) new-name)
  (dolist (buffer (buffer-list))
	(with-current-buffer buffer
	  (setq perject-buffer
			(mapcar (lambda (proj)
					  (if (string-equal (car proj) old-name)
						  (cons new-name (cdr proj))
						proj))
					perject-buffer))))
  (when (file-exists-p (perject--get-collection-dir old-name))
	(rename-file (perject--get-collection-dir old-name)
				 (perject--get-collection-dir new-name))))

(defun perject-delete-collection (name &optional kill-buffers)
  "Delete the collection named NAME.
This includes closing the collection and deleting all corresponding projects and
the corresponding desktop file. In interactive use, the user is asked for NAME.
The variable `perject-confirmation' determines whether the user is asked for
confirmation before deleting the collection.
If the optional argument KILL-BUFFERS is non-nil, kill all buffers that belong
to that collection and to no other collection or project. In interactive use,
the prefix argument determines this variable as specified by
`perject-kill-buffers-by-default'.
The variable `perject-confirmation-kill-buffers' decides if the user is asked
for confirmation before any buffers are killed.
An error is thrown if there is no collection to delete or all the open frames
belong to the selected collection."
  (interactive
   (list
    (perject--get-collection-name
     "Delete collection: " 'active nil t (car (perject--current))
     "There currently is no collection to delete"
	 "No collection specified")
    (or (and (member 'delete perject-kill-buffers-by-default) (not current-prefix-arg))
		(and (not (member 'delete perject-kill-buffers-by-default)) current-prefix-arg))))
  (when (or (not (member 'delete perject-confirmation))
            (y-or-n-p (format "Deleting collection '%s'. Are you sure?" name)))
    ;; If the collection is active, close it.
    (when (perject--collection-p name 'active)
      (let ((perject-confirmation-kill-buffers
			 (and (member 'delete perject-confirmation-kill-buffers)
				  (list 'close)))
			perject-save-on-close perject-confirmation)
        (perject-close-collection name kill-buffers)))
    (when (file-exists-p (perject--get-collection-dir name))
      (delete-directory (perject--get-collection-dir name) t))))

(defun perject-save-collection (name &optional release-lock no-msg)
  "Save the collection named NAME.
In interactive use, if a prefix argument is supplied or the current frame is not
associated with any collection, ask the user for NAME, presenting the currently
active collections as candidates. Otherwise, use the current collection.
If the optional argument RELEASE-LOCK is non-nil, Emacs will release the lock of
the corresponding desktop file. This option is always nil in interactive use. If
the optional argument NO-MSG is non-nil, don't print any messages."
  (interactive
   (list
    (if (or current-prefix-arg
            (not (perject--current)))
        (perject--get-project "Save collection: " 'active nil t (car (perject--current))
								   "No collection to save" "No collection specified")
      (car (perject--current)))))
  (perject-desktop-save name release-lock no-msg))

(defun perject-save-collections (names &optional release-lock no-msg)
  "Save the list of collections NAMES.
NAMES may have one of the following values:
- a list of collections to save (non active ones are ignored)
- 'all: save all active collections
- 'exit: save the active collections in accordance with `perject-save-on-exit'
In interactive use, no prefix argument corresponds to 'all, one prefix argument
corresponds to 'exit and any other prefix argument allows the user to manually
select the list of collections to be saved using `completing-read-multiple'.
If the optional argument RELEASE-LOCK is non-nil, Emacs will release the lock of
the corresponding desktop file. If the optional argument NO-MSG is non-nil, no
messages are printed."
  (interactive
   (list
	(pcase current-prefix-arg
	  ('nil 'all)
	  ('(4) 'exit)
	  (_ (completing-read-multiple
		  "Save Collections: " (perject--list-collections 'active) nil t)))))
  (let ((collections
		 (pcase names
		   ((pred listp) names)
		   ('all (perject--list-collections 'active))
		   ('exit (pcase perject-save-on-exit
					('all (perject--list-collections 'active))
					((pred listp) (if (eq (car perject-save-on-exit) 'not)
									  (cl-set-difference (perject--list-collections 'active)
														 (cdr perject-save-on-exit)
														 :test #'string-equal)
									perject-save-on-exit))
					('recent perject--previous-collections)
					((pred functionp) (funcall perject-save-on-exit
											   perject--previous-collections
											   (perject--list-collections 'active))))))))
    (dolist (name collections)
	  (perject-desktop-save name release-lock (not (eq (length collections) 1))))
	(when (and collections (not no-msg))
	  (message "Perject: Saved collections: %s" (string-join collections ", ")))))


;;;; Managing Projects

(defun perject-switch-project (proj &optional msg)
  "Switch to the project PROJ within the current collection.
PROJ may either be a project name within the current collection or a dotted pair
with car the current collection name and cdr a project name.
If no such project exists, create it. If PROJ is nil, deselect the
current project and only focus on the current collection.
If the optional argument MSG is non-nil, also print an informative message.
In interactive use, this is determined by `perject-messages'."
  (interactive
   (progn
	  (perject-assert-collection)
	  (list (perject--get-project-name "Switch to project (or create new one): " 'current nil nil nil
									   nil #'ignore)
			(member 'switch-project perject-messages))))
  (let ((proj (if (or (not proj) (stringp proj))
				  (cons (car (perject--current)) proj)
				proj)))
	;; If the project does not exist yet, add it to the end of the current collection.
	(when (and (cdr proj) (not (perject--project-p proj)))
	  (setcdr
	   (assoc (car proj) perject-collections)
	   (append (alist-get (car proj) perject-collections nil nil #'string-equal)
			   (list (cdr proj)))))
	(perject--set-current proj)
	(when msg
	  (if (cdr proj)
		  (message "Switched to project '%s'." (perject-project-to-string proj))
		(message "Switch to collection '%s'." (car proj))))))

(defun perject-switch-to-next-project (&optional msg)
  "Switch to the next project within the current collection.
If there is no current collection, throw an error.
If the optional argument MSG is non-nil, also print an informative message.
In interactive use, this is determined by `perject-messages'."
  (interactive (list (member 'next-project perject-messages)))
  (perject-assert-collection)
  (let ((projects (alist-get (car (perject--current)) perject-collections nil nil #'string-equal))
		(current (cdr (perject--current))))
	(unless projects
	  (user-error "The current collection has no associated projects"))
	(let ((index (or (and current (cl-position current projects)) 0)))
	  (perject-switch-project (nth (mod (1+ index) (length projects)) projects) msg))))

(defun perject-switch-to-previous-project (&optional msg)
  "Switch to the previous project within the current collection.
If there is no current collection, throw an error.
If the optional argument MSG is non-nil, also print an informative message.
In interactive use, this is determined by `perject-messages'."
  (interactive (list (member 'previous-project perject-messages)))
  (perject-assert-collection)
  (let ((projects (alist-get (car (perject--current)) perject-collections nil nil #'string-equal))
		(current (cdr (perject--current))))
	(unless projects
	  (user-error "The current collection has no associated projects"))
	(let ((index (or (and current (cl-position current projects)) 0)))
	  (perject-switch-project (nth (mod (1- index) (length projects)) projects) msg))))

(defun perject-rename-project (proj new-proj)
  "Rename the project PROJ to NEW-PROJ.
PROJ and NEW-PROJ are both dotted pairs with car a collection and cdr a project
name. NEW-PROJ may also be a string to be used as the new project name within
the same collection.
In interactive use, the user is asked for the project to rename and for the new
name. If a prefix argument is supplied, the user is also asked to select a
collection to move PROJ into."
  (interactive
   (let* ((proj
		   (perject--get-project-name
			"Select project to rename: " 'all nil t (perject--current)
			"There currently is no project to rename"
			"No project specified"))
		  (col
		   (if current-prefix-arg
			   (perject--get-collection-name
				"Select a collection to move the project to: " 'active nil t
				(car (perject--current))
				nil
				"No collection specified")
			 (car proj))))
	 (list
	  proj
	  (cons col (perject--get-new-project-name
				 col (format "New name of project '%s' in collection '%s': " (cdr proj) col))))))
  (let* ((old-col (car proj))
		 (old-name (cdr proj))
		 (new-col (if (stringp new-proj) old-col (car new-proj)))
		 (new-name (if (stringp new-proj) new-proj (cdr new-proj)))
		 (new-proj (cons new-col new-name))
		 (old-col-list (assoc old-col perject-collections)))
	(dolist (frame (frame-list))
	  (when (perject--is-assoc-with frame proj)
		(perject--set-current new-proj frame)))
	(if (string-equal old-col new-col)
		(setcdr old-col-list (cl-substitute new-name old-name (cdr old-col-list) :test #'equal))
	  (setcdr old-col-list (delete old-name (cdr old-col-list)))
	  (push new-name (alist-get new-col perject-collections nil nil #'string-equal)))
	(dolist (buffer (perject--get-buffers proj))
	  (with-current-buffer buffer
		(setq perject-buffer (cl-substitute new-proj proj perject-buffer :test #'equal))))))

(defun perject-delete-project (proj &optional kill-buffers)
  "Delete the project PROJ.
In interactive use, the user is asked for PROJ.
The variable `perject-confirmation' determines whether the user is asked for
confirmation before deleting the project.
If the optional argument KILL-BUFFERS is non-nil, kill all buffers that belong
to that collection and to no other project. In interactive use, the prefix
argument determines this variable as specified by
`perject-kill-buffers-by-default'.
The variable `perject-confirmation-kill-buffers' decides if the user is asked
for confirmation before any buffers are killed.
An error is thrown if there is no project to delete.
This function runs the hooks `perject-before-delete-project-hook' and
`perject-after-delete-project-hook'."
  (interactive
   (list
    (perject--get-project-name
     "Delete project: " 'all nil t (perject--current)
     "There currently is no project to delete"
	 "No project specified")
    (or (and (member 'delete-project perject-kill-buffers-by-default) (not current-prefix-arg))
		(and (not (member 'delete-project perject-kill-buffers-by-default)) current-prefix-arg))))
  (when (or (not (member 'delete-project perject-confirmation))
            (y-or-n-p (format "Deleting project '%s'. Are you sure?"
							  (perject-project-to-string proj))))
	(let ((buffers (perject--get-buffers proj))
		  buffers-to-kill)
	  (run-hook-with-args 'perject-before-delete-project-hook proj buffers)
	  ;; Remove the project from the list of active collections.
	  (let ((col (assoc (car proj) perject-collections)))
		(setcdr col (delete (cdr proj) (cdr col))))
	  ;; Remove all buffers from the project.
	  (dolist (buffer buffers)
		(with-current-buffer buffer
		  (setq perject-buffer (delete proj perject-buffer))
		  (unless perject-buffer
			(push buffer buffers-to-kill))))
	  (when (and kill-buffers
				 buffers-to-kill
				 (or (not (member 'delete-project perject-confirmation-kill-buffers))
					 (y-or-n-p (format "Kill buffers belonging only to project '%s'?"
									   (perject-project-to-string proj)))))
		(dolist (buffer buffers-to-kill)
		  (kill-buffer buffer))
		(setq buffers (cl-set-difference buffers buffers-to-kill)))
	  (run-hook-with-args 'perject-after-delete-project-hook proj buffers))))

(defun perject-print-buffer-projects (&optional buffer)
  "Print the names of the projects with which the buffer BUFFER is associated.
If nil, BUFFER defaults to the current buffer, which is also its value in
interactive use."
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
		(buffer-name (buffer-name buffer)))
	(pcase (buffer-local-value 'perject-buffer buffer)
	  ('nil
	   (message "The buffer '%s' is not associated with any projects." buffer-name))
	  (`(,project)
	   (message "The buffer '%s' is associated with the project '%s'." buffer-name (perject-project-to-string project)))
	  (projects
	   (message "The buffer '%s' is associated with the projects: %s."
				buffer-name
				(string-join (mapcar #'perject-project-to-string projects) ", "))))))

(defun perject-project-to-string (proj)
  "Return a string representing the project PROJ.
PROJ is a dotted pair with car a collection and cdr a project name.
Which string is returned is determined by `perject-project-format'."
  (if (stringp perject-project-format)
	  (format perject-project-format (car proj) (cdr proj))
	(funcall perject-project-format (car proj) (cdr proj))))


;;;; Sorting Collections and Projects

(transient-define-prefix perject-sort-collections ()
  "Transient menu to sort the active collections.
This is for example useful to influence the order in which collections are
loaded."
  [:description
   (lambda ()
	 (let ((col (perject--list-collections 'active)))
	   (unless (> (length col) 0)
		 (user-error "There currently are no collections to sort"))
	   (setq perject--sort-collections-index (min perject--sort-collections-index (1- (length col))))
	   (let ((current (nth perject--sort-collections-index col)))
		 (concat
		  "Sort collections: "
		  (string-join (mapcar (lambda (c)
								 (propertize c 'face
											 (if (string-equal c current)
												 'perject-sort-collections-current
											   'perject-sort-collections-other)))
							   col)
					   ", ")))))
   ("f" "Shift marked collection to right" perject--sort-collections-shift-right :transient t)
   ("b" "Shift marked collection to left" perject--sort-collections-shift-left :transient t)
   ("n" "Select the next collection" perject--sort-collections-next :transient t)
   ("p" "Select the previous collection" perject--sort-collections-previous :transient t)])

(defun perject--sort-collections-shift-right ()
  "Shift collection to the right.
The collection is determined by `perject--sort-collections-index'."
  (interactive)
  (unless (eq transient-current-command 'perject-sort-collections)
    (user-error "This function can only be called within `perject-sort-collections'"))
  (setq perject-collections
		(if (eq perject--sort-collections-index (1- (length perject-collections)))
			;; The current entry is the last one.
			(cons (car (last perject-collections)) (butlast perject-collections))
		  (append
		   (seq-take perject-collections perject--sort-collections-index)
		   (list (nth (1+ perject--sort-collections-index) perject-collections))
		   (list (nth perject--sort-collections-index perject-collections))
		   (seq-drop perject-collections (+ perject--sort-collections-index 2)))))
  (setq perject--sort-collections-index (mod (1+ perject--sort-collections-index) (length perject-collections))))

(defun perject--sort-collections-shift-left ()
  "Shift collection to the left.
The collection is determined by `perject--sort-collections-index'."
  (interactive)
  (let ((perject--sort-collections-index (mod (1- perject--sort-collections-index) (length perject-collections))))
	(perject--sort-collections-shift-right))
  (setq perject--sort-collections-index (mod (1- perject--sort-collections-index) (length perject-collections))))

(defun perject--sort-collections-next ()
  "Select the next collection as determined by `perject--sort-collections-index'."
  (interactive)
  (unless (eq transient-current-command 'perject-sort-collections)
    (user-error "This function can only be called within `perject-sort-collections'"))
  (setq perject--sort-collections-index (mod (1+ perject--sort-collections-index) (length perject-collections))))

(defun perject--sort-collections-previous ()
  "Select the previous collection as determined by `perject--sort-collections-index'."
  (interactive)
  (unless (eq transient-current-command 'perject-sort-collections)
    (user-error "This function can only be called within `perject-sort-collections'"))
  (setq perject--sort-collections-index (mod (1- perject--sort-collections-index) (length perject-collections))))


(transient-define-prefix perject-sort-projects ()
  "Transient menu to sort the projects of the current collection.
This is for example useful to influence the order used for
`perject-switch-to-next-project' and `perject-switch-to-previous-project'."
  [:description
   (lambda ()
	 (perject-assert-collection)
	 (let ((projects (mapcar #'cdr (perject--list-projects (car (perject--current))))))
	   (unless (> (length projects) 0)
		 (user-error "The current collection has no projects to sort"))
	   (setq perject--sort-projects-index (min perject--sort-projects-index (1- (length projects))))
	   (let ((current (nth perject--sort-projects-index projects)))
		 (format "Sort projects of collection '%s': %s"
				 (car (perject--current))
				 (string-join (mapcar (lambda (p)
										(propertize p 'face
													(if (equal p current)
														'perject-sort-projects-current
													  'perject-sort-projects-other)))
									  projects)
							  ", ")))))
   ("f" "Shift marked project to right" perject--sort-projects-shift-right :transient t)
   ("b" "Shift marked project to left" perject--sort-projects-shift-left :transient t)
   ("n" "Select the next project" perject--sort-projects-next :transient t)
   ("p" "Select the previous project" perject--sort-projects-previous :transient t)])

(defun perject--sort-projects-shift-right ()
  "Shift project to the right.
The project is determined by `perject--sort-projects-index'."
  (interactive)
  (unless (eq transient-current-command 'perject-sort-projects)
    (user-error "This function can only be called within `perject-sort-projects'"))
  (let ((projects (alist-get (car (perject--current)) perject-collections nil nil #'string-equal)))
	(setcdr (assoc (car (perject--current)) perject-collections)
		  (if (eq perject--sort-projects-index (1- (length projects)))
			  ;; The current entry is the last one.
			  (cons (car (last projects)) (butlast projects))
			(append
			 (seq-take projects perject--sort-projects-index)
			 (list (nth (1+ perject--sort-projects-index) projects))
			 (list (nth perject--sort-projects-index projects))
			 (seq-drop projects (+ perject--sort-projects-index 2)))))
	(setq perject--sort-projects-index (mod (1+ perject--sort-projects-index) (length projects)))))

(defun perject--sort-projects-shift-left ()
  "Shift project to the left.
The collection is determined by `perject--sort-projects-index'."
  (interactive)
  (let ((length (length (alist-get (car (perject--current)) perject-collections nil nil #'string-equal))))
	(let ((perject--sort-projects-index (mod (1- perject--sort-projects-index) length)))
	  (perject--sort-projects-shift-right))
	(setq perject--sort-projects-index (mod (1- perject--sort-projects-index) length))))

(defun perject--sort-projects-next ()
  "Select the next collection as determined by `perject--sort-projects-index'."
  (interactive)
  (unless (eq transient-current-command 'perject-sort-projects)
    (user-error "This function can only be called within `perject-sort-projects'"))
  (setq perject--sort-projects-index
		(mod (1+ perject--sort-projects-index)
			 (length (alist-get (car (perject--current)) perject-collections nil nil #'string-equal)))))

(defun perject--sort-projects-previous ()
  "Select the previous collection as determined by `perject--sort-projects-index'."
  (interactive)
  (unless (eq transient-current-command 'perject-sort-projects)
    (user-error "This function can only be called within `perject-sort-projects'"))
  (setq perject--sort-projects-index
		(mod (1- perject--sort-projects-index)
			 (length (alist-get (car (perject--current)) perject-collections nil nil #'string-equal)))))


;;;; Internal Interface

(defun perject--current (&optional frame)
  "Return the collection and project currently associated with the frame FRAME.
If FRAME is nil, use the current frame.
The returned value is a dotted pair with car the collection and cdr the project
name."
  (frame-parameter frame 'perject-project))

(defun perject--set-current (proj &optional frame)
  "Set the collection and project of the frame FRAME to PROJ.
PROJ may be a dotted pair with car the collection and cdr the project name.
It may alternatively be a collection name.
If PROJ is nil, the frame will no longer belong to a collection.
If FRAME is nil, it defaults to the selected frame."
  (let ((proj (if (stringp proj) (cons proj nil) proj)))
	(set-frame-parameter frame 'perject-project proj)
	(when perject-frame-title-format
	  (set-frame-parameter frame 'name (and proj (funcall perject-frame-title-format proj))))
	(when perject-mode-line-format
	  (force-mode-line-update))))

(defun perject--is-assoc-with (obj proj)
  "Return a non-nil value if object OBJ is associated with PROJ.
Otherwise, nil is returned. PROJ may either be a dotted pair with car a
collection and cdr a project name or a collection name. If it is a dotted pair
with cdr nil, behave as if it was only the collection name. OBJ must be a buffer
or a frame."
  (let ((proj (if (stringp proj) (cons proj nil) proj)))
  (if (null (cdr proj))
	  (pcase-exhaustive obj
		((pred bufferp)
		 (cl-some (-compose (apply-partially #'string-equal (car proj)) #'car)
				  (buffer-local-value 'perject-buffer obj)))
		((pred framep) (string-equal (car proj) (car (perject--current obj)))))
	(pcase-exhaustive obj
	  ((pred bufferp) (member proj (buffer-local-value 'perject-buffer obj)))
	  ((pred framep) (equal proj (perject--current obj)))))))

(defun perject--is-anonymous-buffer (buffer)
  "Return non-nil if the buffer BUFFER is anonymous.
This means that it is not associated with any project."
  (null (buffer-local-value 'perject-buffer buffer)))

(defun perject--get-buffers (proj)
  "Return the list of buffers associated with PROJ.
PROJ may be a dotted pair with car a collection and cdr a project name. It may
alternatively be a collection name."
  (cl-remove-if-not
   (-compose
	(apply-partially #'cl-some
					 (if (stringp proj)
						 (-compose (apply-partially #'string-equal proj) #'car)
					   (apply-partially #'equal proj)))
	(apply-partially #'buffer-local-value 'perject-buffer))
   (buffer-list)))

(defun perject--get-anonymous-buffers ()
  "Return the list of all buffers not associated with any project."
  (cl-remove-if (apply-partially #'buffer-local-value 'perject-buffer) (buffer-list)))

(defun perject--collection-p (name &optional scope)
  "Return a non-nil value if there exists a collection called NAME.
The optional argument SCOPE behaves like for `perject--list-collections'."
  (member name (perject--list-collections scope)))

(defun perject--project-p (proj)
  "Return a non-nil value if the project PROJ exists.
PROJ is a cons cell with car a collection name and cdr a project name."
  (member (cdr proj) (alist-get (car proj) perject-collections nil nil #'string-equal)))

(defun perject-assert-collection (&optional frame)
  "Ensure that frame FRAME has a current collection and return it (a string).
If not, throw an error. If nil, FRAME defaults to the selected frame."
  (or (car (perject--current frame))
	  (user-error "The %sframe is not associated with any collection"
				  (if frame "" "current "))))

(defun perject-assert-project (&optional frame)
  "Ensure that frame FRAME has a current project and return it (a dotted pair).
If not, throw an error. If nil, FRAME defaults to the selected frame."
  (let ((current (perject--current frame)))
	(or (and (cdr current) current)
		(user-error "The %sframe is not associated with any project"
					(if frame "" "current ")))))

(defun perject--get-collection-dir (name)
  "Return the directory belonging to a (possibly non-existent) collection with name NAME."
  (expand-file-name (concat (file-name-as-directory perject-directory) name)))

(defun perject--get-frames (proj)
  "Return the currently open frames which belong to PROJ.
PROJ may either be a dotted pair with car a collection and cdr a project name or
a collection name."
  (cl-remove-if-not (lambda (frame) (perject--is-assoc-with frame proj))
					(frame-list)))

(defun perject--get-collection-name
	(prompt type &optional predicate require-match def no-candidate empty-string)
  "Ask the user for a collection name using `completing-read' and return it.
TYPE may have one of the following values, which determines the available
candidates:
- a list of collections
- 'active: active collections
- 'inactive: inactive collections
- 'all: all collections
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
  (let* ((candidates
		  (pcase type
			('all (perject--list-collections))
			('inactive (perject--list-collections 'inactive))
			('active (perject--list-collections 'active))
			(_ type)))
		 (collection
		  (or
		   (if (functionp predicate)
			   (cl-remove-if-not predicate candidates)
			 candidates)
		   (pcase no-candidate
			 ((pred functionp) (funcall no-candidate))
			 ((pred stringp) (user-error no-candidate)))))
		 (completion-extra-properties
		  `(:annotation-function
			,(lambda (str)
			   (concat
				(propertize " " 'display '(space :align-to (- center 40)))
				(propertize
				 (or
				  (and (perject--collection-p str 'active)
					   (or (string-join (mapcar #'cdr (perject--list-projects str)) ", ")
						   "no projects"))
				  (format-time-string "%Y %b %d %a" (file-attribute-access-time
													 (file-attributes
													  (desktop-full-file-name
													   (perject--get-collection-dir str)))))
				  "")
				 'face 'perject-project-annotator-main)
				(when (perject--collection-p str 'active)
				  (concat
				   (propertize " " 'display '(space :align-to center))
				   (propertize
					(format "%4d buffer%s"
							(length (perject--get-buffers str))
							(if (not (eq (length (perject--get-buffers str)) 1)) "s" ""))
					'face 'perject-project-annotator-buffers)
				   (propertize " " 'display '(space :align-to (+ center 30)))
				   (propertize
					(format "%4d frame%s%s"
							(length (perject--get-frames str))
							(if (not (eq (length (perject--get-frames str)) 1)) "s" "")
							(if (string-equal (car (perject--current)) str) " [current]" ""))
					'face 'perject-project-annotator-frames)))))))
		 (name
		   (completing-read
			prompt collection nil require-match nil 'perject-collection-name-history def)))
	(if (and (string-empty-p name) empty-string)
		(if (functionp empty-string)
			(funcall empty-string)
		  (user-error empty-string))
	  name)))

(defun perject--get-project-name
	(prompt type &optional predicate require-match def no-candidate empty-string)
  "Ask the user for a project name using `completing-read' and return it.
The arguments are the same as for `perject--get-collection-name', except for
TYPE. It may have one of the following values:
- a list of dotted pairs (collection . project)
- a collection name: only those projects belonging to that collection
- 'current: only those projects belonging to the current collection;
  if there is no current collection, behave like 'all
- 'all: all projects from all active collections."
  (let* ((candidates
		  (pcase type
			('all (perject--list-projects 'all))
			('current (perject--list-projects))
			((pred stringp) (perject--list-projects type))
			(_ type)))
		 (collection
		  (or
		   (if (functionp predicate)
			   (cl-remove-if-not predicate candidates)
			 candidates)
		   (pcase no-candidate
			 ((pred functionp) (funcall no-candidate))
			 ((pred stringp) (user-error no-candidate)))))
		 (alist
		  (mapcar (lambda (cand) (cons (perject-project-to-string cand) cand)) collection))
		 (completion-extra-properties
		  `(:annotation-function
			,(lambda (str)
			   (let ((proj (alist-get str alist nil nil #'string-equal)))
				 (concat
				  (when (perject--project-p proj)
					(concat
					 (propertize " " 'display '(space :align-to (- center 30)))
					 (propertize
					  (format "%4d buffer%s"
							  (length (perject--get-buffers proj))
							  (if (not (eq (length (perject--get-buffers proj)) 1)) "s" ""))
					  'face 'perject-project-annotator-buffers)
					 (propertize " " 'display '(space :align-to (+ center 30)))
					 (propertize
					  (format "%4d frame%s%s"
							  (length (perject--get-frames proj))
							  (if (not (eq (length (perject--get-frames proj)) 1)) "s" "")
							  (if (equal (perject--current) proj) " [current]" ""))
					  'face 'perject-project-annotator-frames))))))))
		 (name
		  (completing-read
		   prompt alist nil require-match nil 'perject-project-name-history
		   (and (cdr def) (perject-project-to-string def)))))
	(if (and (string-empty-p name) empty-string)
		(if (functionp empty-string)
			(funcall empty-string)
		  (user-error empty-string))
	  (alist-get name alist name nil #'equal))))


(defun perject--get-new-collection-name (prompt)
  "Ask the user for a new collection name using `read-string' and return it.
PROMPT is a string to prompt with; normally it ends in a colon and a space. The
string entered must be a valid nonexistent collection name; i.e. it may only
contain letters, digits and any characters specified by
`perject-valid-naming-chars'. If there already exists a collection with the the
specified name, an error is thrown."
  (let ((name (read-string prompt)))
    (mapc
	 (lambda (char)
       (or (and (>= char 48) (<= char 57)) ;; digit
           (and (>= char 65) (<= char 90)) ;; uppercase letter
           (and (>= char 97) (<= char 122)) ;; lowercase letter
           (member char perject-valid-naming-chars)
           (user-error
			"The character '%c' is not valid for naming a collection. See the variable `perject-valid-naming-chars'."
			char)))
	 name)
	(when (string-empty-p name)
      (user-error "The collection name cannot be empty."))
    (when (member name (perject--list-collections))
      (user-error "There already is a collection named '%s'." name))
    name))

(defun perject--get-new-project-name (collection prompt)
  "Ask the user for a new project name using `read-string'.
COLLECTION is the name of the collection to which the new project shall belong.
The return value is a dotted pair with car the collection name and cdr the newly
obtained project name.
PROMPT is a string to prompt with; normally it ends in a colon and a space. The
string entered must be a valid nonexistent project name; i.e. it may only
contain letters, digits and any characters specified by
`perject-valid-naming-chars'. If there already exists a project within that
collection with the the specified name, an error is thrown."
  (let ((name (read-string prompt)))
    (mapc
	 (lambda (char)
       (or (and (>= char 48) (<= char 57)) ;; digit
           (and (>= char 65) (<= char 90)) ;; uppercase letter
           (and (>= char 97) (<= char 122)) ;; lowercase letter
           (member char perject-valid-naming-chars)
           (user-error
			"The character '%c' is not valid for naming a projectn. See the variable `perject-valid-naming-chars'."
			char )))
	 name)
	(when (string-empty-p name)
      (user-error "The project name cannot be empty."))
    (when (member name (mapcar #'cdr (perject--list-projects collection)))
      (user-error "There already is a project named '%s' within this collection." name))
    name))

(defun perject--list-collections (&optional scope)
  "Return a list containing the names of all collections.
Each collection is represented as a directory in `perject-directory'. If SCOPE
is 'active, only return the active collections; i.e. those which are currently
loaded. If SCOPE is 'inactive, return all collections that are not active at the
moment."
  (if (eq scope 'active)
	  (mapcar 'car perject-collections)
	(let ((col
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
		  (cl-set-difference col (perject--list-collections 'active) :test #'string-equal)
		col))))

(defun perject--list-projects (&optional collection)
  "Return a list containing the names of all projects belonging to COLLECTION.
More precisely, the list contains dotted pairs with car the respective
collection name and cdr the project name.
COLLECTION is a string denoting a collection. If COLLECTION is nil, use the
current collection if there is one. If there is no current collection or
COLLECTION is 'all, return the projects of all active collections."
  (cl-flet ((fun (list) (mapcar (apply-partially #'cons (car list)) (cdr list))))
	(let ((collection (or collection (car (perject--current)))))
	  (if (stringp collection)
		  (fun (assoc collection perject-collections))
		(seq-mapcat #'fun perject-collections)))))


;;;; Interface to desktop.el

(defun perject-desktop-load (name &optional no-msg)
  "Using `desktop-read' load the collection named NAME from the corresponding desktop file.
If the optional argument NO-MSG is non-nil, don't print any messages.
This function also adds NAME to the alist of active collections `perject-collections'."
  (let ((desktop-var-serdes-funs
		 (cons
		  (list 'perject-buffer
				nil
				(lambda (projects)
				  (append perject-buffer projects)))
		  desktop-var-serdes-funs))
		(desktop-load-locked-desktop t)
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
	(setq desktop-dirname (file-name-as-directory (perject--get-collection-dir name)))
	(desktop-read desktop-dirname)
	(push perject--desktop-current perject-collections))
  ;; Change the frame title of the newly restored frames, if desired.
  ;; The 'name' frame parameter in `frameset-filter-alist' is not restored by default.
  ;; While we could change that setting, this would also influence other times when the user
  ;; names the frame, so we instead just set the value when loading.
  (when perject-frame-title-format
	(dolist (frame perject--desktop-restored-frames)
	  (set-frame-parameter frame 'name
						   (funcall perject-frame-title-format (perject--current frame)))))
  (run-hook-with-args 'perject-desktop-after-load-hook perject--desktop-current))

(defun perject-desktop-save (name &optional release-lock no-msg)
  "Using `desktop-save' save the collection named NAME to the corresponding desktop file.
If the optional argument RELEASE-LOCK is non-nil, Emacs will release the lock of
the corresponding desktop file. If the optional argument NO-MSG is non-nil,
don't print any messages."
  (unless name
	(error "Cannot save an unnamed collection with `perject-desktop-save'"))
  (let ((perject--desktop-current (assoc name perject-collections))
		(desktop-globals-to-save
		 (cons 'perject--desktop-current desktop-globals-to-save))
		(desktop-var-serdes-funs
		 (cons
		  (list 'perject-buffer
				(lambda (projects)
				  (cl-remove-if-not (-compose (apply-partially #'string-equal name) #'car) projects))
				nil)
		  desktop-var-serdes-funs))
		;; Frames that do not belong to the project named NAME should not be
		;; saved, so we put them into a list and save the other frames.
		(ignored-frames
		 (cl-remove-if (apply-partially (-flip #'perject--is-assoc-with) name) (frame-list)))
		;; Only save those buffers belonging to the current project and respect
		;; the value of `perject-buffers-not-to-save-function'.
		(desktop-buffers-not-to-save-function
		 (if perject-buffers-not-to-save-function
			 (lambda (file-name buffer-name major-mode minor-modes)
			   (and (perject--is-assoc-with (get-buffer buffer-name) name)
					(funcall perject-buffers-not-to-save-function
							 name file-name buffer-name major-mode minor-modes)))
		   (lambda (_ buffer-name _ _)
			 (perject--is-assoc-with (get-buffer buffer-name) name))))
		;; Hack: Pretend the desktop file is from the same time, so that desktop does not
		;; complain that the desktop file is more recent than the one loaded.
		(desktop-file-modtime (file-attribute-modification-time
							   (file-attributes
								(desktop-full-file-name
								 (file-name-as-directory (perject--get-collection-dir name)))))))
	(run-hook-with-args 'perject-desktop-save-hook perject--desktop-current)
	(dolist (frame ignored-frames)
	  (set-frame-parameter frame 'desktop-dont-save t))
	(desktop-save (file-name-as-directory
				   (perject--get-collection-dir name))
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
		  (message "Perject: collection '%s'%s: %s%d buffer%s restored%s%s."
				   (car perject--desktop-current)
				   (when (cdr perject--desktop-current)
					 (concat " (" (string-join (cdr perject--desktop-current) ", ") ")"))
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

(provide 'perject)
;;; perject.el ends here
