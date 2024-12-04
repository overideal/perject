;;; perject.el --- Session-persistent project management -*- lexical-binding: t -*-

;; Copyright (C) 2022, 2023 overideal

;; Author: overideal
;; Maintainer: overideal
;; Version: 3.2
;; Package-Requires: ((emacs "27.1") (dash "2.10") (transient "0.3.7"))
;; Homepage: https://github.com/overideal/perject

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.


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
(require 'desktop)

;; Silence the byte compiler.
(defvar savehist-additional-variables)
(defvar desktop-buffer-ok-count)
(defvar desktop-buffer-fail-count)


;;;; Constants

(defconst perject-command-line-option "--perject"
  "Command line option which if present overwrites `perject-load-at-startup'.
For example, if Emacs is launched with \\='--perject \"project 1,project
2\"\\=', the projects \"project 1\" and \"project 2\" are loaded.
Write \\='--perject \"\"\\=' if no projects should be opened.")


;;;; Customization

(defgroup perject nil
  "Session-persistent project management."
  :group 'convenience
  :prefix "perject-")

(defcustom perject-directory
  (file-name-as-directory (concat user-emacs-directory "perject"))
  "The directory used by perject to save its data.
This in particular includes the desktop files."
  :type 'directory)

(defcustom perject-switch-to-new-collection 'new
  "Whether and how to switch to a newly created collection within `perject-open'.

It may have one of the following values:
- nil: Do not switch to the newly created collection.
- \\='new: Switch to the newly created collection in a new frame.
- t: Switch to the newly created collection within the selected frame."
  :type '(choice
		  (const :tag "Do not switch to the newly created collection" nil)
		  (const :tag "Switch to the newly created collection in a new frame" new)
		  (const :tag "Switch to the newly created collection within the selected frame" t)))

(defcustom perject-load-at-startup nil
  "The variable controls which collections are automatically loaded at startup.

It may have one of the following values:
- nil: Load no collection.
- \\='all: Load all collections.
- A list of collection names: Load the specified collections (if existent). If a
  name in the list does not correspond to an existing project, do nothing. As a
  special case, if the first element of the list is \\='not, all collections but
  the ones specified in the list are loaded.
- \\='previous: Load the collections that were open at the end of the previous
  session.
- A function: The function is called with two arguments, namely the list of
  collections opened in the previous Emacs session and the list of all
  collections. The function should return a list of collection names to be
  loaded. It could for example ask the user.

The order in which the collections are loaded is determined as follows: In the
list or function case, the collections are loaded \"from left to right\". If the
value is \\='previous, use the current order of the collections, which can be
changed using the command `perject-sort'.

In order for the \\='previous option to work properly, add the following line to
your Emacs configuration file:
\(add-to-list \\='savehist-additional-variables \\='perject--previous-collections)."
  :type '(choice
		  (const :tag "Load no collection" nil)
		  (const :tag "Load all collections" all)
		  (repeat :tag "Load the specified collections (if existent)"
				  (choice string (const not)))
		  (const :tag "Load previously open collections" previous)
		  (function :tag "Custom function"))
  :set (lambda (symbol value)
		 (set-default symbol value)
		 (when (eq value 'previous)
		   (savehist-mode 1)
		   (add-to-list 'savehist-additional-variables 'perject--previous-collections))))

(defcustom perject-save-on-exit 'all
  "The collections to automatically save when exiting Emacs.

It may have one of the following values:
- nil: Don't save any collections.
- \\='all: Save all active collections.
- \\='recent: Save all collections that were open in the previous Emacs session.
- A list of collection names: Save the specified collection (if active). As a
  special case, if the first element of the list is \\='not, all active collections
  but the ones specified in the list are saved upon exiting Emacs.
- A function: The function is called with two arguments, namely the list of
  collections opened in the previous Emacs session and the list of active
  collections. The function should return the list of collection names to be
  saved. It could for example ask the user."
  :type '(choice
		  (const :tag "Don't save any collections" nil)
		  (const :tag "Save all active collections" all)
		  (repeat :tag "Save the specified collections (if existent)"
				  (choice string (const not)))
		  (function :tag "Custom function")))

(defcustom perject-save-frames '(t)
  "Whether saving a collection will also save the frames associated with it.
The value of the variable must be a list, which is of the following form:
\(default (name . value) ...)

The first element serves as the default value. If it is nil, the frames
belonging to the collections are not saved to their desktop files. If it is
\\='ask, the user is asked whether the frames should be saved for every
collection. Otherwise, the frames of every collection are saved.
The remaining elements of the list define exceptions to this rule. Each of the
entries is a dotted pair with car the collection name and cdr one of the
possible values of the default value. The cdr overwrites the behavior of the
default for the collection specified by the car.

The variable `perject-frames-predicate' can be used to further filter which
frames of which collection are saved."
  :type '(cons
		  (choice
		   (const :tag "By default save the frames" t)
		   (const :tag "By default ask the user whether the frames should be saved" ask)
		   (const :tag "By default do not save the frames" nil))
		  (repeat
		   (cons
			string
			(choice
			 (const :tag "Save the frames for this collection" t)
			 (const :tag "Ask the user whether the frames should be saved for this collection" ask)
			 (const :tag "Do not save the frames for this collection" nil))))))

(defcustom perject-raise-and-focus-frame 'init
  "Whether perject should try to raise and focus a frame in certain situations.
This might or might not yield the desired result, depending on the window
manager used. If disabled (by modifying this variable), the user may want to
write a custom function to manually raise and focus a frame.

This variable may have one of the following values:
- nil: Do not perform any manual raising or focusing.
- \\='open: Raise and focus the frame first restored by `perject-open'.
- \\='init: Raise and focus the first frame of the collection that was last
  restored at startup.
- t: Raise and focus the frame in both `perject-open' and at
  startup."
  :type '(choice
		  (const :tag "Do not perform any manual raising or focusing" nil)
		  (const :tag "Raise and focus the frame first restored by
		  `perject-open'" open)
		  (const :tag "Raise and focus the first frame of the collection that
		  was last restored at startup" init)
		  (const :tag "Raise and focus the frame in both
		  `perject-open' and at startup" t)))

(defcustom perject-messages
  '(save add-buffer remove-buffer next-collection previous-collection next-project previous-project)
  "Whether to print informative messages when performing certain actions.

The value of this variable is a list which may contain any of the following
symbols, whose presence in the list leads to a message being printed in the
indicated command:
- \\='save: `perject-save'
- \\='add-buffer: `perject-add-buffer-to-project',
- \\='remove-buffer: `perject-remove-buffer-from-project',
- \\='switch: `perject-switch',
- \\='next-collection `perject-next-collection',
- \\='previous-collection `perject-previous-collection',
- \\='next-project: `perject-next-project',
- \\='previous-project: `perject-previous-project'."
  :type '(set
		  (const :tag "`perject-save'" save)
		  (const :tag "`perject-add-buffer-to-project'" add-buffer)
		  (const :tag "`perject-remove-buffer-from-project'" remove-buffer)
		  (const :tag "`perject-switch'" switch)
		  (const :tag "`perject-next-collection'" next-collection)
		  (const :tag "`perject-previous-collection'" previous-collection)
		  (const :tag "`perject-next-project'" next-project)
		  (const :tag "`perject-previous-project'." previous-project)))

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
  name is guaranteed to be a proper string but the project name could be nil.
  The function should return a string to be used as the frame title.
  It may also contain nil to decline naming the frame.
By default, the function `perject-frame-title' is used."
  :type '(choice
		  (const :tag "Don't alter the title of the frame" nil)
		  (function :tag "Custom function")))

(defcustom perject-reuse-starting-frame t
  "Whether perject should reuse the frame when loading collections at startup.
When starting Emacs, a single frame is produced. If this variable is non-nil,
that frame will be reused when the first collection that has at least one frame
is loaded. If this variable is nil, that frame is not altered."
  :type '(choice
		  (const :tag "Reuse original frame" t)
		  (const :tag "Don't reuse original frame" nil)))

(defcustom perject-global-vars-to-save nil
  "A list of global variables saved and restored by perject for every collection.
Every entry of the list may either be a global variable or a list with three
elements. In the first case, when a collection is loaded from its desktop file,
the global variable is set to the value specified in that desktop file. As such,
the value of the variable is overwritten. This means that the value is
determined by the last collection loaded.

An entry may alternatively be a a list with three elements:
\(var serializer deserializer).

The first entry is the variable to be saved. The second entry is a function
\(\"serializer\") that is called when a collection is saved to its desktop file.
The serializer is supplied with the current value of the variable and the name
of the collection being saved. Its return value is saved to the desktop file.
The deserializer is called when a collection is loaded from its desktop file.
Given the value from the desktop file and the name of the collection being
loaded, its return value serves as the new value of the variable.

The deserializer should take into account the fact that the variable might
already have a value (e.g. if another collection was already loaded before the
current one)."
  :type '(repeat
		  (choice (list variable function function)
				  variable)))

(defcustom perject-local-vars-to-save
  (cons '(mark-ring perject--serialize-mark-ring perject--deserialize-mark-ring)
		desktop-locals-to-save)
  "A list of global variables saved and restored by perject for every collection.
The entries of the list are of the same form as those in
`perject-global-vars-to-save', which see."
  :type '(repeat
		  (choice (list variable function function)
				  variable)))

(defcustom perject-buffers-predicate nil
  "Function identifying buffers not to save to the desktop file of a collection.
By default, when saving a collection, all buffers belonging to the collection
are saved. This variable can be used to only save a subset of those buffers. If
this variable is nil, no additional filtering takes place.
The function is called with five arguments: the collection name, the name of the
visited file, the buffer name, the major mode and the list of active minor
modes. It must return nil if the buffer should not be saved.
For convenience, the buffer list can be further filtered using the variables
`desktop-buffers-not-to-save', `desktop-files-not-to-save' and
`desktop-modes-not-to-save'."
  :type '(choice function (const nil)))

(defcustom perject-frames-predicate nil
  "Function identifying frames not to save to the desktop file of a collection.
Depending on the value of `perject-save-frames', when saving a collection,
either all frames belonging to the collection are saved or no frames are saved.
This variable can be used to only save a subset of those frames. If this
variable is nil, no additional filtering takes place.
The function is called with the collection name and a frame and it must return
nil if the frame should not be saved."
  :type '(choice function (const nil)))

(defcustom perject-auto-add-function nil
  "Function controlling which buffers are automatically associated with projects.
When a hook in `perject-auto-add-hooks' runs, this function is called in order
to decide to which projects the current buffer should be added to.
It is called with two arguments. The first argument is the current buffer. The
second is a cons cell with car a collection name and cdr a project name. This
might be nil or the project name could be nil.
The function should return a list of projects to which the buffer should be
added. By returning nil (the empty list) the buffer is not added to any project.
Setting this variable to nil means always add the buffer to the current project.
Any entries in the returned list that do not correspond to existing projects are
ignored.

This variable is intended to allow for advanced customization and thus for the
majority of use cases, the value nil should suffice."
  :type '(choice function (const nil)))

(defcustom perject-auto-add-hooks
  '(find-file-hook clone-indirect-buffer-hook dired-mode-hook occur-mode-hook
				   help-mode-hook Info-mode-hook org-src-mode-hook)
  "A list of hooks in which the current buffer is added to the current project.
This is used to automatically add buffers to projects.
The following hooks could be interesting to the user: `find-file-hook',
`clone-indirect-buffer-hook', `buffer-list-update-hook',
`after-change-major-mode-hook', `window-selection-change-functions' and many
mode hooks.
Modifcations of this variable only take effect after (re)enabling
`perject-mode'.
By default, the current buffer is added to the current project. The variable
`perject-auto-add-function' can be used to tweak this behavior."
  :type '(repeat variable))

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
This means that all the collections that were configured to load automatically
have been restored from their desktop files. In particular, all the respective
buffers and frames have been loaded.
The functions are called with one argument, namely the list of collection names
that were restored. This list is ordered (the leftmost collection was restored
first)."
  :type 'hook)

(defcustom perject-before-switch-hook nil
  "Hook run before perject has switched project or collection.
This affects the command `perject-switch'. The functions are called with three
arguments: the original collection or project, the collection or project that
will be switched to and the frame."
  :type 'hook)

(defcustom perject-after-switch-hook nil
  "Hook run after perject has switched project or collection.
This affects the command `perject-switch'. The functions are called with three
arguments: the original collection or project, the newly switched to collection
or project and the frame."
  :type 'hook)

(defcustom perject-before-open-hook nil
  "Hook run before perject opens a collection using `perject-open'.
In particular, the projects, buffers and frames belonging to the collection have
not yet been restored.
The functions are called with one argument, namely the name of the collection to
be opened."
  :type 'hook)

(defcustom perject-after-open-hook nil
  "Hook run after perject has opened a collection using `perject-open'.
In particular, the projects, buffers and frames belonging to the collection have
been restored.
The functions are called with one argument, namely the name of the newly opened
project."
  :type 'hook)

(defcustom perject-before-create-hook nil
  "Hook run before perject creates a new collection or project'.
This influences the commands `perject-switch' and `perject-open'.
The functions are called with one argument, which is either a string
representing the name of the new collection or a dotted pair representing the
new project.
Note that if a new project is created that belongs to a new collection, this
hook is run twice: first before creating the collection and another time before
creating the project."
  :type 'hook)

(defcustom perject-after-create-hook nil
  "Hook run after perject has created a new collection or project.
This influences the commands `perject-switch' and `perject-open'.
The functions are called with one argument, which is either a string
representing the name of the new collection or a dotted pair representing the
new project.
Note that if a new project is created that belongs to a new collection, this
hook is run twice: first after creating the collection and another time after
creating the project."
  :type 'hook)

(defcustom perject-before-close-hook nil
  "Hook run before perject closes a collection using `perject-close'.
The functions are called with three arguments, namely the name of the collection
to be closed, the list of all frames that belong to the collection or some
project of it and a list containing all buffers that are associated with some
project of this collection."
  :type 'hook)

(defcustom perject-after-close-hook nil
  "Hook run after perject has closed a collection using `perject-close'.
The functions are called with three arguments, namely the name of the closed
collection, a list of all frames that belonged to the collection or some project
of it and have not been killed and a list containing all buffers that were
associated with some project of this collection and that have not been killed."
  :type 'hook)

(defcustom perject-before-reload-hook nil
  "Hook run before perject reloads a collection using `perject-reload'.
The functions are called with three arguments, namely the name of the collection
to be reloaded, a list of all frames that belong to the collection or some
project of it and have not been killed and a list containing all buffers that
are associated with some project of this collection."
  :type 'hook)

(defcustom perject-after-reload-hook nil
  "Hook run after perject has reloaded a collection using `perject-reload'.
The functions are called with three arguments, namely the name of the reloaded
collection, a list of all frames that belonged to the collection before
reloading and have not been killed and the list of buffers that were associated
with some project of the collection before reloading and have not been killed."
  :type 'hook)

(defcustom perject-rename-hook nil
  "Hook run after perject has renamed a collection or project.
This affects the command `perject-rename'. The functions are called with two
arguments, namely the old collection or project and the new one. The arguments
are either both collection names or both dotted pairs representing projects."
  :type 'hook)

(defcustom perject-before-delete-collection-hook nil
  "Hook run before perject deletes a collection using `perject-delete'.
The functions are called with one argument, namely the name of the collection to
be deleted."
  :type 'hook)

(defcustom perject-after-delete-collection-hook nil
  "Hook run after perject has deleted a collection using `perject-delete'.
The functions are called with one argument, namely the name of the deleted
collection."
  :type 'hook)

(defcustom perject-before-delete-project-hook nil
  "Hook run before perject deletes a project using `perject-delete'.
The functions are called with three arguments, namely the name of the project to
be deleted, a list of frames that currently display this project and a list
containing all buffers that are currently associated with this project."
  :type 'hook)

(defcustom perject-after-delete-project-hook nil
  "Hook run after perject has deleted a project using `perject-delete'.
The functions are called with three arguments, namely the name of the deleted
project, a list of frames that were associated with this project and have not
been killed and a list containing all buffers that were associated with this
project and that have not been killed."
  :type 'hook)

(defcustom perject-desktop-after-load-hook nil
  "Hook run after perject has loaded a collection from its desktop file.
The functions are called with the name of the collection as their only
argument."
  :type 'hook)

(defcustom perject-desktop-save-hook nil
  "Hook run before perject has saved a collection to its desktop file.
The functions are called with the name of the collection as their only
argument."
  :type 'hook)


;;;; Faces

(defgroup perject-faces nil
  "Faces used by perject."
  :group 'perject
  :group 'faces)

(defface perject-mode-line-face '((t :foreground "dark orange"))
  "The face used by the mode line indicator of perject.")

(defface perject-project-annotator-main '((t :inherit font-lock-keyword-face))
  "The face used for displaying the main annotation when selecting a project.")

(defface perject-project-annotator-buffers '((t :inherit completions-annotations))
  "The face used for displaying the number of buffers.
This has an effect when selecting a collection or project.")

(defface perject-project-annotator-tabs '((t :inherit completions-annotations))
  "The face used for displaying the number of tabs.
This has an effect when selecting a collection or project and only if
`perject-tab' is loaded.")

(defface perject-project-annotator-frames '((t :inherit completions-annotations))
  "The face used for displaying the number of frames.
This has an effect when selecting a collection or project.")


;;;; Internal Variables

(defvar perject-mode-line-current
  `(:propertize
	(:eval (if (or (not (perject-current)) (perject-is-assoc-with (current-buffer) (perject-current)))
			   "-" "*"))
	mouse-face mode-line-highlight
	local-map ,(make-mode-line-mouse-map
						 'mouse-1
						 #'perject-mode-line-toggle-current-buffer)
	help-echo
	 (lambda (window _object _point)
	   (with-selected-window window
		 (let ((proj (perject-current)))
		   (if (not (car proj))
			   "No current project"
			 (format "Current buffer is%s associated with current project '%s'\nmouse-1: Toggle"
					 (if (perject-is-assoc-with (current-buffer) proj) "" " not")
					 (perject-project-to-string proj)))))))
  "Mode line construct to indicate the relation between buffer and project.
It displays a dash (-) if there is no current project or if the current buffer
belongs to the current project and an asterix (*) otherwise.")

(put 'perject-mode-line-current 'risky-local-variable t)

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

;; I am surprised such a variable does not already exist in `frameset.el'
(defvar perject--desktop-restored-frames nil
  "The list of the frames which were restored for the most recent collection.
Should not be modified by the user.")

(defvar perject--desktop-reuse-frames nil
  "Internal parameter controlling whether frames are reused.
Its value may be just like the parameter REUSE-FRAMES of `frameset-restore'.
Should not be modified by the user.")

(defvar perject--previous-collections nil
  "Internal variable that stores the collections opened in the last session.
Should not be modified by the user.")


;;;; The Mode

;;;###autoload
(define-minor-mode perject-mode
  "Group buffers and frames into projects which are preserved when restarting."
  :global t
  :group 'perject
  :keymap (make-sparse-keymap)
  (if perject-mode
      (progn
		;; Create main directory if not already existent.
		(unless (file-exists-p perject-directory)
		  (make-directory perject-directory))

        (add-hook 'after-init-hook #'perject--init)
        (add-hook 'kill-emacs-hook #'perject--exit)

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
							 (car (perject-current)) (cdr (perject-current)))))
                (cdr (last mode-line-misc-info)))))

	;; Remove the added hooks.
    (remove-hook 'after-init-hook #'perject--init)
    (remove-hook 'kill-emacs-hook #'perject--exit)
    (dolist (hook perject-auto-add-hooks)
      (remove-hook hook #'perject--auto-add-buffer))
	;; Reset the frame title, but keep the information about which frame belongs
	;; to which project, since that information might still be useful if the
	;; mode is enabled again.
	(when perject-frame-title-format
	  (dolist (frame (frame-list))
		(when (perject-current frame)
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
  (concat invocation-name "@" (system-name) ":" (car perj)
		  (when (cdr perj)
			(concat "|" (cdr perj)))))

(defun perject--init ()
  "Load collections from the last session and set up hooks.
The collections are stored in desktop files. The variable
`perject-load-at-startup' determines which collections are loaded automatically.
However, if specified, the command line option `perject-command-line-option'
takes priority.

After starting Emacs there is a single frame; namely the selected \"starting
frame\". It will be reused as the frame of a newly opened collection if
`perject-reuse-starting-frame' is non-nil.
Whether the first frame of the collection that was last restored will be
selected and focused is determined by `perject-raise-and-focus-frame'."
  (let* ((current-frame (selected-frame))
         (all-collections (perject-get-collections))
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
	;; Read the command line arguments and filter non-existent collections.
	;; We cannot use `command-switch-alist' since those functions are processed after `after-init-hook'.
	(if-let ((index (cl-position perject-command-line-option command-line-args :test #'string-equal))
			 (list (split-string (nth (1+ index) command-line-args) ","))
			 (cols (-separate #'perject-collection-p list)))
		(progn
		  ;; Warn about the non-existent collections, but ignore the empty
		  ;; string, which e.g. occurs when calling Emacs with --perject "" to
		  ;; load no collections
		  (when-let ((no-cols (delete "" (cadr cols))))
			(warn "Perject: The following collections do not exist: %s" (string-join no-cols ", ")))
		  (setq cols-to-load (car cols)
				command-line-args
				(append (seq-take command-line-args index) (seq-drop command-line-args (+ index 2)))))
	  (let ((cols (-separate #'perject-collection-p cols-to-load)))
		(when-let ((no-cols (cadr cols)))
		  (warn "Perject: The following collections do not exist: %s" (string-join no-cols ", "))
		  (setq cols-to-load (car cols)))))
	;; If `perject-reuse-starting-frame' is non-nil, we may reuse the "starting
	;; frame" for a collection, but as soon as it is "claimed" (or somehow
	;; deleted), it cannot be used again.
	;; This behavior is obtained by setting `perject--desktop-reuse-frames'.
	(let ((perject--desktop-reuse-frames (lambda (frame)
										   (and
											(eq frame current-frame)
											(not (perject-current frame)))))
		  (starting-frame-claimed (not perject-reuse-starting-frame))
		  perject-raise-and-focus-frame)
      (dolist (name cols-to-load)
		(when (or (member current-frame perject--desktop-restored-frames)
				  (not (frame-live-p current-frame)))
		  (setq starting-frame-claimed t))
		(if starting-frame-claimed
			;; Starting frame was claimed.
			(let (perject-before-open-hook perject-after-open-hook)
			  (perject-open name))
		  (perject-desktop-load name))))
    ;; Add the hooks specified in `perject-auto-add-hooks'.
    (dolist (hook perject-auto-add-hooks)
      (add-hook hook 'perject--auto-add-buffer))
	;; Select the first restored frame of the collection that was last restored.
	(when-let (((memq perject-raise-and-focus-frame '(t init)))
			   (frames (car (last (remove 'nil (mapcar #'perject-get-frames cols-to-load))))))
	  (select-frame-set-input-focus (car frames)))
    (run-hook-with-args 'perject-after-init-hook cols-to-load)))

(defun perject--exit ()
  "Save collections to be restored next time and prepare to exit Emacs.
The variabe `perject-save-on-exit' determines which collections are saved.
This function is called by `perject-mode' before exiting Emacs (using
`kill-emacs-hook')."
  (let* ((cols (perject-get-collections 'active))
		 (cols-to-save
		  (pcase perject-save-on-exit
			('all cols)
			((pred listp) (if (eq (car perject-save-on-exit) 'not)
							  (cl-set-difference cols (cdr perject-save-on-exit)
												 :test #'string-equal)
							perject-save-on-exit))
			('recent perject--previous-collections)
			((pred functionp) (funcall perject-save-on-exit
									   perject--previous-collections
									   cols)))))
	(perject-save cols-to-save t (memq 'save perject-messages))
	;; Reverse the list of active collections, so that when restoring the
	;; collections in the next section the order of the resulting list is the same
	;; as before.
	(setq perject--previous-collections (nreverse cols))))


;;;; Creating, Renaming and Deleting

(defun perject-create (proj)
  "Create a new collection or project PROJ.
PROJ may be a collection or a project represented a dotted pair with car a
collection and cdr a project name."
  (run-hook-with-args 'perject-before-create-hook proj)
  (if (stringp proj)
	  (progn
		(make-directory (perject-get-collection-dir proj) t)
		;; The order of the collections matters and we want the new collection to
		;; be at the rightmost position.
		(setq perject-collections (append perject-collections (list (list proj))))
		;; Create a desktop file.
		(perject-save proj))
	;; If the corresponding collection does not exist yet, create it.
	(unless (perject-collection-p (car proj))
	  (perject-create (car proj)))
	(setcdr
	 (assoc (car proj) perject-collections)
	 (append (alist-get (car proj) perject-collections nil nil #'string-equal)
			 (list (cdr proj)))))
  (run-hook-with-args 'perject-after-create-hook proj))

(defun perject-create-new-frame (&optional proj)
  "Create a new frame for the project PROJ and select it.
PROJ may be a collection name or a dotted pair with car a collection and cdr a
project name. It may also be nil in which case this command behaves like
`make-frame-command'.
In interactive use, PROJ defaults to the current project.
If there is no current project or if a single prefix argument is supplied, the
user may select PROJ from the list of all projects. In any other case, the user
can choose from the list of collections."
  (interactive
   (list
	(or (and (not current-prefix-arg) (perject-current))
		(if (equal current-prefix-arg '(4))
			(perject--get-project-name
			 "Create new frame for project: "
			 (if (equal current-prefix-arg '(4)) 'all 'current)
			 nil t (perject-current)
			 "No project exists" "No project specified")
		  (perject--get-collection-name
		   "Create new frame for collection: "
		   'active nil t nil "No collection exists" #'ignore)))))
  (perject-switch proj (make-frame-command)))

(defun perject-rename (proj new-proj)
  "Rename the collection or project PROJ to NEW-PROJ.
PROJ and NEW-PROJ may either both be collection names or projects represented by
dotted pairs.

In interactive use, the behavior is dependent on the number of prefix arguments.
Without a prefix argument, the user may select a project from the list of all
projects to rename. With a single prefix argument, the user may also select a
collection to move PROJ into. In any other case, the user may select a
collection to rename.

This function runs the hook `perject-rename-hook'."
  (interactive
   (let ((proj
		  (if (or (not current-prefix-arg) (equal current-prefix-arg '(4)))
			  (perject--get-project-name
			   "Select project to rename: " 'all nil t (perject-current)
			   "There currently is no project to rename"
			   "No project specified")
			(perject--get-collection-name
			 "Select collection to rename: " 'active nil t (car (perject-current))
			 "There currently is no collection to rename"
			 "No collection specified"))))
	 (list
	  proj
	  (if (or (not current-prefix-arg) (equal current-prefix-arg '(4)))
		  (let ((col
				 (if current-prefix-arg
					 (perject--get-collection-name
					  "Select a collection to move the project to: " 'active nil t
					  (car (perject-current)) nil "No collection specified")
				   (car proj))))
			(cons col (perject--get-new-project-name
					   col (format "New name of project '%s' in collection '%s': " (cdr proj) col))))
		(perject--get-new-collection-name (format "New name of collection '%s': " proj))))))
  (if (stringp proj)
	  (perject-rename-collection proj new-proj)
	(perject-rename-project proj new-proj))
  (run-hook-with-args 'perject-rename-hook proj new-proj)
  (force-mode-line-update t))

(defun perject-rename-collection (old-name new-name)
  "Rename the collection named OLD-NAME to NEW-NAME."
  (dolist (frame (frame-list))
	(when (perject-is-assoc-with frame old-name)
	  (perject-set-current (cons new-name (cdr (perject-current frame))) frame)))
  (setcar (assoc old-name perject-collections) new-name)
  (dolist (buffer (buffer-list))
	(with-current-buffer buffer
	  (setq perject-buffer
			(mapcar (lambda (proj)
					  (if (string-equal (car proj) old-name)
						  (cons new-name (cdr proj))
						proj))
					perject-buffer))))
  (when (file-exists-p (perject-get-collection-dir old-name))
	(rename-file (perject-get-collection-dir old-name)
				 (perject-get-collection-dir new-name))))

(defun perject-rename-project (proj new-proj)
  "Rename the project PROJ to NEW-PROJ.
PROJ and NEW-PROJ are both dotted pairs with car a collection and cdr a project
name. NEW-PROJ may also be a string to be used as the new project name within
the same collection."
  (let* ((old-col (car proj))
		 (old-name (cdr proj))
		 (new-col (if (stringp new-proj) old-col (car new-proj)))
		 (new-name (if (stringp new-proj) new-proj (cdr new-proj)))
		 (new-proj (cons new-col new-name))
		 (old-col-list (assoc old-col perject-collections)))
	(dolist (frame (frame-list))
	  (when (perject-is-assoc-with frame proj)
		(perject-set-current new-proj frame)))
	(if (string-equal old-col new-col)
		(setcdr old-col-list (cl-substitute new-name old-name (cdr old-col-list) :test #'equal))
	  (setcdr old-col-list (delete old-name (cdr old-col-list)))
	  (push new-name (alist-get new-col perject-collections nil nil #'string-equal)))
	(dolist (buffer (perject-get-buffers proj))
	  (with-current-buffer buffer
		(setq perject-buffer (cl-substitute new-proj proj perject-buffer :test #'equal))))))

(defun perject-delete-collection (name kill-frames kill-buffers)
  "Delete the collection named NAME.
Deleting a collection encompasses closing the collection (if active) and
deleting all its projects and the corresponding desktop file.
KILL-FRAMES and KILL-BUFFERS are interpreted as described in the variables
`perject-delete-default' (except that any functions must have been replaced with
their return values).

This function runs the hooks `perject-before-delete-collection-hook' and
`perject-after-delete-collection-hook'."
	(run-hook-with-args 'perject-before-delete-collection-hook name)
    ;; If the collection is active, close it.
    (when (perject-collection-p name 'active)
      (perject-close-collection name nil kill-frames kill-buffers))
    (when (file-exists-p (perject-get-collection-dir name))
      (delete-directory (perject-get-collection-dir name) t))
	(run-hook-with-args 'perject-after-delete-collection-hook name)
	(message "Deleted collection '%s'" name))

(defun perject-delete-project (proj kill-frames kill-buffers)
  "Delete the project PROJ.
KILL-FRAMES and KILL-BUFFERS are interpreted as described in the variables
`perject-delete-default' (except that any functions must have been replaced with
their return values).

This function runs the hooks `perject-before-delete-project-hook' and
`perject-after-delete-project-hook'."
  (run-hook-with-args 'perject-before-delete-project-hook
					  proj (perject-get-frames proj) (perject-get-buffers proj))
  (let ((buffers (perject-get-buffers proj))
		(frames (perject-get-frames proj))
		buffers-to-kill)
	;; Remove the project from the list of active collections.
	(let ((col (assoc (car proj) perject-collections)))
	  (setcdr col (delete (cdr proj) (cdr col))))
	;; Deal with the buffers belonging to the project.
	(dolist (buffer buffers)
	  (with-current-buffer buffer
		(setq perject-buffer (delete proj perject-buffer))
		(unless (or perject-buffer (eq kill-buffers 'all) (not kill-buffers))
		  (push buffer buffers-to-kill))))
	(when (eq kill-buffers 'all)
	  (setq buffers-to-kill buffers))
	;; Deal with the frames belonging to the project.
	(let ((delete-frames (and kill-frames
							  (not (eq kill-frames 'keep))
							  (not (eq (length frames) (length (frame-list)))))))
	  (dolist (frame frames)
		(if delete-frames
			(delete-frame frame)
		  (perject-set-current (and (eq kill-frames 'keep) (car proj)) frame)))
	  (when delete-frames (setq frames nil)))
	(dolist (buffer buffers-to-kill)
	  (kill-buffer buffer))
	(setq buffers (cl-set-difference buffers buffers-to-kill))
	(run-hook-with-args 'perject-after-delete-project-hook proj frames buffers)
	(message "Deleted project '%s'" (perject-project-to-string proj))))


;;;; Opening, Closing and Saving

;; The `perject-open' command is very subtle, due to the following:
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
(defun perject-open (name)
  "Create or open the collection named NAME.
This means that all projects that belonged to the collection are restored from
the corresponding desktop file. Furthermore, this function switches to one of
the restored frames (if any). Depending on the value of
`perject-raise-and-focus-frame', the first restored frame will be raised and
focused.
If there is no collection named NAME yet, create it and switch to it as
specified by `perject-switch-to-new-collection'.
Before creating the collection, run `perject-before-create-hook' and afterwards
run `perject-after-create-hook'.
If no new collection is created, run the hooks `perject-before-open-hook' and
`perject-after-open-hook' instead.
In interactive use, the user is asked for the collection name."
  (interactive
   (list
	(perject--get-collection-name "Open collection (or create new one): "
								  'inactive nil nil nil nil
								  "No collection specified")))
  (when (perject-collection-p name 'active)
	(user-error "The collection '%s' is already open" name))
  (if (perject-collection-p name)
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
		(let* ((frame (selected-frame))
			   (wc (current-window-configuration))
			   (desktop-after-read-hook
				(cons (lambda ()
						(unless (memq frame perject--desktop-restored-frames)
						  (set-window-configuration wc t)))
					  desktop-after-read-hook)))
		  (perject-desktop-load name))
		(dolist (hook perject-auto-add-hooks)
		  (add-hook hook #'perject--auto-add-buffer))
		(when (and perject--desktop-restored-frames
				   (memq perject-raise-and-focus-frame '(t open)))
		  (select-frame-set-input-focus (car perject--desktop-restored-frames)))
		(run-hook-with-args 'perject-after-open-hook name))
	(perject-create name)
	(pcase perject-switch-to-new-collection
	  ('new (perject-create-new-frame name))
	  ('t (perject-switch name)))
	(message "Created collection '%s'" name)))

(defun perject-open-in-new-instance (name)
  "Open a new Emacs instance for the collection named NAME.
This means that a new Emacs process is created and in it, the collection is
loaded. In interactive use, the user is asked for the collection name."
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

(defun perject-close-collection (name save kill-frames kill-buffers)
  "Close the collection named NAME.
This closes all projects belonging to the collection.
The three arguments SAVE, KILL-FRAMES and KILL-BUFFERS are interpreted as
described in the variable `perject-close-default' (except that any functions
must have been replaced with their return values).

This function runs the hooks `perject-before-close-hook' and
`perject-after-close-hook'."
  (run-hook-with-args 'perject-before-close-hook name
					  (perject-get-buffers name) (perject-get-frames name))
  ;; Recompute the buffers and frames after the hook, in case e.g. the hook
  ;; function killed any frames or buffers.
  (let ((buffers (perject-get-buffers name))
		(frames (perject-get-frames name))
		buffers-to-kill)
    (if save
		(perject-save name t)
	  ;; If we don't save, we need to manually remove the lock file.
	  (desktop-release-lock (file-name-as-directory (perject-get-collection-dir name))))
	;; Remove the collection from the list of active collections.
	(setq perject-collections (assoc-delete-all name perject-collections))
	;; Deal with the buffers belonging to the collection.
	(dolist (buffer buffers)
	  (with-current-buffer buffer
		(setq perject-buffer
			  (cl-delete-if (-compose (apply-partially #'string-equal name) #'car)
							perject-buffer))
		(unless (or perject-buffer (eq kill-buffers 'all) (not kill-buffers))
		  (push buffer buffers-to-kill))))
	(when (eq kill-buffers 'all)
	  (setq buffers-to-kill buffers))
	;; Deal with the frames belonging to the collection.
	(let ((kill-frames (and kill-frames
							(not (eq (length frames) (length (frame-list)))))))
	  (dolist (frame frames)
		(if kill-frames
			(delete-frame frame)
		  (perject-set-current nil frame)))
	  (when kill-frames (setq frames nil)))
	(dolist (buffer buffers-to-kill)
	  (kill-buffer buffer))
	(setq buffers (cl-set-difference buffers buffers-to-kill))
    (run-hook-with-args 'perject-after-close-hook name frames buffers)
	(message "Perject: Closed collection '%s'" name)))

(defun perject-reload-collection (name kill-frames kill-buffers)
  "Reload the collection named NAME from its desktop file.
This discards any changes to the collection and reverts it to the state from the
previous save. This is achieved by closing and reopening the collection.
Frames belonging to the collection are reused.
KILL-FRAMES and KILL-BUFFERS are interpreted as described in the variables
`perject-reload-default' (except that any functions must have been replaced with
their return values).

This function runs the hooks `perject-before-reload-hook' and
`perject-after-reload-hook'."
  (run-hook-with-args 'perject-before-reload-hook name
					  (perject-get-frames name) (perject-get-buffers name))
  ;; Allow reusing frames that belong to the collection.
  (let* ((frames (perject-get-frames name))
		 ;; Remember which frames belong to which project.
		 (project-frames (when (eq kill-frames 'keep)
							(-annotate #'perject-current frames)))
		 (buffers (perject-get-buffers name))
		 (perject--desktop-reuse-frames
		  (apply-partially (-flip #'member) frames))
		 unused-frames)
	(perject-close-collection name nil nil kill-buffers)
	(perject-open name)
	(setq buffers (cl-remove-if-not #'buffer-live-p buffers)
		  unused-frames (cl-set-difference frames perject--desktop-restored-frames))
	;; Deal with the frames that were not reused.
	;; They currently belong to no collection because of `perject-close-collection'.
	(when kill-frames
	  (let ((delete-frames (and (not (eq kill-frames 'keep))
								(not (eq (length unused-frames) (length (frame-list)))))))
		(dolist (frame unused-frames)
		  (if delete-frames
			  (delete-frame frame)
			(perject-switch (car (rassq frame project-frames)) frame)))))
	(setq frames (cl-remove-if-not #'frame-live-p frames))
	(run-hook-with-args 'perject-after-reload-hook name frames buffers)))

(defun perject-save (name &optional release-lock msg)
  "Save the collection named NAME.
NAME may also be a list of collections, in which case every collection in the
list is saved.
In interactive use, no prefix argument saves the current collection. With one
prefix argument, the user may manually select a collection to save and any other
prefix argument saves all collections.
If the optional argument RELEASE-LOCK is non-nil, Emacs will release the lock of
the corresponding desktop file. This argument is nil in interactive use.
If the optional argument MSG is non-nil, print a message. In interactive use,
this depends on the value of `perject-messages'."
  (interactive
   (list
	(cond ((or (not (perject-current)) (equal current-prefix-arg '(4)))
		   (perject--get-collection-name
			"Save collection: " 'active nil t (car (perject-current))
			"No collection to save" "No collection specified"))
		  (current-prefix-arg (perject-get-collections 'active))
		  (t (car (perject-current))))
	nil (memq 'save perject-messages)))
  (cond
   ((stringp name) (perject-desktop-save name release-lock msg))
   ((eq (length name) 1) (perject-desktop-save (car name) release-lock msg))
   (t (dolist (col name)
		(perject-desktop-save col release-lock nil))
	  (when (and name msg)
		(message "Perject: Saved collections: %s" (string-join name ", "))))))


;;;; Switching

(defun perject-switch (proj &optional frame msg)
  "Switch to PROJ in the frame FRAME.
PROJ may either be a collection or a project. The latter is represented by a
dotted pair with car a collection and cdr a project name. PROJ may also be nil,
in that case, remove the current collection and project (if any) from the frame,
so that it does not belong to any collection anymore. If MSG is non-nil, also
print a message.
If the specified project or collection does not exist, create it.

In interactive use, the current frame is used, the user is asked for PROJ
and MSG is determined by the variable `perject-messages'. Without a prefix
argument, the user may specify a project from the current collection. If there
is no current collection or if a single prefix argument was supplied, let the
user switch to an arbitrary project. In any other case, the user may select a
collection to switch to.

This function runs the hooks `perject-before-switch-hook' and
`perject-after-switch-hook'. If a new collection or
project was created, it furthermore runs the hooks `perject-before-create-hook'
and `perject-after-create-hook'."
  (interactive
   (list
	(cond
	 ((and current-prefix-arg (not (equal current-prefix-arg '(4))))
	  (perject--get-collection-name
	   "Switch to collection (or create new one): " 'active
	   ;; If we already have a collection focused, do not include it in the list.
	   (unless (cdr (perject-current))
		 (-compose #'not (apply-partially #'perject-is-assoc-with (selected-frame))))
	   nil nil nil #'ignore))
	 ((and (not current-prefix-arg) (perject-current))
	  (let ((proj
			 (perject--get-project-name
			  "Switch to project (or create new one): " 'current
			  (-compose #'not (apply-partially #'perject-is-assoc-with (selected-frame)))
			  nil nil nil #'ignore)))
		(pcase proj
		  ('nil (car (perject-current)))
		  ((pred stringp) (cons (car (perject-current)) proj))
		  (_ proj))))
	 (t
	  (perject--get-project-name
	   "Switch to project: " 'all
	   (-compose #'not (apply-partially #'perject-is-assoc-with (selected-frame)))
	   t nil "No project to switch to" "No project specified")))
	(selected-frame)
	(memq 'switch perject-messages)))

  (let ((proj (if (and (consp proj) (not (cdr proj))) (car proj) proj))
		(frame (or frame (selected-frame)))
		(old-proj (if (cdr (perject-current frame)) (perject-current frame)
					(car (perject-current frame)))))
	(run-hook-with-args 'perject-before-switch-hook old-proj proj frame)
	;; Create the project or collection if not already existent.
	(when (or (and (stringp proj) (not (perject-collection-p proj)))
			  (and (consp proj) (not (perject-project-p proj))))
		(perject-create proj))
	(perject-set-current proj frame)
	(when msg
	  (let ((is-current (equal frame (selected-frame))))
		(cl-flet ((fun (proj)
					(if (stringp proj)
						(concat "collection '" proj "'")
					  (concat "project '" (perject-project-to-string proj) "'"))))
		  (cond
		   ((and proj old-proj)
			(message "Switched from %s to %s in %sframe"
					 (fun old-proj) (fun proj) (if is-current "current " "")))
		   (proj
			(message "Switched to %s in %sframe" (fun proj) (if is-current "current " "")))
		   (old-proj
			(message "Removed %sframe from %s" (if is-current "current " "") (fun old-proj)))
		   (t
			(message "%s is not associated with a collection"
					 (if is-current "Current frame" "Frame")))))))
	(run-hook-with-args 'perject-after-switch-hook old-proj proj frame)
	(force-mode-line-update t)))

(defun perject-next-collection (&optional msg)
  "Switch to a project of the next active collection.
If the next collection has no projects, just switch to that collection. If there
are no active collections, throw an error. If the optional argument MSG is
non-nil, also print an informative message.
In interactive use, this is determined by `perject-messages'.

This function runs the hooks `perject-before-switch-hook' and
`perject-after-switch-hook'."
  (interactive (list (memq 'next-collection perject-messages)))
  (let ((collections (perject-get-collections 'active))
		(current (car (perject-current))))
	(unless collections
	  (user-error "There currently are no collections"))
	(let* ((index (or (and current (cl-position current collections :test #'string-equal)) 0))
		   (col (nth (mod (1+ index) (length collections)) collections))
		   (target (if-let ((projects (perject-get-projects col)))
					   (car projects) col)))
	  (perject-switch target nil msg))))

(defun perject-previous-collection (&optional msg)
  "Switch to a project of the previous active collection.
If the previous collection has no projects, just switch to that collection. If
there are no active collections, throw an error. If the optional argument MSG is
non-nil, also print an informative message.
In interactive use, this is determined by `perject-messages'.

This function runs the hooks `perject-before-switch-hook' and
`perject-after-switch-hook'."
  (interactive (list (memq 'previous-collection perject-messages)))
  (let ((collections (perject-get-collections 'active))
		(current (car (perject-current))))
	(unless collections
	  (user-error "There currently are no collections"))
	(let* ((index (or (and current (cl-position current collections :test #'string-equal)) 0))
		   (col (nth (mod (1- index) (length collections)) collections))
		   (target (if-let ((projects (perject-get-projects col)))
					   (car projects) col)))
	  (perject-switch target nil msg))))

(defun perject-next-project (&optional msg)
  "Switch to the next project within the current collection.
If there is no current collection, throw an error. If the optional argument MSG
is non-nil, also print an informative message.
In interactive use, this is determined by `perject-messages'.

This function runs the hooks `perject-before-switch-hook' and
`perject-after-switch-hook'."
  (interactive (list (memq 'next-project perject-messages)))
  (let ((projects (alist-get (perject-assert-collection) perject-collections nil nil #'string-equal))
		(current (cdr (perject-current))))
	(unless projects
	  (user-error "The current collection has no associated projects"))
	(let* ((index (or (and current (cl-position current projects :test #'string-equal)) 0))
		   (proj (cons (car (perject-current)) (nth (mod (1+ index) (length projects)) projects))))
	  (perject-switch proj nil msg))))

(defun perject-previous-project (&optional msg)
  "Switch to the previous project within the current collection.
If there is no current collection, throw an error. If the optional argument MSG
is non-nil, also print an informative message.
In interactive use, this is determined by `perject-messages'.

This function runs the hooks `perject-before-switch-hook' and
`perject-after-switch-hook'."
  (interactive (list (memq 'previous-project perject-messages)))
  (let ((projects (alist-get (perject-assert-collection) perject-collections nil nil #'string-equal))
		(current (cdr (perject-current))))
	(unless projects
	  (user-error "The current collection has no associated projects"))
	(let* ((index (or (and current (cl-position current projects :test #'string-equal)) 0))
		   (proj (cons (car (perject-current)) (nth (mod (1- index) (length projects)) projects))))
	  (perject-switch proj nil msg))))


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
Note that this function does not check whether PROJ is an existing project and
whether BUFFER has already been killed, so caller functions should take care of
that."
  (interactive
   (list
	(current-buffer)
	(if (and (not current-prefix-arg) (cdr (perject-current)))
		(perject-current)
	  ;; Let the user select from the projects of the current collection only if
	  ;; not all of them are already associated with the buffer.
	  (if-let (((or (not current-prefix-arg) (equal current-prefix-arg '(4))))
			   (col (car (perject-current)))
			   (projects (cl-remove-if
						  (apply-partially #'perject-is-assoc-with (current-buffer))
						  (perject-get-projects col))))
		  (perject--get-project-name
		   "Add buffer to project: " projects nil t nil nil
		   "No project specified")
		(perject--get-project-name
		 "Add buffer to project: " 'all
		 (-compose 'not (apply-partially #'perject-is-assoc-with (current-buffer)))
		 t nil
		 "All projects are already associated with the current buffer"
		 "No project specified")))
    (memq 'add-buffer perject-messages)))
  (when (perject-is-assoc-with buffer proj)
	(user-error "Buffer '%s' is already associated with project '%s'"
                (buffer-name buffer) (perject-project-to-string proj)))
  (with-current-buffer buffer
	(push proj perject-buffer))
  (when msg
    (message "Added buffer '%s' to project '%s'."
			 (buffer-name buffer) (perject-project-to-string proj))))


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
Note that this function does not check whether PROJ is an existing project and
whether BUFFER has already been killed, so caller functions should take care of
that."
  (interactive
   (list
	(current-buffer)
	(if (and (not current-prefix-arg) (cdr (perject-current)))
		(perject-current)
	  ;; Let the user select from the projects of the current collection only if
	  ;; at least one of them is associated with the buffer.
	  (if-let (((or (not current-prefix-arg) (equal current-prefix-arg '(4))))
			   (col (car (perject-current)))
			   (projects (cl-remove-if-not
						  (apply-partially #'perject-is-assoc-with (current-buffer))
						  (perject-get-projects col))))
		  (perject--get-project-name
		   "Remove buffer from project: " projects nil t nil nil
		   "No project specified")
		(perject--get-project-name
		 "Remove buffer from project: " 'all
		 (apply-partially #'perject-is-assoc-with (current-buffer))
		 t nil
		 "The buffer is currently not associated with any project"
		 "No project specified")))
	(memq 'remove-buffer perject-messages)))
  (unless (perject-is-assoc-with buffer proj)
	(user-error "Buffer '%s' is not associated with project '%s'"
                (buffer-name buffer) (perject-project-to-string proj)))
  (with-current-buffer buffer
	(setq perject-buffer (delete proj perject-buffer)))
  (when msg
    (message "Removed buffer '%s' from project '%s'."
			 (buffer-name buffer) (perject-project-to-string proj))))

(defun perject-print-buffer-projects (&optional buffer)
  "Print the names of the projects with which the buffer BUFFER is associated.
If nil, BUFFER defaults to the current buffer, which is also its value in
interactive use."
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
		(buffer-name (buffer-name buffer)))
	(pcase (buffer-local-value 'perject-buffer buffer)
	  ('nil
	   (message "The buffer '%s' is not associated with any projects" buffer-name))
	  (`(,project)
	   (message "The buffer '%s' is associated with the project '%s'" buffer-name (perject-project-to-string project)))
	  (projects
	   (message "The buffer '%s' is associated with the projects: %s"
				buffer-name
				(string-join (mapcar #'perject-project-to-string projects) ", "))))))


;;;; Public Interface

(defun perject-current (&optional frame)
  "Return the collection or project currently associated with the frame FRAME.
If FRAME is nil, use the current frame.
The returned value is a dotted pair with car the collection and cdr the project
name."
  (frame-parameter frame 'perject-project))

(defun perject-set-current (proj &optional frame)
  "Set the collection and project of the frame FRAME to PROJ.
PROJ may be a dotted pair with car the collection and cdr the project name.
It may alternatively be a collection name or nil. In the latter case, the frame
will no longer belong to a collection or project.
If FRAME is nil, it defaults to the selected frame."
  (let ((proj (if (stringp proj) (cons proj nil) proj)))
	(set-frame-parameter frame 'perject-project proj)
	(when perject-frame-title-format
	  (set-frame-parameter frame 'name (and proj (funcall perject-frame-title-format proj))))))

(defun perject-get-collection-dir (name)
  "Return the directory belonging to a collection named NAME.
The collection and directory need not actually exist."
  (expand-file-name (concat (file-name-as-directory perject-directory) name)))

(defun perject-get-collections (&optional scope)
  "Return a list containing the names of all collections.
Each collection is represented as a directory in `perject-directory'.
If SCOPE is \\='active, only return the active collections; i.e. those which are
currently loaded. If SCOPE is \\='inactive, return all collections that are not
active at the moment."
  (if (eq scope 'active)
	  (mapcar #'car perject-collections)
	(let ((col
		   (and
			(file-exists-p perject-directory)
			(remove ".."
					(remove "."
							(mapcar #'car
									(cl-remove-if-not
									 (lambda (elem)
									   (eq (cadr elem) t))
									 (directory-files-and-attributes perject-directory))))))))
	  (if (eq scope 'inactive)
		  (cl-set-difference col (perject-get-collections 'active) :test #'string-equal)
		col))))

(defun perject-get-projects (&optional collection)
  "Return a list containing the names of all projects belonging to COLLECTION.
More precisely, the list contains dotted pairs with car the respective
collection name and cdr the project name.
COLLECTION is a string denoting a collection. If COLLECTION is nil, use the
current collection if there is one. If there is no current collection or
COLLECTION is \\='all, return the projects of all active collections."
  (cl-flet ((fun (list) (mapcar (apply-partially #'cons (car list)) (cdr list))))
	(let ((collection (or collection (car (perject-current)))))
	  (if (stringp collection)
		  (fun (assoc collection perject-collections))
		(seq-mapcat #'fun perject-collections)))))

(defun perject-collection-p (name &optional scope)
  "Return a non-nil value if there exists a collection called NAME.
The optional argument SCOPE behaves like for `perject-get-collections'."
  (member name (perject-get-collections scope)))

(defun perject-project-p (proj)
  "Return a non-nil value if the project PROJ exists.
PROJ is a cons cell with car a collection name and cdr a project name."
  (member (cdr proj) (alist-get (car proj) perject-collections nil nil #'string-equal)))

(defun perject-project-to-string (proj)
  "Return a string representing the project PROJ.
PROJ is a dotted pair with car a collection and cdr a project name.
Which string is returned is determined by `perject-project-format'."
  (if (stringp perject-project-format)
	  (format perject-project-format (car proj) (cdr proj))
	(funcall perject-project-format (car proj) (cdr proj))))

(defun perject-is-assoc-with (obj proj)
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
		((pred framep) (string-equal (car proj) (car (perject-current obj)))))
	(pcase-exhaustive obj
	  ((pred bufferp) (member proj (buffer-local-value 'perject-buffer obj)))
	  ((pred framep) (equal proj (perject-current obj)))))))

(defun perject-anonymous-buffer-p (buffer)
  "Return non-nil if the buffer BUFFER is anonymous.
This means that it is not associated with any project."
  (null (buffer-local-value 'perject-buffer buffer)))

(defun perject-get-frames (proj)
  "Return the currently open frames which belong to PROJ.
PROJ may either be a dotted pair with car a collection and cdr a project name or
a collection name. In the latter case, frames that belong to one of the projects
of the collection are also returned."
  (cl-remove-if-not (lambda (frame) (perject-is-assoc-with frame proj))
					(frame-list)))

(defun perject-get-buffers (proj)
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

(defun perject-get-anonymous-buffers ()
  "Return the list of all buffers not associated with any project."
  (cl-remove-if (apply-partially #'buffer-local-value 'perject-buffer) (buffer-list)))

(defun perject-assert-collection (&optional frame)
  "Ensure that frame FRAME has a current collection and return it (a string).
If not, throw an error. If nil, FRAME defaults to the selected frame."
  (or (car (perject-current frame))
	  (user-error "The %sframe is not associated with any collection"
				  (if frame "" "current "))))

(defun perject-assert-project (&optional frame)
  "Ensure that frame FRAME has a current project and return it (a dotted pair).
If not, throw an error. If nil, FRAME defaults to the selected frame."
  (let ((current (perject-current frame)))
	(or (and (cdr current) current)
		(user-error "The %sframe is not associated with any project"
					(if frame "" "current ")))))


;;;; User Input Interface

(defun perject--get-collection-name
	(prompt type &optional predicate require-match def no-candidate empty-string)
  "Ask the user for a collection name using `completing-read' and return it.
TYPE may have one of the following values, which determines the available
candidates:
- a list of collections
- \\='active: active collections
- \\='inactive: inactive collections
- \\='all: all collections
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
			('all (perject-get-collections))
			('inactive (perject-get-collections 'inactive))
			('active (perject-get-collections 'active))
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
				(propertize " " 'display '(space :align-to (- center 60)))
				(propertize
				 (or
				  (and (perject-collection-p str 'active)
					   (or (string-join (mapcar #'cdr (perject-get-projects str)) ", ")
						   "no projects"))
				  (format-time-string "%Y %b %d %a" (file-attribute-access-time
													 (file-attributes
													  (desktop-full-file-name
													   (perject-get-collection-dir str)))))
				  "")
				 'face 'perject-project-annotator-main)
				(when (perject-collection-p str 'active)
				  (concat
				   (propertize " " 'display '(space :align-to (- center 20)))
				   (propertize
					(format "%4d buffer%s"
							(length (perject-get-buffers str))
							(if (not (eq (length (perject-get-buffers str)) 1)) "s" ""))
					'face 'perject-project-annotator-buffers)
				   (when (fboundp 'perject-tab-collection-tabs)
					   (concat
						(propertize " " 'display '(space :align-to (+ center 10)))
						(propertize
						 (format "%2d tab%s"
								 (length (perject-tab-collection-tabs str))
								 (if (not (eq (length (perject-tab-collection-tabs str)) 1)) "s" ""))
						 'face 'perject-project-annotator-tabs)))
				   (propertize " " 'display '(space :align-to (+ center 40)))
				   (propertize
					(format "%4d frame%s%s"
							(length (perject-get-frames str))
							(if (not (eq (length (perject-get-frames str)) 1)) "s" "")
							(if (string-equal (car (perject-current)) str) " [current]" ""))
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
PROMPT, PREDICATE, REQUIRE-MATCH, DEF, NO-CANDIDATE and EMPTY-STRING are the
 same as for `perject--get-collection-name'.
TYPE may have one of the following values:
- a list of dotted pairs (collection . project)
- a collection name: only those projects belonging to that collection
- \\='current: only those projects belonging to the current collection;
  if there is no current collection, behave like \\='all
- \\='all: all projects from all active collections."
  (let* ((candidates
		  (pcase type
			('all (perject-get-projects 'all))
			('current (perject-get-projects))
			((pred stringp) (perject-get-projects type))
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
				  (when (perject-project-p proj)
					(concat
					 (propertize " " 'display '(space :align-to (- center 30)))
					 (propertize
					  (format "%4d buffer%s"
							  (length (perject-get-buffers proj))
							  (if (not (eq (length (perject-get-buffers proj)) 1)) "s" ""))
					  'face 'perject-project-annotator-buffers)
					 (when (fboundp 'perject-tab-tabs)
					   (concat
						(propertize " " 'display '(space :align-to center))
						(propertize
						 (format "%2d tab%s"
								 (length (perject-tab-tabs proj))
								 (if (not (eq (length (perject-tab-tabs proj)) 1)) "s" ""))
						 'face 'perject-project-annotator-tabs)))
					 (propertize " " 'display '(space :align-to (+ center 30)))
					 (propertize
					  (format "%4d frame%s%s"
							  (length (perject-get-frames proj))
							  (if (not (eq (length (perject-get-frames proj)) 1)) "s" "")
							  (if (equal (perject-current) proj) " [current]" ""))
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
			"The character '%c' is not valid for naming a collection. See the variable `perject-valid-naming-chars'"
			char)))
	 name)
	(when (string-empty-p name)
      (user-error "The collection name cannot be empty"))
    (when (member name (perject-get-collections))
      (user-error "There already is a collection named '%s'" name))
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
			"The character '%c' is not valid for naming a project. See the variable `perject-valid-naming-chars'"
			char )))
	 name)
	(when (string-empty-p name)
      (user-error "The project name cannot be empty"))
    (when (member name (mapcar #'cdr (perject-get-projects collection)))
      (user-error "There already is a project named '%s' within this collection" name))
    name))


;;;; Helper Functions

(defun perject-mode-line-toggle-current-buffer (event)
  "Toggle the association between the buffer and the project from the mode-line.
If the current buffer is associated with the current project, remove this
association. Otherwise, add it.
EVENT is a mouse event."
  (interactive "e")
  (when-let ((proj (perject-current))
			 ((car proj))
			 (buffer (window-buffer (posn-window (event-start event)))))
    (if (perject-is-assoc-with buffer proj)
		(perject-remove-buffer-from-project buffer proj)
	  (perject-add-buffer-to-project buffer proj))))

(defun perject--auto-add-buffer (&optional ignore)
  "Silently add the current buffer to projects.
This honors the variable `perject-auto-add-function' unless IGNORE is non-nil,
in which case the current buffer is simply added to the current project.
Does nothing to projects that are already associated with the buffer."
  (let ((buffer (current-buffer)))
	(if (or ignore (not perject-auto-add-function))
		(let ((project (perject-current)))
		  (when (and (cdr project) (not (perject-is-assoc-with buffer project)))
			(perject-add-buffer-to-project buffer project)))
	  (dolist (project (funcall perject-auto-add-function buffer (perject-current)))
		(when (and (perject-project-p project)
				   (not (perject-is-assoc-with buffer project)))
		  (perject-add-buffer-to-project buffer project))))))

(defun perject--serialize-mark-ring (value _)
  "Serialize VALUE for the mark ring."
  (mapcar #'marker-position value))

(defun perject--deserialize-mark-ring (value _)
  "Deserialize VALUE for the mark ring."
  (mapcar #'copy-marker value))


;;;; Interface to desktop.el

(defun perject-desktop-load (name)
  "Load the collection named NAME from the corresponding desktop file.
This includes adding NAME to the alist of active collections
`perject-collections'."
  ;; Locally bind `perject--previous-collections' so that
  ;; `perject--desktop-info' can temporarily hijack it in order to save the
  ;; message to be printed.
  (let ((perject--previous-collections perject--previous-collections))
	(let ((desktop-var-serdes-funs
		   (cons
			(list 'perject-buffer
				  nil
				  (lambda (projects)
					(append perject-buffer projects)))
			(mapcar (lambda (triple)
					  (list (car triple) (cadr triple) (apply-partially (-flip (caddr triple)) name)))
					(cl-remove-if-not #'listp perject-local-vars-to-save))))
		  (desktop-load-locked-desktop t)
		  ;; `desktop-read' prints information about the loaded desktop file,
		  ;; which we want to print ourselves. However, all other messages (e.g.
		  ;; those printed by the modes loaded when restoring buffers) should be
		  ;; visible. We also want to print any warnings (which in the code are
		  ;; just messages) that `desktop-read' produces.
		  ;; To silence `desktop-read' temporarily, we locally bind
		  ;; `inhibit-message' and `message-log-max' and change them within
		  ;; `desktop-after-read-hook'.
		  (inhibit-message inhibit-message)
		  (message-log-max message-log-max)
		  (desktop-after-read-hook
		   (append desktop-after-read-hook
				   (list (apply-partially #'perject--desktop-info name)))))
	  ;; Unlike `desktop-save', `desktop-load' seems to have a weird
	  ;; implementation, so we need the following line since otherwise desktop
	  ;; thinks that it is loading the already loaded desktop again and refuses to
	  ;; do so.
	  (setq desktop-dirname (file-name-as-directory (perject-get-collection-dir name)))
	  ;; Make `desktop-restore-frameset' accept another parameter from
	  ;; `frameset-restore', namely `perject--desktop-reuse-frames'.
	  ;; The function also sets ;; `perject--desktop-restored-frames'.
	  (cl-letf (((symbol-function 'desktop-restore-frameset)
				 (lambda ()
				   (when (desktop-restoring-frameset-p)
					 ;; Reset `perject--desktop-restored-frames'.
					 (setq perject--desktop-restored-frames nil)
					 (frameset-restore
					  desktop-saved-frameset
					  :reuse-frames perject--desktop-reuse-frames
					  ;; Use the cleanup to set the list of restored frames.
					  :cleanup-frames (lambda (frame action)
										(when (memq action '(:reused :created))
										  (push frame perject--desktop-restored-frames)))
					  :force-display desktop-restore-in-current-display
					  :force-onscreen desktop-restore-forces-onscreen)))))
	  (desktop-read desktop-dirname)))
	;; Change the frame title of the newly restored frames, if desired.
	;; The 'name' frame parameter in `frameset-filter-alist' is not restored by default.
	;; While we could change that setting, this would also influence other times when the user
	;; names the frame, so we instead just set the value when loading.
	(when perject-frame-title-format
	  (dolist (frame perject--desktop-restored-frames)
		(set-frame-parameter frame 'name
							 (funcall perject-frame-title-format (perject-current frame)))))
	(run-hook-with-args 'perject-desktop-after-load-hook name)
	(when (stringp perject--previous-collections)
	  (message perject--previous-collections))))

(defun perject-desktop-save (name &optional release-lock msg)
  "Save the collection named NAME to the corresponding desktop file.
If the optional argument RELEASE-LOCK is non-nil, Emacs will release the lock of
the corresponding desktop file. If the optional argument MSG is non-nil, print a
message after saving the collection."
  (unless (perject-collection-p name 'active)
	(error "Collection '%s' does not exist" name))
  (run-hook-with-args 'perject-desktop-save-hook name)
  (let ((desktop-globals-to-save
		 ;; Save the current collection and its projects. This must be saved
		 ;; first, so that when loading the desktop file the collection and its
		 ;; projects are set when the other variables are loaded.
		 (cons
		  (list 'perject-collections (-flip #'assoc) nil)
		  perject-global-vars-to-save))
		(desktop-var-serdes-funs
		 (cons
		  (list 'perject-buffer
				(lambda (projects)
				  (cl-remove-if-not (-compose (apply-partially #'string-equal name) #'car) projects))
				nil)
		  (mapcar (lambda (triple)
					(list (car triple) (apply-partially (-flip (cadr triple)) name) (caddr triple)))
				  (cl-remove-if-not #'listp perject-local-vars-to-save))))
		(desktop-locals-to-save (cl-remove-if #'listp perject-local-vars-to-save))
		;; Only save those buffers belonging to the current project and respect
		;; the value of `perject-buffers-predicate'.
		(desktop-buffers-not-to-save-function
		 (if perject-buffers-predicate
			 (lambda (file-name buffer-name major minors)
			   (and (perject-is-assoc-with (get-buffer buffer-name) name)
					(funcall perject-buffers-predicate
							 name file-name buffer-name major minors)))
		   (lambda (_ buffer-name _ _)
			 (perject-is-assoc-with (get-buffer buffer-name) name))))
		(save-frames (let* ((exception (assoc name (cdr perject-save-frames)))
							(value (if exception (cdr exception) (car perject-save-frames))))
					   (if (eq value 'ask)
						   (y-or-n-p (format "Save frames of collection '%s'?" name))
						 value)))
		;; Preserve the original function definition of `desktop-outvar',
		;; which is overwritten locally.
		(outvar (symbol-function 'desktop-outvar))
		;; Hack: Pretend the desktop file is from the same time, so that desktop does not
		;; complain that the desktop file is more recent than the one loaded.
		(desktop-file-modtime (file-attribute-modification-time
							   (file-attributes
								(desktop-full-file-name
								 (file-name-as-directory
								  (perject-get-collection-dir name)))))))
	;; Overwrite `desktop-outvar' so `desktop-save' deals with our more
	;; complex version of `desktop-globals-to-save' correctly.
	(cl-letf (((symbol-function 'desktop--check-dont-save)
			   (cond
				((not save-frames) #'ignore)
				((functionp perject-frames-predicate)
				 (lambda (frame)
					 (and (perject-is-assoc-with frame name)
						  (funcall perject-frames-predicate name frame))))
				(t (apply-partially (-flip #'perject-is-assoc-with) name))))
			  ((symbol-function 'desktop-outvar)
			   (lambda (triple)
				 (if (listp triple)
					 (insert "(perject-desktop-load-global-variable '"
							 (symbol-name (car triple))
							 " "
							 (desktop-value-to-string
							  (funcall (cadr triple) (symbol-value (car triple)) name))
							 " \""
							 name
							 "\")\n")
				   (funcall outvar triple)))))
	  (desktop-save (file-name-as-directory
					 (perject-get-collection-dir name))
					release-lock)))
  (when msg (message "Perject: Saved collection '%s'" name)))

(defun perject-desktop-load-global-variable (sym value name)
  "Set the global variable SYM to VALUE for the collection named NAME.
This is achieved using the deserializer as specified in
`perject-global-vars-to-save'."
  (if (eq sym 'perject-collections)
	  ;; If the variable is `perject-collections', add the new collection and its projects to it.
	  (push value perject-collections)
	(let ((deserializer (cadr (alist-get sym perject-global-vars-to-save))))
	  (if (functionp deserializer)
		  (set sym (funcall deserializer value name))
		(warn "No deserializer found for symbol '%s' in desktop file of collection
		  '%s'. Check `perject-global-vars-to-save'" sym name)))))

;; Printing the message directly within this function does not give the desired
;; result, so we temporarily overwrite `perject--previous-collections' in order
;; to print the message later.
(defun perject--desktop-info (name)
  "Generate information about the collection named NAME that was just restored.
This also sets the variables `inhibit-message' and `message-log-max' (which were
locally bound when this function is called).
Never call this function manually."
  ;; Code adapted from `desktop-read'.
  ;; We access the variables from `desktop-read'.
  (let ((projects (mapcar #'cdr (perject-get-projects name))))
	(setq perject--previous-collections
	 (format "Perject: Restored collection '%s'%s: %s%s%d buffer%s%s%s"
			 name
			 (if projects
				 (concat " (" (string-join projects ", ") ")") "")
			 (if desktop-saved-frameset
				 (let ((fn (length (frameset-states desktop-saved-frameset))))
				   (format "%d frame%s, "
						   fn (if (= fn 1) "" "s")))
			   "")
			 (if (fboundp 'perject-tab-collection-tabs)
				 (let ((fn (length (perject-tab-collection-tabs name))))
				   (format "%d tab%s, " fn (if (= fn 1) "" "s")))
			   "")
			 desktop-buffer-ok-count
			 (if (= 1 desktop-buffer-ok-count) "" "s")
			 (if (< 0 desktop-buffer-fail-count)
				 (format ", %d failed to restore" desktop-buffer-fail-count)
			   "")
			 (if desktop-buffer-args-list
				 (format ", %d to restore lazily"
						 (length desktop-buffer-args-list))
			   ""))))
  (setq inhibit-message t message-log-max nil))

(provide 'perject)
;;; perject.el ends here
