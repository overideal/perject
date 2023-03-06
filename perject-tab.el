;;; perject-tab.el --- Tab-tab integration for Perject -*- lexical-binding: t -*-

;; Copyright (C) 2022, 2023 overideal

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

;; Integrate perject with tab-bar.
;; This allows saving tabs (window configurations) for every project and
;; provides convenient switching between different arrangements of windows.


;;; Code:

(require 'perject)
(require 'tab-bar)
(require 'cl-lib)
(require 'seq)


;;;; Customization

(defgroup perject-tab nil
  "Add tab (window configuration) support to `perject'."
  :group 'perject
  :prefix "perject-tab-")

(defface perject-tab-mode-line-face '((t :foreground "dark orange"))
  "The face used by the mode line indicator of perject tabs."
  :group 'perject-faces)

(defcustom perject-tab-mode-line-format #'perject-tab-mode-line-indicator
  "This variable determines the mode line indicator of perject-tab.
It may have one of the following two values:
- nil: No mode line entry is shown for perject tab.
- A function: Call that function with four arguments: A dotted pair representing
  the current project, the list of tabs of that project, the current index and
  the recent index. The function should return the string to display in the mode
  line.
By default, the function `perject-tab-mode-line-indicator' is used. It displays
\"[1/4]\" if the user is at the first tab out of four, where the kind of
brackets used is determined by the \"mutable\" states (see
`perject-tab-states')."
  :type '(choice
		  (const :tag "No mode line entry is shown for perject" nil)
		  (function :tag "Custom function")))

(defcustom perject-tab-name-function tab-bar-tab-name-function
  "Function to get a tab name for perject.
This variable acts exactly like `tab-bar-tab-name-function' and exists for the
reason that a user might want a different naming scheme to apply to perject tabs
than to ordinary ones."
  :type '(choice (const :tag "Selected window buffer"
                        tab-bar-tab-name-current)
                 (const :tag "Selected window buffer with window count"
                        tab-bar-tab-name-current-with-count)
                 (const :tag "Truncated buffer name"
                        tab-bar-tab-name-truncated)
                 (const :tag "All window buffers"
                        tab-bar-tab-name-all)
                 (function  :tag "Function"))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update)))

(defcustom perject-tab-name-truncated-max tab-bar-tab-name-truncated-max
  "Maximum length of the tab name from the current buffer.
This variable acts exactly like `tab-bar-tab-name-truncated-max' and exists for
the reason that a user might want a different maximum length to apply to perject
tabs than to ordinary ones."
  :type 'natnum)

(defcustom perject-tab-messages
  '(create delete cycle-state set reset index switch next previous recent)
  "Whether to print informative messages when performing certain actions.
The value of this variable is a list which may contain any of the following
symbols, whose presence in the list leads to a message being printed in the
indicated command:
- \\='create: `perject-tab-create',
- \\='delete: `perject-tab-delete',
- \\='cycle-state: `perject-tab-cycle-state',
- \\='set: `perject-tab-set'
- \\='reset: `perject-tab-reset',
- \\='index: `perject-tab-increment-index', `perject-tab-decrement-index',
- \\='switch: `perject-tab-switch',
- \\='next: `perject-tab-next',
- \\='previous: `perject-tab-previous',
- \\='recent: `perject-tab-recent'."
  :type '(set
		  (const :tag "`perject-tab-create'" create)
		  (const :tag "`perject-tab-delete'" delete)
		  (const :tag "`perject-tab-cycle-state'" cycle-state)
		  (const :tag "`perject-tab-set'" set)
		  (const :tag "`perject-tab-reset'" reset)
		  (const :tag "`perject-tab-increment-index', `perject-tab-decrement-index'" index)
		  (const :tag "`perject-tab-switch'" switch)
		  (const :tag "`perject-tab-next'" next)
		  (const :tag "`perject-tab-previous'" previous)
		  (const :tag "`perject-tab-recent'" recent)))

(defcustom perject-tab-no-next #'perject-tab-no-tab
  "A function called by `perject-tab-next' if there are no tabs.
The default function simply prints an informative message."
  :type 'function)

(defcustom perject-tab-no-previous #'perject-tab-no-tab
  "A function called by `perject-tab-previous' if there are no tabs.
The default function simply prints an informative message."
  :type 'function)

(defcustom perject-tab-no-recent #'perject-tab-no-recent
  "A function called by `perject-tab-recent' if there is no recent tab.
The default function simply prints an informative message."
  :type 'function)

(defcustom perject-tab-states
  '(("dynamic" perject-tab--dynamic-state "[" "]") ("immutable" ignore "⟦" "⟧") ("mutable" always "⟨" "⟩"))
  "A list representing the different states a tab may be in.
The state of a tab decides whether the current window configuration will be
saved to that tab when switching from that tab to another one.

By default, the following states are provided:
- \"immutable\": An immutable tab is never updated.
- \"mutable\": A mutable tab is always updated.
- \"dynamic\": A dynamic tab is updated only if the new window configuration has
  the same window layout. This test ignores details such as the values of point
  and scrolling positions.

The user may freely alter this list and add or remove states. However, it may
not be empty. Furthermore, its order matters: The first entry of the list
serves as a default value for newly created tabs and
`perject-tab-cycle-state' cycles the states in the order given by the list.

Each entry in the list is one possible state, which is given as a list:
The first string is the name of the state. It is used to represent the state in
text.
The second entry is a function that is called with two arguments when switching
tabs. Its first argument is the tab's project and the second is the tab itself.
The window configuration that is current when the function is called
\(`current-window-configuration') will be used to update the tab if and only if
the function returns a non-nil value.
The remaining two arguments are two strings (usually brackets) used by
`perject-tab-mode-line-indicator' to represent the state in the mode line. These
only have an effect if `perject-tab-mode-line-format' is set to that function."
  :type '(repeat
		  (list string function string string)))

(defcustom perject-tab-index-after-delete 'recent-previous
  "This variable determines the new index after deleting a tab.
This influences the command `perject-tab-delete'.
It may have one of the following values:
- \\='next: Use the next index.
- \\='previous: Use the previous index.
- \\='recent-next: Use the index of the most recently selected tab after
  deleting. If there is no recent index, use the next index.
- \\='recent-previous: Use the index of the most recently selected tab after
  deleting. If there is no recent index, use the previous index.
After deleting, perject will also switch to the corresponding tab if
`perject-tab-switch-after-delete' is non-nil.
Use `perject-tab-delete-hook' in case you want more sophisticated control
over perject's behavior after deleting a tab."
  :type '(choice
		  (const :tag "Use the next index" next)
		  (const :tag "Use the previous index" previous)
		  (const :tag "Use the index of the most recently selected tab after
		  deleting and the next one if it does not exist" recent-next)
		  (const :tag "Use the index of the most recently selected tab after
		  deleting and the previous one if it does not exist" recent-previous)))

(defcustom perject-tab-switch-after-delete t
  "When non-nil, automatically switch to a tab after deleting another one.
The tab switched to is determined by `perject-tab-index-after-delete'."
  :type '(choice
		  (const :tag "Switch tab after delete" t)
		  (const :tag "Do not switch tab after delete" t)))

(defcustom perject-tab-extra-data '((buffers . perject-tab-get-visible-buffers))
  "List representing extra data that is saved on tab creation.
For each entry, extra data will be saved to every new tab created. This can
later be accessed, e.g. by the functions in `perject-tab-states'.
Each entry is dotted pair with car a symbol cdr a function. The symbol is used
as the key of the new data within every tab (which is essentially an alist). To
produce the data, the specified function is called without any arguments when
the tab is created (i.e. when the current window configuration is that being
saved to the tab) and its return value is saved.

Note that the \"dynamic\" state (see `perject-tab-states') depends on the entry
\"buffers\" being in this list."
  :type '(repeat
		  (cons symbol function)))


;;;; Hooks

(defcustom perject-tab-create-hook nil
  "Hook run after a new tab was created using `perject-tab-create'.
The functions are called with two arguments, namely the dotted pair representing
the project and the affected frame."
  :type 'hook)

(defcustom perject-tab-delete-hook nil
  "Hook run after a tab was deleted using `perject-tab-delete'.
The functions are called with two arguments, namely the dotted pair representing
the project and the affected frame."
  :type 'hook)

(defcustom perject-tab-switch-hook nil
  "Hook run after switching tabs using `perject-tab-switch'.
The functions are called with three arguments: a dotted pair representing the
project, the old index and the new index that was switched to."
  :type 'hook)

(defcustom perject-tab-cycle-state-hook nil
  "Hook run after cyling the state of a tab using `perject-tab-cycle-state'.
The functions are called with two arguments, namely a dotted pair representing
the project and the index being toggled."
  :type 'hook)


;;;; Internal Variables

(defvar perject-tab--tabs nil
  "Alist saving the list of tabs (cdr) for each project (car).")


;;;; The Mode

;;;###autoload
(define-minor-mode perject-tab-mode
  "Add tab (window configuration) support to `perject'."
  :global t
  :keymap (make-sparse-keymap)
  (if perject-tab-mode
	  (progn
		(unless perject-mode (perject-mode))
		(unless (or (not perject-mode-line-format)
                    (assoc 'perject-tab-mode mode-line-misc-info)
                    (not mode-line-misc-info))
          (push '(perject-tab-mode
				  (:eval
				   (funcall perject-tab-mode-line-format
							(perject-current) (perject-tab-tabs)
							(perject-tab-index 'current) (perject-tab-index 'recent))))
                (cdr (last mode-line-misc-info))))
		(add-to-list 'perject-global-vars-to-save
					 '(perject-tab--tabs perject-tab--serialize-tabs perject-tab--deserialize-tabs))
		(add-hook 'perject-desktop-after-load-hook #'perject-tab--init)
		(add-hook 'perject-rename-hook #'perject-tab--rename)
		;; This hook also covers the case that an active collection is deleted.
		(add-hook 'perject-after-close-hook #'perject-tab--close-or-delete)
		(add-hook 'perject-after-delete-project-hook #'perject-tab--close-or-delete)
		(add-hook 'perject-after-switch-hook #'perject-tab--switch))

	;; Remove the added hooks etc.
	(setq perject-global-vars-to-save (delete '(perject-tab--tabs perject-tab--serialize-tabs perject-tab--deserialize-tabs) perject-global-vars-to-save))
	(remove-hook 'perject-desktop-after-load-hook #'perject-tab--init)
	(remove-hook 'perject-rename-hook #'perject-tab--rename)
	(remove-hook 'perject-after-close-hook #'perject-tab--close-or-delete)
	(remove-hook 'perject-after-delete-project-hook #'perject-tab--close-or-delete)
	(remove-hook 'perject-after-switch-hook #'perject-tab--switch)))


;;;; Commands

;;;;; Managing Tabs

(defun perject-tab-create (&optional proj frame msg)
  "Save the window configuration within frame FRAME as a new tab for PROJ.
PROJ is a project given as dotted pair with car a collection and cdr a project
name. It may also be nil, in which case the current project of FRAME is used. If
nil, FRAME defaults to the selected one. If MSG is non-nil, also print a
message.
In interactive use, FRAME is the selected frame and the value of MSG is
determined by `perject-tab-messages'.
After creating the tab, this function runs the hook `perject-tab-create-hook'."
  (interactive (list nil nil (memq 'create perject-tab-messages)))
  (let* ((proj (or proj (perject-assert-project frame)))
		 (index (or (perject-tab-index 'current proj frame) 0)))
	(if-let ((list (assoc proj perject-tab--tabs)))
		(setf (nthcdr index (cdr list))
			  (cons (perject-tab-make nil frame) (nthcdr index (cdr list))))
	  (push (list proj (perject-tab-make nil frame)) perject-tab--tabs))
	(perject-tab-set-index 'recent (perject-tab-index 'current proj frame) proj frame)
	(perject-tab-set-index 'current (1+ index) proj frame)
	;; When the current project had no tabs and we created one, we need to update the "current"
	;; index of all frames displaying the project to 1.
	(let ((frame (or frame (selected-frame))))
	  (when (eq index 0)
		(dolist (f (perject-get-frames proj))
		  (unless (eq f frame)
			(perject-tab-set-index 'current 1 proj f))))
	  (when msg
		(message "Created tab for project '%s'" (perject-project-to-string proj)))
	  (run-hook-with-args 'perject-tab-create-hook proj frame))
	(force-mode-line-update t)))

(defun perject-tab-delete (&optional proj frame msg)
  "Delete the current tab within frame FRAME belonging to the project PROJ.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case the current project of FRAME is used. If nil,
FRAME defaults to the selected one. If MSG is non-nil, also print a message.
In interactive use, FRAME is the selected frame and the value of MSG is
determined by `perject-tab-messages'.
After deleting the tab, this function runs the hook
`perject-tab-delete-hook'."
  (interactive (list nil nil (memq 'delete perject-tab-messages)))
  (let* ((proj (or proj (perject-assert-project frame)))
		 (tabs (perject-tab-assert-tabs proj))
		 (length (length tabs))
		 (index (perject-tab-index 'current proj frame)))
	;; Remove the tab from the list.
	(if (eq index 1)
		(setcdr (assoc proj perject-tab--tabs) (cdr tabs))
	  (setf (nthcdr (1- index) tabs) (nthcdr index tabs)))
	;; Compute the current and recent index for all frames, unless they have no
	;; current index for the project.
	(dolist (f (frame-list))
	  (when-let ((current (perject-tab-index 'current proj f)))
		(let ((recent (perject-tab-index 'recent proj f)))
		  (when recent
			(if (> recent index)
				(setq recent (1- recent))
			  (when (eq recent index) (setq recent nil))))
		  ;; Compute the new current index in the frame.
		  ;; If the frame is currently displaying the deleted tab, switch to a
		  ;; different one if desired.
		  (if (eq current index)
			  (progn
				(if (and recent (memq perject-tab-index-after-delete '(recent-next recent-previous)))
					(setq current recent recent nil)
				  (setq current
						(if (memq perject-tab-index-after-delete '(previous recent-previous))
							(perject-tab-previous-index current (1- length))
						  (perject-tab-next-index (1- current) (1- length)))))
				;; The current index of the frame is the one being deleted, but
				;; we need to also ensure that it has the correct project.
				(if (and current (equal (perject-current f) proj)
						 perject-tab-switch-after-delete)
					  (with-selected-frame f
						(perject-tab-switch current 'ignore))
				  (perject-tab-set-index 'current current proj f)))
			(when (> current index)
			  (perject-tab-set-index 'current (1- current) proj f)))
		  (perject-tab-set-index 'recent
								 (and (not (eq recent (perject-tab-index 'current proj f)))
									  recent)
								 proj f))))
	(when msg
	  (message "Deleted tab of project '%s'" (perject-project-to-string proj)))
	(run-hook-with-args 'perject-tab-delete-hook proj (or frame (selected-frame)))
	(when (eq length 1)
	  (force-mode-line-update t))))

(defun perject-tab-cycle-state (num &optional proj msg)
  "Cycle the \"mutable\" state of the tab of project PROJ at index NUM.
The states are defined by `perject-tab-states'.
PROJ is a dotted pair with car a collection and cdr a project name. It may also
be nil, in which case the current project of the selected frame is used.
In interactive use, NUM defaults to the current index and PROJ to the
current project.
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'."
  (interactive (list (perject-tab-index 'current) nil
					 (memq 'cycle-state perject-tab-messages)))
  (let* ((proj (or proj (perject-assert-project)))
		 (tab (progn (perject-tab-assert-index num proj) (nth (1- num) (perject-tab-tabs proj))))
		 (new-state (or (cadr (member (perject-tab-state num proj) (mapcar #'car perject-tab-states)))
						(caar perject-tab-states))))
	(setf (alist-get 'perject-state tab) new-state)
    (when msg
	  (if (and (eq (perject-tab-index 'current) num)
			   (equal proj (perject-current)))
		  (message "Tab is now %s" new-state)
		(message "Tab %s of project '%s' is now %s"
				 num (perject-project-to-string proj)
				 new-state)))
	(run-hook-with-args 'perject-tab-cycle-state-hook proj num)
	(force-mode-line-update t)))

(defun perject-tab-set (num &optional tab-or-frame proj msg)
  "Set the tab with index NUM in project PROJ to TAB-OR-FRAME.
TAB-OR-FRAME may be a tab, a frame or nil. If it is a frame, use the current
window configuration of that frame, defaulting to the selected frame if
TAB-OR-FRAME is nil. If TAB-OR-FRAME is not a tab, ensure that the newly created
tab has the same \"mutable\" state as the tab it is replacing.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case it defaults to the current project.
In interactive use, set the tab of the current index to the current window
configuration.
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'."
  (interactive
   (list (perject-tab-index 'current) nil nil (memq 'set perject-tab-messages)))
  (let* ((proj (or proj
				   (perject-assert-project (and (framep tab-or-frame) tab-or-frame))))
		 (tab (progn (perject-tab-assert-index num proj)
					 (if (or (framep tab-or-frame) (null tab-or-frame))
						 (perject-tab-make (perject-tab-state num proj) tab-or-frame)
					   tab-or-frame))))
	(setf (nth (1- num) (perject-tab-tabs proj)) tab)
	(when msg
	  (if (and (eq (perject-tab-index 'current) num)
			   (equal proj (perject-current)))
		  (message "Updated current tab")
		(message "Updated tab %s of project '%s'" num (perject-project-to-string proj))))))

(defun perject-tab-increment-index (&optional msg)
  "Increase the index of the current tab by one.
This effectively interchanges the current tab with its successor.
If it is the tab with the highest index, it becomes the tab with the lowest
index 1 (and all other tabs increase their index by one).
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'."
  (interactive (list (memq 'index perject-tab-messages)))
  (perject-assert-project)
  (let ((tabs (perject-tab-assert-tabs (perject-current)))
		(index (perject-tab-index 'current))
		(recent (perject-tab-index 'recent)))
	;; Change the order of the tabs and update the recent index.
	(if (eq index (length tabs))
		;; The current tab is the last one.
		(progn
		  (setcdr (assoc (perject-current) perject-tab--tabs)
				  (cons (car (last tabs)) (butlast tabs)))
		  (when (and recent (not (eq recent index)))
			;; Recent and index should never be equal anyway.
			(perject-tab-set-index 'recent (1+ recent))))
	  (setcdr (assoc (perject-current) perject-tab--tabs)
			  (append
			   (seq-take tabs (1- index))
			   (list (nth index tabs) (nth (1- index) tabs))
			   (seq-drop tabs (1+ index))))
	  (when (eq recent (perject-tab-next-index index (length tabs)))
		(perject-tab-set-index 'recent index)))
	(perject-tab-set-index 'current (perject-tab-next-index index (length tabs)))
	(when msg
	  (message "Changed index of tab: %s -> %s" index (perject-tab-index 'current)))))

(defun perject-tab-decrement-index (&optional msg)
  "Decrease the index of the current tab by one.
This effectively interchanges the current tab with its predecessor.
If the tab has index 1, it becomes the tab with the highest index (and all other
tabs decrease their index by one).
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'."
  (interactive (list (memq 'index perject-tab-messages)))
  (perject-assert-project)
  (let ((tabs (perject-tab-assert-tabs (perject-current)))
		(index (perject-tab-index 'current))
		(recent (perject-tab-index 'recent)))
	(if (eq index 1)
		;; The current tab is the first one.
		(progn
		  (setcdr (assoc (perject-current) perject-tab--tabs)
				  (append (cdr tabs) (list (car tabs))))
		  (when (and recent (not (eq recent index)))
			(perject-tab-set-index 'recent (1- recent))))
	  (setcdr (assoc (perject-current) perject-tab--tabs)
			  (append
			   (seq-take tabs (- index 2))
			   (list (nth (1- index) tabs) (nth (- index 2) tabs))
			   (seq-drop tabs index)))
	  (when (eq recent (perject-tab-previous-index index (length tabs)))
		(perject-tab-set-index 'recent index)))
	(perject-tab-set-index 'current (perject-tab-previous-index index (length tabs)))
	(when msg
	  (message "Changed index of tab: %s -> %s" index (perject-tab-index 'current)))))


;;;;; Switching Tabs

(defun perject-tab-switch (num &optional force-update msg)
  "Switch to the tab with index NUM (starting from one) of the current project.
This influences the selected frame. If the state of the old tab (the one before
switching) allows it (see `perject-tab-states'), also update its value to the
current window configuration.
This is the behavior in interactive use, but when called as a function it is
influenced by the optional argument FORCE-UPDATE. If it is nil, behave as above.
If it is t, always update and if it is \\='ignore, never update the old tab.
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'.
After switching tabs, this function runs the hook
`perject-tab-switch-hook'."
  (interactive
   (list current-prefix-arg (perject-current)
		 (memq 'switch perject-tab-messages)))
  (let ((proj (perject-assert-project))
		(old-index (perject-tab-index 'current)))
	(unless num (user-error "Use a prefix argument to specify an index to switch to"))
	(perject-tab-assert-index num proj)
	;; If the current tab is mutable and FORCE-UPDATE allows it, update the tab.
	(when (or (eq force-update t)
			  (and (not (eq force-update 'ignore)) old-index
				   (perject-tab-update-p old-index)))
	  (perject-tab-set old-index))
	(let ((tab-bar-tabs-function #'perject-tab--tabs-function)
		  (current (nth (1- num) (perject-tab-tabs)))
		  ;; Silence `tab-bar-select-tab'.
		  (tab-bar-mode t))
	  ;; At the end of `tab-bar-select-tab' the new tab is replaced by a pseudo
	  ;; tab indicating that it is current, but not saving the information we
	  ;; need (e.g. when another frame wants to display that same tab).
	  ;; This can either be solved by manually switching the offending value
	  ;; back or by locally overwriting a function. We use the second approach.
	  (cl-letf (((symbol-function 'tab-bar--current-tab-make) (lambda (&rest _) current)))
		(tab-bar-select-tab num))
	  ;; After restoring a tab from the desktop, many entries of the tab (e.g.
	  ;; the window configuration wc) are nil. In that case, we refresh the tab
	  ;; after switching to it, creating the data.
	  (unless (window-configuration-p (alist-get 'wc current))
		(perject-tab-set num)))
	;; If the new index is the same as the old one, do not update recent.
	(unless (eq num (perject-tab-index 'current))
	  (perject-tab-set-index 'recent (perject-tab-index 'current)))
	(perject-tab-set-index 'current num)
	(when msg
	  (message "Switched to tab %s in project '%s'" num (perject-project-to-string proj)))
	(run-hook-with-args 'perject-tab-switch-hook proj old-index num)))

(defun perject-tab-next (&optional msg)
  "Switch to the next tab for the current project in the selected frame.
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'."
  (interactive (list (memq 'next perject-tab-messages)))
  (perject-assert-project)
  (if-let ((tabs (perject-tab-tabs)))
	  (progn
		(perject-tab-switch (perject-tab-next-index (perject-tab-index 'current)
													 (length tabs)))
		(when msg
		  (message "Switched to next tab of project '%s'"
				   (perject-project-to-string (perject-current)))))
	(funcall perject-tab-no-next)))

(defun perject-tab-previous (&optional msg)
  "Switch to the previous tab for the current project in the selected frame.
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'."
  (interactive (list (memq 'previous perject-tab-messages)))
  (perject-assert-project)
  (if-let ((tabs (perject-tab-tabs)))
	  (progn
		(perject-tab-switch (perject-tab-previous-index (perject-tab-index 'current)
														 (length tabs)))
		(when msg
		  (message "Switched to previous tab of project '%s'"
				   (perject-project-to-string (perject-current)))))
	(funcall perject-tab-no-previous)))

(defun perject-tab-recent (&optional msg)
  "Switch to the most recently selected tab.
The variable `perject-tab-no-recent' determines what happens if this function is
called when there is no recent tab.
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'."
  (interactive (list (memq 'recent perject-tab-messages)))
  (perject-assert-project)
  (if-let ((recent (perject-tab-index 'recent)))
	  (progn
		(perject-tab-switch recent)
		(when msg
		  (message "Switched to most recent tab of project '%s'"
				   (perject-project-to-string (perject-current)))))
	(funcall perject-tab-no-recent)))

(defun perject-tab-reset (&optional msg)
  "Change the current window configuration to that given by the current tab.
This essentially resets the window configuration to that specified by the
current tab.
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'."
  (interactive (list (memq 'reset perject-tab-messages)))
  (let ((index (perject-tab-index 'current)))
  (perject-tab-switch index 'ignore)
  (when msg
	(message "Reset window configuration to tab %s of project '%s'"
			 index (perject-project-to-string (perject-current))))))


;;;; Public Interface

(defun perject-tab-tabs (&optional proj)
  "Return the list of perject tabs for project PROJ.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case it defaults to the current project."
  (alist-get (or proj (perject-current)) perject-tab--tabs nil nil #'equal))

(defun perject-tab-index (symbol &optional proj frame)
  "Return the index of SYMBOL for project PROJ in the frame FRAME.
SYMBOL may be \\='current or \\='recent, which references the current or most
recent index of FRAME, respectively. If FRAME is nil, use the current frame.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case it defaults to the current project."
  (let ((proj (or proj (perject-current))))
	(funcall (if (eq symbol 'current) #'car #'cdr)
			 (alist-get proj (frame-parameter frame 'perject-tab) nil nil #'equal))))

(defun perject-tab-set-index (symbol num &optional proj frame)
  "Set the index of SYMBOL to NUM for project PROJ in the frame FRAME.
SYMBOL may be \\='current or \\='recent, which references the current or most
recent index of FRAME, respectively. NUM is a number or nil, in which case the
respective index is set to that value. If FRAME is nil, use the current frame.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case it defaults to the current project."
  (let ((proj (or proj (perject-current frame))))
	(if-let ((list (assoc proj (frame-parameter frame 'perject-tab))))
		(setf (if (eq symbol 'current) (cadr list) (cddr list)) num)
	  (push (cons proj (cons (and (eq symbol 'current) num) (and (eq symbol 'recent) num)))
			(frame-parameter frame 'perject-tab)))))

(defun perject-tab-state (num &optional proj)
  "Return the \"mutable\" state of the tab of project PROJ at index NUM.
More precisely, the name of the state is returned.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case the current project of the selected frame is
used."
  (alist-get 'perject-state (nth (1- num) (perject-tab-tabs proj))))

(defun perject-tab-update-p (num &optional proj frame)
  "Return non-nil if the tab of project PROJ at index NUM should be updated.
This is determined by its state (see `perject-tab-states').
FRAME is a frame or nil, in which case the selected frame is used and PROJ is a
dotted pair with car a collection and cdr a project name. If PROJ is nil, use
the current project of FRAME."
  (let* ((proj (or proj (perject-current frame)))
		 (tab (nth (1- num) (perject-tab-tabs proj)))
		 (fun (car (alist-get (alist-get 'perject-state tab) perject-tab-states nil nil #'string-equal))))
	;; If the state is invalid, use the default state.
	(if frame
		(with-selected-frame frame
		  (funcall (if (functionp fun) fun (cadar perject-tab-states)) proj tab))
	  (funcall (if (functionp fun) fun (cadar perject-tab-states)) proj tab))))

(defun perject-tab-make (&optional state frame)
  "Create a new tab corresponding to the current window configuration in FRAME.
The tab will have the state STATE. STATE may be a string or nil. If it is nil,
use the default state (the car of `perject-tab-states'). FRAME may be a frame or
nil. In the latter case, it defaults to the selected frame."
  (let ((tab-bar-tab-name-function perject-tab-name-function)
		(tab-bar-tab-name-truncated-max perject-tab-name-truncated-max)
		(extra-data
		 (cons (cons 'perject-state (or state (caar perject-tab-states)))
			   (if frame
				   (with-selected-frame frame
					 (mapcar (lambda (data) (cons (car data) (funcall (cdr data))))
							 perject-tab-extra-data))
				 (mapcar (lambda (data) (cons (car data) (funcall (cdr data))))
						 perject-tab-extra-data)))))
	(append (tab-bar--tab frame) extra-data)))

(defun perject-tab-collection-tabs (name)
  "Return the list of all tabs belonging to the collection named NAME.
A tab is only in the list if it belongs to some project within the collection."
  (seq-mapcat #'cdr
			  (cl-remove-if-not (apply-partially #'equal name) perject-tab--tabs :key #'caar)))

(defun perject-tab-assert-tabs (proj)
  "Ensure that the project PROJ has at least one tab and return the list of tabs.
If there are no tabs, throw an error."
  (or (perject-tab-tabs proj)
	  (user-error "The %sproject has no tabs"
				  (if (equal proj (perject-current)) "current " ""))))

(defun perject-tab-assert-index (num proj)
  "Ensure that the index NUM is a valid index for the tabs of project PROJ.
PROJ is a dotted pair with car a collection and cdr a project name.
If INDEX is less than one or bigger than the number of tabs of PROJ, throw an
error."
  (let ((length (length (perject-tab-assert-tabs proj))))
	(when (> num length)
	  (user-error "Index %s is too large, as the project `%s' only has %s tabs"
				  num (perject-project-to-string proj) length)))
  (when (< num 1)
	(user-error "Index must be positive but is %s" num)))

(defun perject-tab-next-index (num length)
  "Return the index after NUM with cycling (the index before the last one is 1).
LENGTH is the total number of indices. If it is 0, return nil."
  (when (> length 0) (1+ (mod num length))))

(defun perject-tab-previous-index (num length)
  "Return the index before NUM with cycling (the index before 1 is the last one).
LENGTH is the total number of indices. If it is 0, return nil."
  (when (> length 0) (1+ (mod (- num 2) length))))


;;;; Helper Functions

(defun perject-tab-mode-line-indicator (proj tabs current _)
  "Return a string for the mode line indicator of perject-tab.
PROJ is the current project, TABS is the list of tabs belonging to that proect
and CURRENT is the current index."
  (and (cdr proj)
	   ;; If there are no brackets, use the ones from the first entry
	   (let ((brackets
			  (or (and current
					   (cdr (alist-get (perject-tab-state current proj)
										perject-tab-states nil nil #'string-equal)))
				  (cddar perject-tab-states))))
		 (propertize (format "%s%s/%s%s" (car brackets) (or current 0) (length tabs)
							 (cadr brackets))
					 'face 'perject-tab-mode-line-face))))

(defun perject-tab-no-tab ()
  "Inform the user that there is no tab."
  (message "There currently is no tab for the project `%s'"
		   (perject-project-to-string (perject-current))))

(defun perject-tab-no-recent ()
  "Inform the user that there is no recent tab."
  (message "There currently is no recent tab for the project '%s'"
		   (perject-project-to-string (perject-current))))

(defun perject-tab--tabs-function (&optional frame)
  "Return a list of tabs belonging to the frame FRAME.
This takes the current project into account."
  (perject-tab-tabs (perject-current frame)))

(defun perject-tab-get-visible-buffers ()
  "Return all visible buffers in the selected frame."
  (mapcar #'window-buffer (window-list nil 0)))

(defun perject-tab--dynamic-state (_ tab)
  "Return non-nil if the same buffers from the tab TAB are currently visible.
This function ignores the window positions and whether the same buffer is
displayed multiple times."
  (let ((buffers (alist-get 'buffers tab)))
	(or (not buffers) (seq-set-equal-p buffers (perject-tab-get-visible-buffers)))))


;;;; Hook Functions for Perject

;; After loading the tabs from the desktop file, some data (like the window
;; configuration wc) is missing. We thus regenerate the tabs "on demand" (i.e.
;; after having switched to it) in `perject-tab-switch' (which see), so we need
;; to do it manually for the "starting indices" (i.e. the current ones).
;; This also updates the tab according to its state. We have to use
;; `perject-tab-switch' instead of just `perject-tab-set', because the frame
;; saved by desktop is restored with its previous window configuration. If there
;; is e.g. an immutabe tab, we need to "reset" the window configuration to that
;; of the tab.
;; In case multiple frames show the same tab of the same project, the order of
;; the frames in the list matters.
(defun perject-tab--init (name)
  "Initialize the tabs for the collection named NAME.
More precisely, the current tabs of all frames belonging to a project of the
collection are regenerated."
  (dolist (proj (perject-get-projects name))
	(when (perject-tab-tabs proj)
	  (dolist (frame (perject-get-frames proj))
		(if-let ((current (perject-tab-index 'current proj frame)))
			(with-selected-frame frame
			  (perject-tab-switch current))
		  (warn "Frame belonging to project '%s' has no current index"
				(perject-project-to-string proj)))))))

(defun perject-tab--deserialize-tabs (tabs name)
  "Given the tabs TABS of NAME, return a new value for `perject-tab--tabs'.
NAME is a collection name."
  ;; If there already are any tabs belonging to those projects (which should
  ;; never happen) replace them.
  (let ((projects (perject-get-projects name)))
	(append tabs
			(cl-remove-if (lambda (pair) (member (car pair) projects)) perject-tab--tabs))))

(defun perject-tab--serialize-tabs (tabs name)
  "Extract the tabs of NAME from the list of tabs TABS and return the result.
NAME is a collection name."
  (let* ((projects (perject-get-projects name))
		 (alist (cl-remove-if-not (lambda (pair) (member (car pair) projects)) tabs))
		 ;; Filter the remaining tabs like `frameset-filter-tabs' does.
		 (filtered-alist
		  (mapcar
		   (lambda (pair)
			 (cons (car pair)
				   (mapcar
					(lambda (tab)
					  (seq-reduce
					   (lambda (current param)
						 (assq-delete-all param current))
					   (append '(wc wc-point wc-bl wc-bbl wc-history-back wc-history-forward)
							   (mapcar #'car perject-tab-extra-data))
					   tab))
					(cdr pair))))
		   alist)))
    filtered-alist))

(defun perject-tab--rename (proj new-proj)
  "React to perject renaming a collection or project from PROJ to NEW-PROJ."
  (if (stringp proj)
	  (cl-flet ((fun (elem)
				  (if (equal (caar elem) proj)
					  (cons (cons new-proj (cdar elem)) (cdr elem))
					elem)))
		(setq perject-tab--tabs
			  (mapcar #'fun perject-tab--tabs))
		(dolist (frame (frame-list))
		  (set-frame-parameter frame 'perject-tab
							   (mapcar #'fun (frame-parameter frame 'perject-tab)))))
	(when-let ((tabs (assoc proj perject-tab--tabs)))
	  (setcar tabs new-proj))
	(dolist (frame (frame-list))
	  (when-let ((indices (assoc proj (frame-parameter frame 'perject-tab))))
		(setcar indices new-proj)))))

(defun perject-tab--close-or-delete (proj &rest _)
  "React to perject closing or deleting a collection or project PROJ."
  (if (stringp proj)
	  (cl-flet ((fun (elem) (equal (caar elem) proj)))
		(setq perject-tab--tabs (cl-remove-if #'fun perject-tab--tabs))
		(dolist (frame (frame-list))
		  (set-frame-parameter frame 'perject-tab
							   (cl-remove-if #'fun (frame-parameter frame 'perject-tab)))))
	(setq perject-tab--tabs (assoc-delete-all proj perject-tab--tabs))
	(dolist (frame (frame-list))
	  (set-frame-parameter frame 'perject-tab
						   (assoc-delete-all proj (frame-parameter frame 'perject-tab))))))

(defun perject-tab--switch (old-proj proj frame)
  "React to perject switching from OLD-PROJ to PROJ in frame FRAME.
OLD-PROJ may be a collection or a project."
  ;; If the current tab of the old project is mutable, update it.
  (when-let ((index (and (consp old-proj)
						 (perject-tab-index 'current old-proj frame)))
			 ((perject-tab-update-p index old-proj frame)))
	(perject-tab-set index frame old-proj))
  (when (and (consp proj) (perject-tab-tabs proj))
	(with-selected-frame frame
	  (perject-tab-switch (or (perject-tab-index 'current proj frame) 1) 'ignore))))



(provide 'perject-tab)
;;; perject-tab.el ends here
