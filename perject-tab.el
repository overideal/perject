;;; perject-tab.el --- Tab-tab integration for Perject -*- lexical-binding: t -*-


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

(defface perject-tab-mode-line-face '((t :foreground "dark orange"))
  "The face used by the mode line indicator of perject tabs.")

(defcustom perject-tab-mode-line-format #'perject-tab-mode-line-indicator
  "This variable determines the mode line indicator of perject-tab.
It may have one of the following two values:
- nil: No mode line entry is shown for perject tab.
- A function: Call that function with four arguments: A dotted pair representing
  the current project, the list of tabs of that project, the current index and
  the recent index. The function should return the string to display in the mode
  line.
By default, the function `perject-tab-mode-line-indicator' is used. It displays
\"[1/4]\" if the user is at the first tab out of four and \"⟨1/4⟩\" if that tab
is also mutable."
  :type '(choice
		  (const :tag "No mode line entry is shown for perject" nil)
		  (function :tag "Custom function")))

(defcustom perject-tab-name-function tab-bar-tab-name-function
  "Function to get a tab name for perject.
This variable acts exactly like `tab-bar-tab-name-function' and exists for the reason
that a user might want a different naming scheme to apply to perject tabs than
to ordinary ones."
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
  :type 'integer)

(defcustom perject-tab-messages
  '(create delete toggle-mutable set reset index switch next previous recent)
  "Whether to print informative messages when performing certain actions.
The value of this variable is a list which may contain any of the following
symbols, whose presence in the list leads to a message being printed in the
indicated command:
- 'create: `perject-tab-create',
- 'delete: `perject-tab-delete',
- 'toggle-mutable: `perject-tab-toggle-mutable',
- 'set: `perject-tab-set'
- 'reset: `perject-tab-reset',
- 'index: `perject-tab-increment-index', `perject-tab-decrement-index',
- 'switch: `perject-tab-switch',
- 'next: `perject-tab-next',
- 'previous: `perject-tab-previous',
- 'recent: `perject-tab-recent'."
  :type '(set
		  (const :tag "`perject-tab-create'" create)
		  (const :tag "`perject-tab-delete'" delete)
		  (const :tag "`perject-tab-toggle-mutable'" toggle-mutable)
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

(defcustom perject-tab-index-after-delete 'recent-previous
  "This variable determines the new index after deleting a tab.
This influences the command `perject-tab-delete'.
It may have one of the following values:
- 'next: Use the next index.
- 'previous: Use the previous index.
- 'recent-next: Use the index of the most recently selected tab after deleting.
  If there is no recent index, use the next index.
- 'recent-previous: Use the index of the most recently selected tab after deleting.
  If there is no recent index, use the previous index.
After deleting, perject will also open the corresponding tab if
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


;;;; Hooks

(defcustom perject-tab-create-hook nil
  "Hook run after a new tab was created using `perject-tab-create'.
The functions are called with one argument, namely a dotted pair representing
the project."
  :type 'hook)

(defcustom perject-tab-delete-hook nil
  "Hook run after a tab was deleted using `perject-tab-delete'.
The functions are called with one argument, namely a dotted pair representing
the project."
  :type 'hook)

(defcustom perject-tab-switch-hook nil
  "Hook run after switching tabs using `perject-tab-switch'.
The functions are called with three arguments: a dotted pair representing the
project, the old index and the new index that was switched to."
  :type 'hook)

(defcustom perject-tab-toggle-mutable-hook nil
  "Hook run after toggling the \"mutable\" status of a tab using `perject-tab-toggle-mutable'.
The functions are called with two arguments, namely a dotted pair representing
the project and the index being toggled.")



;;;; Internal Variables

(defvar perject-tab--tabs nil
  "Alist saving the list of tabs (cdr) for each project (car).")

(defvar perject-tab--tabs-current nil
  "Internal variable used to save and restore the tabs.
It is a list with each entry being a list with car a project and remaining
values the tabs of that project.
Should not be modified by the user.")


;;;; The Major Mode

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
							(perject--current) (perject-tab-tabs)
							(perject-tab-index 'current) (perject-tab-index 'recent))))
                (cdr (last mode-line-misc-info))))
		(add-to-list 'desktop-globals-to-save 'perject-tab--tabs-current)
		(add-hook 'perject-desktop-after-load-hook #'perject-tab--load)
		(add-hook 'perject-desktop-save-hook #'perject-tab--save)
		(add-hook 'perject-rename-hook #'perject-tab--rename)
		;; This hook also covers the case that an active collection is deleted.
		(add-hook 'perject-after-close-hook #'perject-tab--close-or-delete)
		(add-hook 'perject-after-delete-project-hook #'perject-tab--close-or-delete)
		(add-hook 'perject-after-switch-hook #'perject-tab--switch)
		)

	;; Remove the added hooks etc.
	(setq desktop-globals-to-save (delete 'perject-tab--tabs-current desktop-globals-to-save))
	(remove-hook 'perject-desktop-after-load-hook #'perject-tab--load)
	(remove-hook 'perject-desktop-save-hook #'perject-tab--save)
	(remove-hook 'perject-rename-hook #'perject-tab--rename)
	(remove-hook 'perject-after-close-hook #'perject-tab--close-or-delete)
	(remove-hook 'perject-after-delete-project-hook #'perject-tab--close-or-delete)
	(remove-hook 'perject-after-switch-hook #'perject-tab--switch)))


;;;; Public Interface

(defun perject-tab-tabs (&optional proj)
  "Return the list of perject tabs for project PROJ.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case it defaults to the current project."
  (alist-get (or proj (perject--current)) perject-tab--tabs nil nil #'equal))

(defun perject-tab-index (symbol &optional proj frame)
  "Return the index of SYMBOL for project PROJ in the frame FRAME.
SYMBOL may be 'current or 'recent, which references the current or most recent
index of FRAME, respectively. If FRAME is nil, use the current frame.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case it defaults to the current project."
  (let* ((proj (or proj (perject--current)))
		 (index
		  (funcall (if (eq symbol 'current) #'car #'cdr)
				   (alist-get proj (frame-parameter frame 'perject-tab) nil nil #'equal)))
		 (length (length (perject-tab-tabs proj))))
	(when (and (numberp index) (> length 0))
	  (if (> index length)
		  (progn
			(message "Warning: Index too large")
			(setf index length))
		index))))

(defun perject-tab-mutable-p (num &optional proj)
  "Return non-nil if the tab of project PROJ at index NUM is mutable.
When a tab is mutable, switching from that tab to another one will save the
current window configuration to that tab.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case the current project of the selected frame is
used."
  (assoc 'mutable (nth (1- num) (perject-tab-tabs proj))))

(defun perject-tab-make (&optional frame)
  "Create a new tab corresponding to the current window configuration in frame FRAME.
If nil, FRAME defaults to the selected frame."
  (let ((tab-bar-tab-name-function perject-tab-name-function)
		(tab-bar-tab-name-truncated-max perject-tab-name-truncated-max))
	(tab-bar--tab frame)))

(defun perject-tab-collection-tabs (name)
  "Return the list of all tabs belonging to some project within the collection named NAME."
  (seq-mapcat #'cdr
			  (cl-remove-if-not (apply-partially #'equal name) perject-tab--tabs :key #'caar)))

(defun perject-tab-assert-tabs (proj)
  "Ensure that the project PROJ has at least one tab and return the list of tabs.
If there are no tabs, throw an error."
  (or (perject-tab-tabs proj)
	  (user-error "The %sproject has no tabs"
				  (if (equal proj (perject--current)) "current " ""))))

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


;;;; Commands

;;;;; Managing Tabs

(defun perject-tab-create (&optional proj frame msg)
  "Save the current window configuration within frame FRAME as a new tab for the project PROJ.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case the current project of FRAME is used. If nil,
FRAME defaults to the selected one.If MSG is non-nil, also print a message.
In interactive use, FRAME is the selected frame and the value of MSG is
determined by `perject-tab-messages'.
After creating the tab, this function runs the hook
`perject-tab-create-hook'."
  (interactive (list (perject--current) nil (memq 'create perject-tab-messages)))
  (let* ((proj (or proj (perject-assert-project frame)))
		 (index (or (perject-tab-index 'current proj frame) 0)))
	(if (assoc proj perject-tab--tabs)
		(setf (nthcdr index (alist-get proj perject-tab--tabs nil nil #'equal))
			  (cons (perject-tab-make frame) (nthcdr index (alist-get proj perject-tab--tabs nil nil #'equal))))
	  (push (list proj (perject-tab-make frame)) perject-tab--tabs))
	(perject-tab--set-index 'recent (perject-tab-index 'current proj frame) proj frame)
	(perject-tab--set-index 'current (1+ index) proj frame)
	(when msg
	  (message "Created tab for project '%s'" (perject-project-to-string proj)))
	(run-hook-with-args 'perject-tab-create-hook proj)))

(defun perject-tab-delete (&optional proj frame msg)
  "Delete the current tab within frame FRAME belonging to the project PROJ.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case the current project of FRAME is used. If nil,
FRAME defaults to the selected one. If MSG is non-nil, also print a message.
In interactive use, FRAME is the selected frame and the value of MSG is
determined by `perject-tab-messages'.
After deleting the tab, this function runs the hook
`perject-tab-delete-hook'."
  (interactive (list (perject--current) nil (memq 'delete perject-tab-messages)))
  (let* ((proj (or proj (perject-assert-project frame)))
		 (tabs (perject-tab-assert-tabs proj))
		 (length (length tabs))
		 (index (perject-tab-index 'current proj frame))
		 (recent (perject-tab-index 'recent proj frame)))
	;; Remove the tab from the list.
	(if (eq index 1)
		(setcdr (assoc proj perject-tab--tabs) (cdr tabs))
	  (setf (nthcdr (1- index) tabs) (nthcdr index tabs)))
	;; Update the recent index.
	(cond
	 ((eq recent index)
	  (setq recent nil))
	 ((and recent (> recent index))
	  (setq recent (1- recent))))
	;; Update the current index.
	(if (and recent (memq perject-tab-index-after-delete '(recent-next recent-previous)))
		(progn (perject-tab--set-index 'current recent proj frame)
			   (setq recent nil))
	  (when (memq perject-tab-index-after-delete '(previous recent-previous))
		(perject-tab--set-index 'current (perject-tab--previous-index index (1- length)) proj frame)))
	(perject-tab--set-index 'recent recent proj frame)
	(when (and perject-tab-switch-after-delete
			   (perject-tab-index 'current proj frame))
	  (perject-tab-switch (perject-tab-index 'current proj frame)))
	(when msg
	  (message "Deleted tab of project '%s'" (perject-project-to-string proj)))
	(run-hook-with-args 'perject-tab-delete-hook proj)))

(defun perject-tab-toggle-mutable (num &optional proj msg)
  "Toggle the \"mutable\" status of the tab of project PROJ at index NUM.
When a tab is mutable, switching from that tab to another one will save the
current window configuration to that tab.
PROJ is a dotted pair with car a collection and cdr a project name. It may also
be nil, in which case the current project of the selected frame is used.
In interactive use, NUM defaults to the current index and PROJ to the
current project.
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'."
  (interactive (list (perject-tab-index 'current)
					 (perject--current)
					 (memq 'toggle-mutable perject-tab-messages)))
  (let* ((proj (or proj (perject-assert-project)))
		 (tabs (progn (perject-tab-assert-index num proj) (perject-tab-tabs proj)))
		 (mutable (perject-tab-mutable-p num proj)))
	(if mutable
		(setf (nth (1- num) tabs) (assoc-delete-all 'mutable (nth (1- num) tabs)))
	  (setf (nth (1- num) tabs) (append (nth (1- num) tabs) (list (cons 'mutable nil)))))
    (when msg
	  (if (and (eq (perject-tab-index 'current) num)
			   (equal proj (perject--current)))
		  (message "Tab is now %s" (if mutable "immutable" "mutable"))
		(message "Tab %s of project '%s' is now %s"
				 num (perject-project-to-string proj)
				 (if mutable "immutable" "mutable"))))
	(run-hook-with-args 'perject-tab-toggle-mutable-hook proj num)))

(defun perject-tab-set (num &optional tab-or-frame proj msg)
  "Set the tab with index NUM in project PROJ to TAB-OR-FRAME.
TAB-OR-FRAME may be a tab, a frame or nil. If it is a frame, use the current
window configuration of that frame, defaulting to the selected frame if
TAB-OR-FRAME is nil. If TAB-OR-FRAME is not a tab, ensure that the newly created
tab has the same \"mutable\" status as the tab it is replacing.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case it defaults to the current project.
In interactive use, set the tab of the current index to the current window
configuration.
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'."
  (interactive
   (list (perject-tab-index 'current) nil (perject--current) (memq 'set perject-tab-messages)))
  (let* ((proj (or proj
				   (perject-assert-project (and (framep tab-or-frame) tab-or-frame))))
		 ;; If a new tab is created, ensure that the mutability stays the same.
		 (tab (progn (perject-tab-assert-index num proj)
					 (if (or (framep tab-or-frame) (null tab-or-frame))
						 (if (perject-tab-mutable-p num proj)
							 (append (perject-tab-make tab-or-frame) (list (cons 'mutable nil)))
						   (perject-tab-make tab-or-frame))
						  tab-or-frame))))
	(setf (nth (1- num) (perject-tab-tabs proj)) tab)
	(when msg
	  (if (and (eq (perject-tab-index 'current) num)
			   (equal proj (perject--current)))
		  (message "Updated current tab"))
	  (message "Updated tab %s of project '%s'" num (perject-project-to-string proj)))))

(defun perject-tab-increment-index (&optional msg)
  "Increase the index of the current tab by one.
This effectively interchanges the current tab with its successor.
If it is the tab with the highest index, it becomes the tab with the lowest
index 1 (and all other tabs increase their index by one).
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'."
  (interactive (list (memq 'index perject-tab-messages)))
  (perject-assert-project)
  (let ((tabs (perject-tab-assert-tabs (perject--current)))
		(index (perject-tab-index 'current))
		(recent (perject-tab-index 'recent)))
	;; Change the order of the tabs and update the recent index.
	(if (eq index (length tabs))
		;; The current tab is the last one.
		(progn
		  (setcdr (assoc (perject--current) perject-tab--tabs)
				  (cons (car (last tabs)) (butlast tabs)))
		  (when (and recent (not (eq recent index)))
			;; Recent and index should never be equal anyway.
			(perject-tab--set-index 'recent (1+ recent))))
	  (setcdr (assoc (perject--current) perject-tab--tabs)
			  (append
			   (seq-take tabs (1- index))
			   (list (nth index tabs) (nth (1- index) tabs))
			   (seq-drop tabs (1+ index))))
	  (when (eq recent (perject-tab--next-index index (length tabs)))
		(perject-tab--set-index 'recent index)))
	(perject-tab--set-index 'current (perject-tab--next-index index (length tabs)))
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
  (let ((tabs (perject-tab-assert-tabs (perject--current)))
		(index (perject-tab-index 'current))
		(recent (perject-tab-index 'recent)))
	(if (eq index 1)
		;; The current tab is the first one.
		(progn
		  (setcdr (assoc (perject--current) perject-tab--tabs)
				  (append (cdr tabs) (list (car tabs))))
		  (when (and recent (not (eq recent index)))
			(perject-tab--set-index 'recent (1- recent))))
	  (setcdr (assoc (perject--current) perject-tab--tabs)
			  (append
			   (seq-take tabs (- index 2))
			   (list (nth (1- index) tabs) (nth (- index 2) tabs))
			   (seq-drop tabs index)))
	  (when (eq recent (perject-tab--previous-index index (length tabs)))
		(perject-tab--set-index 'recent index)))
	(perject-tab--set-index 'current (perject-tab--previous-index index (length tabs)))
	(when msg
	  (message "Changed index of tab: %s -> %s" index (perject-tab-index 'current)))))


;;;;; Switching Tabs

(defun perject-tab-switch (num &optional set-old msg)
  "Switch to the tab with index NUM (starting at one) of the current project in the selected frame.
If the old tab (the one before switching) is mutable, also update its value to
the current window configuration.
This is the behavior in interactive use, but when called as a function it is
influenced by the optional argument SET-OLD. If it is nil, behave as above. If
it is t, always update and if it is 'ignore, never update the old tab. 
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'.
After switching tabs, this function runs the hook
`perject-tab-switch-hook'."
  (interactive
   (list current-prefix-arg (perject--current)
		 (memq 'switch perject-tab-messages)))
  (let ((proj (perject-assert-project))
		(old-index (perject-tab-index 'current)))
	(perject-tab-assert-index num proj)
	;; If the current tab is mutable and the SET-OLD allows it, update the tab.
	(when (or (eq set-old t)
			  (and (not (eq set-old 'ignore)) old-index ;; (not (eq num old-index))
				   (perject-tab-mutable-p old-index)))
	  (perject-tab-set old-index))
	(let ((tab-bar-tabs-function #'perject-tab--tabs-function)
		  (current (nth (1- num) (perject-tab-tabs)))
		  ;; Silence `tab-bar-select-tab'.
		  (tab-bar-mode t))
	  (tab-bar-select-tab num)
	  ;; `tab-bar' replaces the new tab by a pseudo tab indicating that it is current,
	  ;; but not saving the information we need (e.g. for other frames holding the same project).
	  ;; Thus we change the offending value back.
      (setf (nth (1- num) (perject-tab-tabs)) current)
	  ;; After restoring a tab from the desktop, many entries of the tab (e.g. the
	  ;; window configuration wc) are nil. In that case, we refresh the tab after
	  ;; switching to it, creating the data.
	  (unless (window-configuration-p (alist-get 'wc current))
		(perject-tab-set num)))
	;; If the new index is the same as the old one, do not update recent.
	(unless (eq num (perject-tab-index 'current))
	  (perject-tab--set-index 'recent (perject-tab-index 'current)))
	(perject-tab--set-index 'current num)
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
		(perject-tab-switch (perject-tab--next-index (perject-tab-index 'current)
													 (length tabs)))
		(when msg
		  (message "Switched to next tab of project '%s'"
				   (perject-project-to-string (perject--current)))))
	(funcall perject-tab-no-next)))

(defun perject-tab-previous (&optional msg)
  "Switch to the previous tab for the current project in the selected frame.
If MSG is non-nil, also print a message. In interactive use, this is determined
by the value of `perject-tab-messages'."
  (interactive (list (memq 'previous perject-tab-messages)))
  (perject-assert-project)
  (if-let ((tabs (perject-tab-tabs)))
	  (progn
		(perject-tab-switch (perject-tab--previous-index (perject-tab-index 'current)
														 (length tabs)))
		(when msg
		  (message "Switched to previous tab of project '%s'"
				   (perject-project-to-string (perject--current)))))
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
				   (perject-project-to-string (perject--current)))))
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
			 index (perject-project-to-string (perject--current))))))


;;;; Internal Interface

(defun perject-tab--set-index (symbol num &optional proj frame)
  "Set the index of SYMBOL to NUM for project PROJ in the frame FRAME.
SYMBOL may be 'current or 'recent, which references the current or most recent
index of FRAME, respectively. NUM is a number or nil, in which case the
respective index is set to that value. If FRAME is nil, use the current frame.
PROJ is a dotted pair with car a collection and cdr a project name.
It may also be nil, in which case it defaults to the current project."
  (let* ((proj (or proj (perject--current frame)))
		 (data (alist-get proj (frame-parameter frame 'perject-tab) nil nil #'equal)))
	(if data
		(setf (if (eq symbol 'current) (car data) (cdr data)) num)
	  (push (list proj (cons (and (eq symbol 'current) num) (and (eq symbol 'recent) num)))
			(frame-parameter frame 'perject-tab)))))

(defun perject-tab--tabs-function (&optional frame)
  "Return a list of tabs belonging to FRAME taking the current project into account."
  (perject-tab-tabs (perject--current frame)))

(defun perject-tab--next-index (num length)
  "Return the index after NUM with cycling (i.e. the index before the last one is 1).
LENGTH is the total number of indices. If it is 0, return nil."
  (when (> length 0) (1+ (mod num length))))

(defun perject-tab--previous-index (num length)
  "Return the index before NUM with cycling (i.e. the index before 1 is the last one).
LENGTH is the total number of indices. If it is 0, return nil."
  (when (> length 0) (1+ (mod (- num 2) length))))


;;;; Helper Functions

(defun perject-tab-mode-line-indicator (proj tabs current _)
  "Return a string for the mode line indicator of perject-tab."
  (and (cdr proj)
	   (propertize (format (if (and current (perject-tab-mutable-p current proj)) "⟨%s/%s⟩" "[%s/%s]")
						   (or current 0) (length tabs))
				   'face 'perject-tab-mode-line-face)))

(defun perject-tab-no-tab ()
  "Inform the user that there is no tab."
  (message "There currently is no tab for the project `%s'"
		   (perject-project-to-string (perject--current))))

(defun perject-tab-no-recent ()
  "Inform the user that there is no recent tab."
  (message "There currently is no recent tab for the project '%s'"
		   (perject-project-to-string (perject--current))))


;;;; Hook Functions for Perject

(defun perject-tab--load (list)
  "Load the tabs of the projects belonging to the projects given by LIST."
  (let ((projects (mapcar (apply-partially #'cons (car list)) (cdr list))))
	;; If there already are any tabs belonging to those projects (which should
	;; never happen) replace them.
	(setq perject-tab--tabs
		  (append perject-tab--tabs-current
				  (cl-remove-if (lambda (pair) (member (car pair) projects))
								perject-tab--tabs)))))

(defun perject-tab--save (list)
  "Prepare for perject to save the tabs of the projects given by LIST."
  ;; Projects are calculated by the same mechanism as in `perject--list-projects'.
  (let* ((projects (mapcar (apply-partially #'cons (car list)) (cdr list)))
		 (alist (cl-remove-if-not (lambda (pair) (member (car pair) projects)) perject-tab--tabs))
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
					   '(wc wc-point wc-bl wc-bbl wc-history-back wc-history-forward)
					   tab))
					(cdr pair))))
		   alist)))
	(setq perject-tab--tabs-current filtered-alist)))

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
  "React to perject switching from the collection or project OLD-PROJ to PROJ in frame FRAME."
  (when (and (consp proj) (perject-tab-tabs proj))
	(with-selected-frame frame
	  ;; If the current tab of the old project is mutable, update it.
	  (when-let ((index (and (consp old-proj) (perject-tab-tabs proj)
							 (perject-tab-index 'current old-proj frame))))
		(when (perject-tab-mutable-p index old-proj)
		  (perject-tab-set index frame old-proj)))
	  (perject-tab-switch (or (perject-tab-index 'current proj frame) 1) 'ignore))))



(provide 'perject-tab)
;;; perject-tab.el ends here
