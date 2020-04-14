;; Integrate perject with ibuffer.

;; (require 'perject) ;; leads to error.
(require 'ibuffer)

;; (defvar perject-ibuffer--predicates nil
;;   "Internal variable used to store the value of `ibuffer-maybe-show-predicates'.")

;; Frame parameter perject-ibuffer
;;   "Internal variable to restore the proper set of buffers when refreshing ibuffer.
;; If the currently shown buffers are restricted to a project, the value of this
;; variable should be the name of that project.
;; Otherwise, this variable should be 0."


;;; Keymap
(defvar perject-ibuffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap ibuffer-update] 'perject-ibuffer-update)
    map)
  "Keymap used when `perject-ibuffer-mode' is active.")

(define-minor-mode perject-ibuffer-mode
  "Toggle perject-ibuffer minor mode on or off.
Turn the mode on if ARG is positive and off otherwise.
The mode integrates ibuffer with perview, i.e. the displayed buffers
are those which belong to the current project.

Defined bindings (in `perject-ibuffer-mode-map'):
\\{perject-ibuffer-mode-map}."
  :group 'perject
  :keymap perject-ibuffer-mode-map
  (unless (eq major-mode 'ibuffer-mode)
    (user-error "`perject-ibuffer-mode' only works with `ibuffer-mode'."))
  (if perject-ibuffer-mode
      (let* ((name (perject--current-project))
             (buffer (get-buffer (perject-ibuffer--buffer-name name))))
        (if buffer
            (switch-to-buffer buffer)
          (rename-buffer (perject-ibuffer--buffer-name name)))
        (perject-ibuffer-update nil))
    (ibuffer-update nil)
    ;; Kill all other ibuffer buffers.
    (dolist (buffer (buffer-list))
      (when (and (eq (with-current-buffer buffer
                       major-mode)
                     'ibuffer-mode)
                 (not (eq buffer (current-buffer))))
        (kill-buffer buffer)))
    (rename-buffer "*Ibuffer*")))

  ;; previously had this in the minor mode:
  ;; (if perject-ibuffer-mode
  ;;     (let ((name (perject--current-project)))
  ;;       (when name
  ;;         (setq perject-ibuffer--predicates
  ;;               (copy-sequence ibuffer-maybe-show-predicates)
  ;;               perject-ibuffer--predicates
  ;;               (cons (-compose #'not (-rpartial 'perject--is-assoc-with name))
  ;;                     ibuffer-maybe-show-predicates))
  ;;         (perject-ibuffer-update nil)))
  ;;   (when perject-ibuffer--predicates
  ;;     (setq ibuffer-maybe-show-predicates perject-ibuffer--predicates
  ;;           perject-ibuffer--predicates nil)))
;; Inspired by the implementation of ibuffer support in perspective.el.
;; (defun perject-ibuffer (arg)
;;   "Call ibuffer, but restrict it to the current project.
;; If ARG is non-nil (in interactive use, if a prefix argument is supplied) or if
;; the current frame is not associated with any project, the buffers from all
;; projects are shown."
;;   (interactive "P")
;;   (let ((name (perject--current-project)))
;;     (if (or arg (not name))
;;         (ibuffer)
;;       (let ((ibuffer-maybe-show-predicates
;;              (cons (-compose #'not (-rpartial 'perject--is-assoc-with name))
;;                    ibuffer-maybe-show-predicates)))
;;         ;; (lambda (buffer) (not (perject--is-assoc-with buffer name)))
;;         (ibuffer)
;;         (local-set-key [remap ibuffer-update] 'perject-ibuffer-update)))))




(defun perject-ibuffer-update (arg &optional silent)
  "Regenerate the list of all buffers while restricting to the current project.
If ARG is nil (in interactive use, if no prefix argument is supplied) only the
buffers are displayed that belong to the current project, if existent.
If ARG is equal to '(4) (in interactive use, if a single prefix argument is
supplied) or if the current frame is not associated with any project, the
buffers from all projects are shown.
Otherwise, toggle whether buffers that match `ibuffer-maybe-show-predicates'
should be displayed.
If optional arg SILENT is non-nil, do not display progress messages.
This is basically a small wrapper around `ibuffer-update'."
  (interactive "P")
  (let ((name (perject--current-project)))
    (if (or (equal arg '(4)) (not name))
        (ibuffer-update nil silent)
      (let ((ibuffer-maybe-show-predicates
             (cons (-compose #'not (-rpartial 'perject--is-assoc-with name))
                   ibuffer-maybe-show-predicates)))
            ;; (buffer (get-buffer (perject-ibuffer--buffer-name name)))
        ;; (if buffer
        ;;     (switch-to-buffer buffer)
        ;;   (rename-buffer (perject-ibuffer--buffer-name name)))
        (ibuffer-update arg silent)))))

(defun perject-ibuffer-add-to-project (arg)
  "Add the marked buffers or the buffer at point to the current project.
If ARG is non-nil (in interactive use, if a prefix argument is supplied) or if
the current frame is not associated with any project, ask the user for the
project."
  (interactive "P")
  (let ((name (and (not arg) (perject--current-project)))
        (buffers (ibuffer-marked-buffer-names)))
    (if buffers
        (let ((buffers-added nil)
              (name (or
                     name
                     (perject--get-active-project
                      "Add marked buffers to project: "
                      nil
                      "There currently is no active project."))))
          (dolist (buffer-name buffers)
            (let ((buffer (get-buffer buffer-name)))
              (when (and (not (perject--is-assoc-with buffer name))
                         (buffer-live-p buffer))
                (perject--add-buffer-to-project buffer name nil)
                (push buffer-name buffers-added))))
          (perject-ibuffer-update)
          (pcase buffers-added
            ('nil
             (message "No buffers added. All selected buffers are already associated with the project '%s'."
                      name))
            (`(,_)
             (message "Added buffer '%s' to project '%s'." (car buffers-added) name))
            (_
             (message "Added the following buffers to project '%s': %s"
                      name (car (append (-interpose ", " buffers-added)))))))
      (perject--add-buffer-to-project
       (ibuffer-current-buffer t) name perject-add-buffer-message))))

(defun perject-ibuffer-remove-from-project (arg)
  "Remove the marked buffers or the buffer at point from the current project.
If ARG is non-nil (in interactive use, if a prefix argument is supplied) or if
the current frame is not associated with any project, ask the user for the
project."
  (interactive "P")
  (let ((name (and (not arg) (perject--current-project)))
        (buffers (ibuffer-marked-buffer-names)))
    (if buffers
        (let ((buffers-removed nil)
              (name (or
                     name
                     (perject--get-active-project
                      "Remove marked buffers from project: "
                      nil
                      "There currently is no active project."))))
          (dolist (buffer-name buffers)
            (let ((buffer (get-buffer buffer-name)))
              (when (perject--is-assoc-with buffer name)
                (perject--remove-buffer-from-project buffer name nil)
                (push buffer-name buffers-removed))))
          (pcase buffers-removed
            ('nil
             (message "No buffers removed. None of the selected buffers were associated with the project '%s'."
                      name))
            (`(,_)
             (message "Removed buffer '%s' from project '%s'." (car buffers-removed) name))
            (_
             (message "Removed the following buffers from project '%s': %s"
                      name (car (append (-interpose ", " buffers-removed)))))))
      (perject--remove-buffer-from-project
       (ibuffer-current-buffer t) name perject-add-buffer-message))))

(defun perject-ibuffer--buffer-name (name)
  "Return the name of the ibuffer buffer showing the buffers of the project named NAME.
NAME should not be nil."
  (concat "*Ibuffer-" name "*"))
