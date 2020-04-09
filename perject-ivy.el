;; Integrate perject with ivy.
;;
;; (require 'perject)
(require 'ivy)

(defun perject-ivy-switch-buffer (arg)
  "Switch to another buffer, respecting the current project.
If ARG is non-nil (in interactive use, if a prefix argument is supplied)
or if the current frame is not associated with any project,
the buffers from all projects are displayed."
  (interactive "P")
  (let ((name (perject--current-project)))
    (if (or arg (not name))
        (ivy-switch-buffer)
      (ivy-read "Switch to buffer: " #'internal-complete-buffer
                :predicate (lambda (buffer-name-and-buffer)
                             (member (cdr buffer-name-and-buffer) (perject--get-buffers name)))
                :keymap ivy-switch-buffer-map
                :preselect (buffer-name (other-buffer (current-buffer)))
                :action #'ivy--switch-buffer-action
                :matcher #'ivy--switch-buffer-matcher
                :caller 'ivy-switch-buffer))))

;; (global-set-key [remap ivy-switch-buffer]
;;       'perject-ivy-switch-buffer)
