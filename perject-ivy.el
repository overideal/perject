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

;; Ivy seems to have a bug, with the curent implementation it happens sometimes that an error
;; "name not defined" or something like that occurs.
;; Using debug:
;; Debugger entered--Lisp error: (void-variable name)
;;   (perject--get-buffers name)
;;   (member (cdr buffer-name-and-buffer) (perject--get-buffers name))
;;   (lambda (buffer-name-and-buffer) (member (cdr buffer-name-and-buffer) (perject--get-buffers name)))((" *Minibuf-1*" . #<buffer  *Minibuf-1*>))
;;   internal-complete-buffer("" (lambda (buffer-name-and-buffer) (member (cdr buffer-name-and-buffer) (perject--get-buffers name))) t)
;;   all-completions("" internal-complete-buffer (lambda (buffer-name-and-buffer) (member (cdr buffer-name-and-buffer) (perject--get-buffers name))))
;;   ivy--buffer-list("" nil (lambda (buffer-name-and-buffer) (member (cdr buffer-name-and-buffer) (perject--get-buffers name))))
;;   ivy--reset-state(#s(ivy-state :prompt "Switch to buffer: " :collection internal-complete-buffer :predicate (lambda (buffer-name-and-buffer) (member (cdr buffer-name-and-buffer) (perject--get-buffers name))) :require-match nil :initial-input nil :history nil :preselect "resources.org" :keymap (keymap (11 . ivy-switch-buffer-kill)) :update-fn nil :sort nil :frame #<frame emacs@waterloo:lisp 0x55603f4e0410> :window #<window 12 on lisp> :buffer #<buffer *Help*> :text nil :action (1 ("o" ivy--switch-buffer-action "default") ("i" #f(compiled-function (x) #<bytecode 0x15580f6d7051>) "insert") ("w" #f(compiled-function (x) #<bytecode 0x15580f6d7061>) "copy") ("f" ivy--find-file-action "find file") ("j" ivy--switch-buffer-other-window-action "other window") ("k" ivy--kill-buffer-action "kill") ("r" ivy--rename-buffer-action "rename") ("x" counsel-open-buffer-file-externally "open externally")) :unwind nil :re-builder ivy--regex-ignore-order :matcher ivy--switch-buffer-matcher :dynamic-collection nil :display-transformer-fn ivy-rich--ivy-switch-buffer-transformer :directory "/home/mwj/m/6/doc/" :caller ivy-switch-buffer :current #("resources.org" 0 13 (face org-drawer)) :def nil :ignore t :multi-action nil :extra-props nil))
;;   ivy-recursive-restore()
;;   ivy--cleanup()
;;   #f(compiled-function () #<bytecode 0x15580f70de09>)()
;;   ivy-read("(): " ("../" "./") :predicate nil :initial-input "" :action ivy-completion-in-region-action :unwind #f(compiled-function () #<bytecode 0x1558114b4241>) :caller ivy-completion-in-region)
;;   ivy-completion-in-region(#<marker at 25 in  *Minibuf-1*> 25 #f(compiled-function (string pred action) #<bytecode 0x15580fe18321>) nil)
;;   completion-in-region(#<marker at 25 in  *Minibuf-1*> 25 #f(compiled-function (string pred action) #<bytecode 0x15580fe18321>) nil)
;;   completion-at-point()
;;   complete-symbol(nil)
;;   funcall-interactively(complete-symbol nil)
;;   call-interactively(complete-symbol nil nil)
;;   command-execute(complete-symbol)
;;   read-from-minibuffer("Open current file with: " nil (keymap (9 . completion-at-point) keymap (18 . counsel-minibuffer-history) (menu-bar keymap (minibuf "Minibuf" keymap (previous menu-item "Previous History Item" previous-history-element :help "Put previous minibuffer history element in the min...") (next menu-item "Next History Item" next-history-element :help "Put next minibuffer history element in the minibuf...") (isearch-backward menu-item "Isearch History Backward" isearch-backward :help "Incrementally search minibuffer history backward") (isearch-forward menu-item "Isearch History Forward" isearch-forward :help "Incrementally search minibuffer history forward") (return menu-item "Enter" exit-minibuffer :key-sequence "\15" :help "Terminate input and exit minibuffer") (quit menu-item "Quit" abort-recursive-edit :help "Abort input and exit minibuffer") "Minibuf")) (10 . exit-minibuffer) (13 . exit-minibuffer) (7 . minibuffer-keyboard-quit) (C-tab . file-cache-minibuffer-complete) (9 . self-insert-command) (XF86Back . previous-history-element) (up . previous-line-or-history-element) (prior . previous-history-element) (XF86Forward . next-history-element) (down . next-line-or-history-element) (next . next-history-element) (27 keymap (60 . minibuffer-beginning-of-buffer) (114 . previous-matching-history-element) (115 . next-matching-history-element) (112 . previous-history-element) (110 . next-history-element))) nil shell-command-history)
;;   apply(read-from-minibuffer "Open current file with: " nil (keymap (9 . completion-at-point) keymap (18 . counsel-minibuffer-history) (menu-bar keymap (minibuf "Minibuf" keymap (previous menu-item "Previous History Item" previous-history-element :help "Put previous minibuffer history element in the min...") (next menu-item "Next History Item" next-history-element :help "Put next minibuffer history element in the minibuf...") (isearch-backward menu-item "Isearch History Backward" isearch-backward :help "Incrementally search minibuffer history backward") (isearch-forward menu-item "Isearch History Forward" isearch-forward :help "Incrementally search minibuffer history forward") (return menu-item "Enter" exit-minibuffer :key-sequence "\15" :help "Terminate input and exit minibuffer") (quit menu-item "Quit" abort-recursive-edit :help "Abort input and exit minibuffer") "Minibuf")) (10 . exit-minibuffer) (13 . exit-minibuffer) (7 . minibuffer-keyboard-quit) (C-tab . file-cache-minibuffer-complete) (9 . self-insert-command) (XF86Back . previous-history-element) (up . previous-line-or-history-element) (prior . previous-history-element) (XF86Forward . next-history-element) (down . next-line-or-history-element) (next . next-history-element) (27 keymap (60 . minibuffer-beginning-of-buffer) (114 . previous-matching-history-element) (115 . next-matching-history-element) (112 . previous-history-element) (110 . next-history-element))) nil shell-command-history nil)
;;   read-shell-command("Open current file with: ")
;;   crux-open-with((4))
;;   funcall-interactively(crux-open-with (4))
;;   call-interactively(crux-open-with nil nil)
;;   command-execute(crux-open-with)
(provide 'perject-ivy)
