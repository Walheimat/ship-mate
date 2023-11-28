;;; ship-mate.el --- Consolidate projects and compilation -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/ship-mate
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

;;; Commentary:

;; Create project-scoped compilation targets. Each command has a
;; unique history per project and any execution will only bother you
;; about unsaved buffers within the project.

;;; Code:

(require 'project)
(require 'ring)
(require 'compile)

(defgroup ship-mate nil
  "Project-scoped compilation."
  :group 'ship-mate)

;;; -- Customization

(defcustom ship-mate-compile-functions '(project-compile recompile)
  "Functions to advise to be project-scoped."
  :group 'ship-mate
  :type '(repeat symbol))

(defcustom ship-mate-lighter " shp"
  "Lighter to indicate `ship-mate-mode'."
  :group 'ship-mate
  :type 'string)

(defcustom ship-mate-command-history-size 10
  "Size of the history per command."
  :group 'ship-mate
  :type 'integer)

(defcustom ship-mate-command-fuzzy-match-function #'ship-mate-command--fuzzy-match-p
  "Function to match a command against history entries.

The function will be called with two arguments: the command to
match against and the history of the last command category."
  :group 'ship-mate
  :type 'function)

(defcustom ship-mate-command-buffer-name-function-generator
 #'ship-mate-command--buffer-name-function
  "Generator function to name buffers.

This function will be called with the current project's name and
should return a function that `compilation-buffer-name-function'
can be set to."
  :group 'ship-mate
  :type 'function)

(defcustom ship-mate-dinghy-enable t
  "Whether to enable `ship-mate-dinghy-mode' in buffers."
  :group 'ship-mate
  :type 'boolean)

(defcustom ship-mate-prompt-for-hidden-buffer nil
  "Whether user should be prompted when hidden buffer is done."
  :group 'ship-mate
  :type 'boolean)

;;; -- Variables

(defvar ship-mate-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" #'ship-mate-hidden-recompile)
    map)
  "Command map for `ship-mate' commands.

Each command created by `ship-mate-create-command' will be
automatically bound in here using either the provided key or the
initial of the command's name.")

(defvar ship-mate-commands nil
  "List of commands and their per-project histories.

Each command created by `ship-mate-create-command' will
`plist-put' a new entry mapping a project's root to a command
history. The general structure is ([COMMAND-SYMBOL] .
HASH-MAP<PROJECT-ROOT, HISTORY>).")

(defvar-local ship-mate-command-category nil
  "The command category of this buffer.

This is set by `ship-mate-command'.")

(defvar ship-mate-command-history nil
  "The history of the currently executed command.")

(defvar ship-mate-command--current-command-name nil
  "The symbol name of the currently executed command.")
(defvar ship-mate-command--last-command nil
  "The symbol of the last executed command.")

(defvar ship-mate-environment nil
  "The project environment.

The value of this variable will be bound to
`compilation-environment'.

Ideally you bind this in in your .dir-locals file.")
(put 'ship-mate-environment 'safe-local-variable #'ship-mate-environment--valid-env-p)

(defvar ship-mate-hands-off-environment nil
  "Whether to not let-bind the environment.

This is set by `ship-mate-command--capture' to avoid resetting
the environment.")

;;; -- Commands

(defun ship-mate-command (cmd &optional arg)
  "Run CMD for the current project.

Each command will be stored in a per-project history. If the
history is non-empty, the user will not be prompted unless called
with a prefix argument ARG.

The `compilation-environment' is set from the project's
`ship-mate-environment'.

If the prefix argument ARG is 0, `comint-mode' will be used
instead of `compile-mode'."
  (let* ((project-vc-name nil)
         (current (project-current t))
         (root (project-root current))
         (name (project-name current))
         (lowercase (downcase name))

         ;; History.
         (table (plist-get ship-mate-commands cmd))
         (history (ship-mate-command--history cmd))
         (ship-mate-command-history (ring-elements history))

         ;; Reading user input.
         (initial (unless (ring-empty-p history)
                    (ring-ref history 0)))
         (ship-mate-command--current-command-name (symbol-name cmd))
         (comint (zerop (prefix-numeric-value arg)))
         (prompt (format "%s project (%s)%s"
                         (capitalize ship-mate-command--current-command-name)
                         name
                         (if comint " interactively: " ": ")))
         (command (or (and (not arg) initial)
                      (read-shell-command prompt initial 'ship-mate-command-history)))

         ;; Binding external variables.
         (default-directory (project-root current))
         (compilation-buffer-name-function (funcall ship-mate-command-buffer-name-function-generator lowercase)))

    (setq ship-mate-command--last-command cmd)

    (ring-remove+insert+extend history command)

    (puthash root history table)

    (let ((buffer (if compilation-environment
                      (compile command comint)
                    (let ((compilation-environment (ship-mate-command--last-environment-or-local-value)))
                      (compile command comint)))))

      (with-current-buffer buffer
        (setq ship-mate-command-category cmd)))))

(defun ship-mate-command--last-environment-or-local-value ()
  "Get the last environment for CMD or default."
  (if-let* ((buffer-name (funcall compilation-buffer-name-function nil))
            (buffer (get-buffer buffer-name)))

      (buffer-local-value 'compilation-environment buffer)

    (ship-mate--local-value 'ship-mate-environment)))

(defun ship-mate-command--fuzzy-match-p (command history)
  "Check if COMMAND matches previous commands in HISTORY."
  (and-let* ((elements (ring-elements history))
             (min-count 1)
             (match (seq-find
                     (lambda (it)
                       (let* ((el-parts (string-split it " "))
                              (matches (seq-count
                                        (lambda (part)
                                          (string-match-p part command))
                                        el-parts)))
                         (> matches min-count)))
                     elements)))))

(defun ship-mate-command--buffer-name-function (project)
  "Return a function to name the compilation buffer for PROJECT."
  (let* ((cmd (or ship-mate-command--current-command-name "compile"))
         (name (format "*ship-mate-%s-%s*" cmd project)))

    (lambda (_major-mode) name)))

(defun ship-mate-command--update-history (command &rest _)
  "Update history using COMMAND.

If COMMAND matches other commands of the last command category,
add it to the history."
  (and-let* (ship-mate-command--last-command
             (history (ship-mate-command--history ship-mate-command--last-command))
             (index (funcall ship-mate-command-fuzzy-match-function command history)))

    (ring-remove+insert+extend history command)))

(defun ship-mate-command--capture (recompile &optional edit)
  "Only call RECOMPILE conditionally.

If we're in a `ship-mate-command' buffer, call recompile after
setting the `compile-history'. Otherwise check if the current
`compile-command' matches the history of the previous command; if
that is the case just call `ship-mate-command' with that last
command again.

As a last resort call `recompile'.

EDIT is passed as-is to all invocations of RECOMPILE."
  (if-let* (((ship-mate--command-buffer-p))
            (cmd ship-mate-command-category)
            (history (ship-mate-command--history ship-mate-command-category)))
      (let ((compile-history (and history (ring-elements history))))
        (with-current-buffer (funcall-interactively recompile edit)
          (setq ship-mate-command-category cmd)))
    (if-let* ((command compile-command)
              (history (and ship-mate-command--last-command
                            (ship-mate-command--history ship-mate-command--last-command)))
              (matches (funcall ship-mate-command-fuzzy-match-function command history)))

        (ship-mate-command ship-mate-command--last-command edit)

      (funcall-interactively recompile edit))))

(defun ship-mate-command--history (cmd)
  "Access history for CMD.

If the history doesn't yet exist, create it using the provided
default.

The default can be a string or a list of strings. In the latter
case, they are inserted in reverse order so that the first item
is the default."
  (if-let ((history (ship-mate-command--existing-history cmd)))

      history

    (ship-mate-command--create-history cmd)))

(defun ship-mate-command--existing-history (cmd)
  "Get the existing history for CMD."
  (when-let* ((table (plist-get ship-mate-commands cmd))
              (project (project-current))
              (root (project-root project))
              (history (gethash root table)))

    history))

(defun ship-mate-command--create-history (cmd &optional empty)
  "Create history for CMD.

If EMPTY is t, do not read the defaults."
  (let* ((table (plist-get ship-mate-commands cmd))
         (project (project-current))
         (root (project-root project))
         (var (intern (format "ship-mate-%s-default-cmd" cmd)))
         (default (if empty nil (project--value-in-dir var root)))
         (new-history (make-ring ship-mate-command-history-size)))

    (cond
     ((listp default)
      (mapc (lambda (it) (ring-insert new-history it)) (reverse default)))
     ((stringp default)
      (ring-insert new-history default))
     (t nil))

    (puthash root new-history table)

    new-history))

(defun ship-mate-command--valid-default-p (val)
  "Check if VAL is a valid project command default."
  (or (stringp val)
      (and (listp val)
           (cl-every #'stringp val))))

(defun ship-mate-command--buffers ()
  "Get all `ship-mate' buffers in the current project."
  (cl-loop for buffer in (buffer-list)
           if (and (ship-mate--command-buffer-p buffer)
                   (equal default-directory
                          (buffer-local-value 'default-directory buffer)))
           collect buffer))

(defun ship-mate-command-next-buffer ()
  "Get the next buffer."
  (interactive)

  (let* ((buffers (ship-mate-command--buffers))
         (pos (cl-position (current-buffer) buffers)))

    (when (<= (length buffers) 1)
      (user-error "Only buffer"))

    (switch-to-buffer (nth (mod (1+ pos) (length buffers)) buffers))))

(defun ship-mate-command-prev-buffer ()
  "Get the next buffer."
  (interactive)

  (let* ((buffers (ship-mate-command--buffers))
         (pos (cl-position (current-buffer) buffers)))

    (when (<= (length buffers) 1)
      (user-error "Only buffer"))

    (switch-to-buffer (nth (mod (+ (length buffers) (1- pos)) (length buffers)) buffers))))

;;; -- Hidden recompilation

(defvar ship-mate-submarine--in-progress nil)
(defvar ship-mate-submarine--buffer nil)
(defvar ship-mate-submarine--timer nil)
(defvar ship-mate-submarine--process nil)

(defun ship-mate-submarine--recompile ()
  "Recompile but with no window."
  (when ship-mate-submarine--in-progress
    (user-error "Compilation in progress"))

  (let ((display-buffer-alist '(("\\*ship-mate" (display-buffer-no-window)))))

    (setq ship-mate-submarine--in-progress t
          ship-mate-submarine--buffer (recompile))))

(defun ship-mate-submarine--check ()
  "Check on the recorded process."
  (unless ship-mate-submarine--process
    (user-error "No process"))

  (unless (process-live-p ship-mate-submarine--process)
    (cancel-timer ship-mate-submarine--timer)

    (setq ship-mate-submarine--timer nil
          ship-mate-submarine--process nil
          ship-mate-submarine--in-progress nil)

    (or (and ship-mate-prompt-for-hidden-buffer
             (not (yes-or-no-p "Compilation finished. Show buffer?")))
        (pop-to-buffer ship-mate-submarine--buffer))))

(defun ship-mate-submarine--watch-process (process)
  "Save PROCESS and set timer to check on it."
  (when ship-mate-submarine--in-progress
    (setq ship-mate-submarine--process process
          ship-mate-submarine--timer (run-with-timer 0 0.1 #'ship-mate-submarine--check))))

;;; -- Global minor mode

(defun ship-mate-mode--setup ()
  "Setup `ship-mate-mode'."
  (advice-add 'compilation-start :after #'ship-mate-command--update-history)
  (advice-add 'recompile :around #'ship-mate-command--capture)

  (add-hook 'compilation-start-hook 'ship-mate-dinghy--maybe-enable)
  (add-hook 'compilation-start-hook 'ship-mate-submarine--watch-process)

  (dolist (fun (append ship-mate-compile-functions '(ship-mate-command)))
    (advice-add fun :around 'ship-mate-with-bounded-compilation)))

(defun ship-mate-mode--teardown ()
  "Tear down `ship-mate-mode'."
  (advice-remove 'compilation-start #'ship-mate-command--update-history)
  (advice-remove 'recompile #'ship-mate-command--capture)

  (remove-hook 'compilation-start-hook 'ship-mate-dinghy--maybe-enable)
  (remove-hook 'compilation-start-hook 'ship-mate-submarine--watch-process)

  (dolist (fun (append ship-mate-compile-functions '(ship-mate-command)))
    (advice-remove fun 'ship-mate-with-bounded-compilation)))

;;; -- Dinghy mode

(defvar ship-mate-dinghy-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-e" #'ship-mate-edit-environment)
    (define-key map "\C-c\C-n" #'ship-mate-command-next-buffer)
    (define-key map "\C-c\C-p" #'ship-mate-command-prev-buffer)

    map)
  "Map used in buffers that enable `ship-mate-dinghy-mode'.")

(define-minor-mode ship-mate-dinghy-mode
  "Minor mode to provide contextual information and bindings."
  :lighter " smd"

  (unless (derived-mode-p 'compilation-mode)
    (user-error "Can only be enabled in compilation buffers"))

  (ship-mate-dinghy--reset-header-line-format))

(defun ship-mate-dinghy--maybe-enable (&optional _process)
  "Enable `ship-mate-dinghy-mode' if not disabled."
  (when (and ship-mate-dinghy-enable
             (ship-mate--command-buffer-p))
    (ship-mate-dinghy-mode)))

(defun ship-mate-dinghy--print-variables ()
  "Pretty-print environment variables."
  (if compilation-environment
      (if (> (length compilation-environment) 3)
          (propertize "active"
                      'face 'mode-line-emphasis
                      'help-echo (string-join compilation-environment "; "))
        (propertize (mapconcat
                     (lambda (it)
                       (let ((parts (split-string it "=")))
                         (propertize (nth 0 parts) 'help-echo (format "Variable set to: %s" (nth 1 parts)))))
                     compilation-environment
                     " ")
                    'face 'mode-line-emphasis))
    (propertize "none" 'face 'mode-line-inactive)))

(defun ship-mate-dinghy--reset-header-line-format (&rest _args)
  "Set header line format.

If BUFFER is non-nil, reset in buffer."
  (when ship-mate-dinghy-mode
    (setq-local header-line-format
                (format "%s[%s]"
                        (propertize "env" 'face 'mode-line)
                        (ship-mate-dinghy--print-variables)))))

;;; -- Editing the environment

(defvar ship-mate-environment--buffer-name "*ship-mate-edit-env*"
  "The name of the buffer used for `ship-mate-edit-environment'.")
(defvar ship-mate-environment--target-buffer nil
  "The buffer `ship-mate-edit-environment' was called from.")

(defvar ship-mate-environment-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-c" #'ship-mate-environment-apply)
    (define-key map "\C-c\C-q" #'ship-mate-environment-clear)
    (define-key map "\C-c\C-k" #'ship-mate-environment-abort)
    map)
  "Map used in buffer created by `ship-mate-edit-environment'.")

(define-minor-mode ship-mate-environment-mode
  "Minor mode to edit the environment."
  :lighter " sme"
  (setq-local header-line-format
              (substitute-command-keys
               "\\<ship-mate-environment-mode-map>\
`\\[ship-mate-environment-apply]' applies and recompiles, \
`\\[ship-mate-environment-clear]' clears all env variables, \
`'\\[ship-mate-environment-abort]' reverts.")))

(defun ship-mate-environment--edit ()
  "Show env edit buffer."
  (with-current-buffer (current-buffer)
    (unless (derived-mode-p 'compilation-mode 'comint-mode)
      (user-error "Can only edit environments of compilation buffers")))

  (setq ship-mate-environment--target-buffer (current-buffer))

  (let* ((buffer (get-buffer-create ship-mate-environment--buffer-name))
         (env (buffer-local-value 'compilation-environment ship-mate-environment--target-buffer))
         (count (length env)))

    (with-current-buffer buffer
      (erase-buffer)

      (seq-map-indexed
       (lambda (it i)
         (insert it)
         (unless (eq i (1- count))
           (insert "\n")))
       env)

      (unless ship-mate-environment-mode
        (ship-mate-environment-mode))

      (pop-to-buffer buffer nil t))))

(defun ship-mate-environment--listify ()
  "Listify the current edit state."
  (let* ((buffer (get-buffer ship-mate-environment--buffer-name))
         (raw (with-current-buffer buffer
                (buffer-string))))

    (seq-filter (lambda (it) (not (string-empty-p it))) (string-split raw "\n"))))

(defun ship-mate-environment--validate ()
  "Validate the current edit state."
  (let ((new-state (ship-mate-environment--listify))
        (warnings nil))

    (unless (ship-mate-environment--valid-env-p new-state)
      (push "Invalid assignments" warnings))

    warnings))

(defun ship-mate-environment-apply ()
  "Apply the edited environment."
  (interactive)

  (when-let ((warnings (ship-mate-environment--validate)))
    (user-error (string-join warnings ", ")))

  (ship-mate-environment--set-environment (ship-mate-environment--listify))
  (ship-mate-environment--quit))

(defun ship-mate-environment--quit ()
  "Quit the editing."
  (quit-window t (get-buffer-window ship-mate-environment--buffer-name))

  (setq ship-mate-environment--target-buffer nil))

(defun ship-mate-environment-abort ()
  "Abort editing."
  (interactive)

  (ship-mate-environment--quit))

(defun ship-mate-environment-clear ()
  "Clear the environment."
  (interactive)

  (ship-mate-environment--set-environment nil)
  (ship-mate-environment--quit))

(defun ship-mate-environment--set-environment (env)
  "Set `compilation-environment' to ENV.

This is set in buffer `ship-mate-environment--buffer-name'."
  (with-current-buffer ship-mate-environment--target-buffer
    (setq-local compilation-environment env)
    (ship-mate-dinghy--reset-header-line-format)))

(defun ship-mate-environment--valid-env-p (value)
  "Check if VALUE is a valid environment."
  (and (listp value)
       (or (null value)
           (seq-every-p
            (lambda (it)
              (eq 2 (length (string-split it "="))))
            value))))

;;; -- Utility

(defun ship-mate--plist-keys (plist)
  "Get all keys from PLIST."
  (unless (plistp plist)
    (user-error "Not a plist"))

  (let ((elements plist)
        (keys nil))

    (while elements
      (push (car elements) keys)
      (setq elements (cddr elements)))

    (reverse keys)))

(defun ship-mate--local-value (symbol &optional project)
  "Get the project-local value of SYMBOL.

Optionally the PROJECT may be passed directly."
  (and-let* (((boundp symbol))
             (project (or project (project-current)))
             (root (project-root project)))

    (project--value-in-dir symbol root)))

(defun ship-mate--read-command (prompt)
  "Complete command using PROMPT."
  (completing-read prompt
                   (ship-mate--plist-keys ship-mate-commands)
                   nil
                   t))

(defun ship-mate--command-buffer-p (&optional buffer)
  "Check if BUFFER is a `ship-mate-command' buffer."
  (let ((buffer (or buffer (current-buffer))))

    (string-match-p "\\*ship-mate" (buffer-name buffer))))

;;; -- API

;;;###autoload
(defun ship-mate-with-bounded-compilation (fun &rest args)
  "Run FUN applying ARGS.

This makes sure that FUN is run while setting
`compilation-save-buffers-predicate' to check if a buffer is part
of a project's buffers."
  (if-let* ((project (project-current nil))
            (buffers (project-buffers project))
            (compilation-save-buffers-predicate (lambda () (memq (current-buffer) buffers))))
      (apply fun args)
    (apply fun args)))

;;;###autoload
(defun ship-mate-select-command (cmd)
  "Complete and run CMD."
  (interactive
   (list (ship-mate--read-command "Select command: ")))

  (ship-mate-command (intern cmd)))

;;;###autoload
(cl-defmacro ship-mate-create-command (name &key key default)
  "Create command NAME.

The command will be bound in `ship-mate-command-map' using its
initial unless KEY is provided. If DEFAULT is non-nil, set the
initial value using it. If COMINT is t, make sure the command is
run in `comint-mode' instead."
  (declare (indent defun))

  (let ((function-name (intern (format "ship-mate-%s" name)))
        (default-var (intern (format "ship-mate-%s-default-cmd" name)))
        (key (or key (substring (symbol-name name) 0 1))))

    `(progn
       (defvar-local ,default-var ,default ,(format "Default for `%s'." function-name))

       (defun ,function-name (&optional arg)
         ,(concat (capitalize (symbol-name name))
                  " the current project.\n\n"
                  "See `ship-mate-command' for behavior of ARG.")
         (interactive "P")

         (ship-mate-command ',name arg))

       (setq ship-mate-commands (plist-put
                                   ship-mate-commands
                                   ',name
                                   ,(make-hash-table :test 'equal)))

       (define-key ship-mate-command-map ,key ',function-name)
       (put ',default-var 'safe-local-variable #'ship-mate-command--valid-default-p))))

;;;###autoload
(defun ship-mate-refresh-history (cmd &optional clear)
  "Refresh history for CMD.

If optional CLEAR is t, clear the history instead of re-filling
it with the default value(s)."
  (interactive (list (ship-mate--read-command "Refresh command: ")
                     current-prefix-arg))

  (ship-mate-command--create-history (intern cmd) clear))

(defun ship-mate-hidden-recompile ()
  "Recompile and show the buffer after compilation finishes."
  (interactive)

  (message "Hidden recompile")

  (ship-mate-submarine--recompile))

;;;###autoload
(defun ship-mate-edit-environment ()
  "Edit the `compilation-environment'.

Needs to be called from within a `compilation-mode' buffer."
  (interactive)

  (ship-mate-environment--edit))

;;;###autoload
(define-minor-mode ship-mate-mode
  "Minor-mode for project-scoped compilation.

Enabling this mode will (1) advise `compilation-start' to update
project-local histories, (2) advise `recompile' to read this
scoped history as well as make all functions in
`ship-mate-compile-functions' bounded to the current
project (will only ask you save buffers in that project) and (3)
adds a hook `compilation-start-hook' to maybe enable
`ship-mate-dinghy-mode'."
  :lighter ship-mate-lighter
  :global t
  (if ship-mate-mode
      (ship-mate-mode--setup)
    (ship-mate-mode--teardown)))

(provide 'ship-mate)

;;; ship-mate.el ends here
