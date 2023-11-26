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
  "Project-scoped compilation ."
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

;;; -- Variables

(defvar ship-mate-commands nil
  "List of commands and their per-project histories.

Each command created by `ship-mate-create-command' will
`plist-put' a new entry mapping a project's root to a command
history. The general structure is ([COMMAND-SYMBOL] .
HASH-MAP<PROJECT-ROOT, HISTORY>).")

(defvar ship-mate-command-history nil)
(defvar ship-mate-command-map (make-sparse-keymap))

(defvar ship-mate-command--current-command nil)
(defvar ship-mate-command--last-category nil)

(defvar ship-mate-environment nil
  "The project environment.

The value of this variable will be bound to
`compilation-environment'.

Ideally you bind this in in your .dir-locals file.")
(put 'ship-mate-environment 'safe-local-variable #'ship-mate-environment--valid-env-p)

;;; -- Commands

(defun ship-mate-command--buffer-name-function (project)
  "Return a function to name the compilation buffer for PROJECT."
  (let* ((cmd (or ship-mate-command--current-command "compile"))
         (name (format "*ship-mate-%s-%s*" cmd project)))

    (lambda (_major-mode) name)))

(defun ship-mate-command (cmd &optional arg)
  "Run CMD for the current project.

Each command will be stored in a per-project history. If the
history is non-empty, the user will not be prompted unless called
with a prefix argument ARG.

The `compilation-environment' is set from the project's
`ship-mate-environment'.

If the prefix argument ARG is 0, `comint-mode' will be used
instead of `compile-mode'."
  (defvar project-vc-name)

  (let* ((project-vc-name nil)
         (current (project-current t))
         (root (project-root current))
         (name (project-name current))

         (compilation-environment (ship-mate--local-value 'ship-mate-environment))

         (table (plist-get ship-mate-commands cmd))

         (history (ship-mate-command--history cmd))
         (ship-mate-command-history (ring-elements history))

         (initial (unless (ring-empty-p history)
                    (ring-ref history 0)))
         (ship-mate-command--current-command (symbol-name cmd))

         (comint (zerop (prefix-numeric-value arg)))
         (prompt (format "%s project (%s)%s"
                         (capitalize ship-mate-command--current-command)
                         name
                         (if comint " interactively: " ": ")))
         (command (or (and (not arg) initial)
                      (read-shell-command prompt initial 'ship-mate-command-history)))

         (default-directory (project-root current))

         (lowercase (downcase name))
         (compilation-buffer-name-function (ship-mate-command--buffer-name-function lowercase)))

    (setq ship-mate-command--last-category cmd)

    (ring-remove+insert+extend history command)

    (puthash root history table)

    (compile command comint)))

(defun ship-mate-command--fuzzy-match-p (command history)
  "Check if COMMAND matches previous commands in HISTORY."
  (when-let* ((elements (ring-elements history))
              (min-count 1)
              (match (seq-find
                      (lambda (it)
                        (let* ((el-parts (string-split it " "))
                               (matches (seq-count
                                         (lambda (part)
                                           (string-match-p part command))
                                         el-parts)))
                          (> matches min-count)))
                      elements)))

    (ring-member history match)))

(defun ship-mate-command--update-history (command &rest _)
  "Update history using COMMAND.

If COMMAND matches other commands of the last command category,
add it to the history."
  (and-let* (ship-mate-command--last-category
             (history (ship-mate-command--history ship-mate-command--last-category))
             (index (ship-mate-command--fuzzy-match-p command history)))

    (ring-remove+insert+extend history command)))

(defun ship-mate-command--rehydrate (recompile &optional edit)
  "Call RECOMPILE after re-hydrating.

This will set `compile-history' when `compile-command' matches a
command in the history of the last category.

EDIT is passed as-is to RECOMPILE."
  (defvar compile-history)

  (if-let* ((command compile-command)
            (history (and ship-mate-command--last-category
                          (ship-mate-command--history ship-mate-command--last-category)))
            (matches (ship-mate-command--fuzzy-match-p command history)))

      (let ((compile-history (ring-elements history)))

        (funcall-interactively recompile edit))

    (funcall-interactively recompile edit)))

(defun ship-mate-command--history (cmd)
  "Access history for CMD.

If the history doesn't yet exist, create it using the provided
default.

The default can be a string or a list of strings. In the latter
case, they are inserted in reverse order so that the first item
is the default."
  (if-let* ((table (plist-get ship-mate-commands cmd))
            (project (project-current))
            (root (project-root project))
            (history (gethash root table)))

      history

    (let* ((var (intern (format "ship-mate-%s-default-cmd" cmd)))
           (default (project--value-in-dir var root))
           (new-history (make-ring ship-mate-command-history-size)))

      (cond
       ((listp default)
        (mapc (lambda (it) (ring-insert new-history it)) (reverse default)))
       ((stringp default)
        (ring-insert new-history default))
       (t nil))

      (puthash root new-history table)

      new-history)))

(defun ship-mate-command--valid-default-p (val)
  "Check if VAL is a valid project command default."
  (or (stringp val)
      (and (listp val)
           (cl-every #'stringp val))))

;;; -- Minor mode

(defun ship-mate-mode--setup ()
  "Setup `ship-mate-mode'."
  (advice-add 'compilation-start :after #'ship-mate-command--update-history)
  (advice-add 'recompile :around #'ship-mate-command--rehydrate)

  (dolist (fun (append ship-mate-compile-functions '(ship-mate-command)))
    (advice-add fun :around 'ship-mate-with-bounded-compilation)))

(defun ship-mate-mode--teardown ()
  "Tear down `ship-mate-mode'."
  (advice-remove 'compilation-start #'ship-mate-command--update-history)
  (advice-remove 'recompile #'ship-mate-command--rehydrate)

  (dolist (fun (append ship-mate-compile-functions '(ship-mate-command)))
    (advice-remove fun 'ship-mate-with-bounded-compilation)))

;;; -- Editing the environment

(defvar ship-mate-environment--buffer-name "*ship-mate-edit-env*")
(defvar ship-mate-environment--target-buffer nil)

(defvar ship-mate-environment-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-c" #'ship-mate-environment-apply)
    (define-key map "\C-c\C-q" #'ship-mate-environment-clear)
    (define-key map "\C-c\C-k" #'ship-mate-environment-abort)
    map))

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

    (string-split raw "\n")))

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

  (with-current-buffer ship-mate-environment--target-buffer
    (setq-local compilation-environment (ship-mate-environment--listify)))

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

  (with-current-buffer ship-mate-environment--target-buffer
    (setq-local compilation-environment nil))

  (ship-mate-environment--quit))

(defun ship-mate-environment--valid-env-p (value)
  "Check if VALUE is a valid environment."
  (and (listp value)
       (seq-every-p
        (lambda (it) (eq 2 (length (string-split it "="))))
        value)))

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

;;; -- API

;;;###autoload
(defun ship-mate-with-bounded-compilation (fun &rest args)
  "Run FUN applying ARGS.

Makes sure this is done with `compilation-save-buffers-predicate'
set to filter by project buffers."
  (if-let* ((project (project-current nil))
            (buffers (project-buffers project))
            (compilation-save-buffers-predicate (lambda () (memq (current-buffer) buffers))))
      (apply fun args)
    (apply fun args)))

;;;###autoload
(defun ship-mate-select-command (cmd)
  "Complete and run CMD."
  (interactive
   (list (completing-read "Select command: "
                          (ship-mate--plist-keys ship-mate-commands)
                          nil
                          t)))

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
(defun ship-mate-edit-environment ()
  "Edit the `compilation-environment'."
  (interactive)

  (ship-mate-environment--edit))

;;;###autoload
(define-minor-mode ship-mate-mode
  "Minor-mode for project-scoped compilation."
  :lighter ship-mate-lighter
  :global t
  (if ship-mate-mode
      (ship-mate-mode--setup)
    (ship-mate-mode--teardown)))

(provide 'ship-mate)

;;; ship-mate.el ends here
