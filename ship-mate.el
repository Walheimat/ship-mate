;;; ship-mate.el --- Consolidate projects and compilation -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/ship-mate
;; Version: 0.7.0
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
(require 'subr-x)
(require 'files-x)

(defgroup ship-mate nil
  "Project-scoped compilation."
  :group 'ship-mate)

;;;; Customization

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

(defcustom ship-mate-command-fuzzy-match-function #'ship-mate-command--fuzzy-match
  "Function to match a command against history entries.

The function will be called with two arguments (as well as one
optional argument): the command to match against and the history
of the last command category (as well as whether a full match
should count). On a match it should return a plist that includes
MATCH, COUNT and INDEX (of matched item)."
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

(defcustom ship-mate-edit-environment-prefix 5
  "Numeric prefix for editing environment before compilation."
  :group 'ship-mate
  :type 'integer)

(defcustom ship-mate-other-project-prefix 2
  "Numeric prefix for running the command in another project."
  :group 'ship-mate
  :type 'integer)

(defcustom ship-mate-log t
  "Whether `ship-mate' should log to a buffer."
  :group 'ship-mate
  :type 'boolean)

;;;; Variables

(defvar ship-mate-command-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "!") #'ship-mate-select-command)
    (define-key map (kbd "@") #'ship-mate-show-results)
    (define-key map (kbd "#") #'ship-mate-rerun-command)
    (define-key map (kbd ">") #'ship-mate-refresh-history)
    (define-key map (kbd "<") #'ship-mate-store-history-as-default)
    (define-key map (kbd "%") #'ship-mate-show-logs)

    map)
  "Command map for `ship-mate' commands.

Commands created by `ship-mate-create-command' are automatically
bound here using an uppercase letter.")

(defvar ship-mate-commands nil
  "List of commands and their per-project histories.

Each command created by `ship-mate-create-command' will
`plist-put' a new entry mapping a project's root to a command
history. The general structure is ([COMMAND-SYMBOL] .
HASH-MAP<PROJECT-ROOT, HISTORY>).")

(defvar ship-mate-environment nil
  "The project environment.

The value of this variable will be bound to
`compilation-environment' when a command is run (provided none
has been set yet). This is shared between all commands.

Ideally you set this in in your .dir-locals file to include
variables executions should have.")
(put 'ship-mate-environment 'safe-local-variable #'ship-mate-environment--valid-env-p)

(defvar ship-mate-multiple nil
  "Commands that may have multiple buffers.

See `ship-mate-create-command'.")

;;;; Internal variables

(defvar ship-mate--command-history nil
  "The history of the currently executed command.")

(defvar ship-mate--last-compilation-type nil
  "The type of the last compilation.

This is either nil, `ship-mate' or `other'.")

(defvar-local ship-mate--this-command nil
  "The `ship-mate' command for this buffer.

This is set by `ship-mate-command'.")

(defvar-local ship-mate--reserved nil
  "Marker set to indicate the compilation was started by `ship-mate'.")

(defvar ship-mate--current-command-name nil
  "The symbol name of the currently executed command.")

(defvar ship-mate--project-meta (make-hash-table :test 'equal)
  "Map storing meta data per project.")

(defvar ship-mate-command--fuzzy-match-ignore "^\\(\\|\s+\\|--\\|.\\)$"
  "Regular expression used by `ship-mate-command--fuzzy-match'.

Parts of a command matching this expression are ignored.")

(defvar ship-mate--complete-for-all nil
  "Whether buffer completion should include foreign buffers.")

(defvar-local ship-mate--hidden nil
  "Indicates whether a buffer is currently hidden.")

(defvar ship-mate-environment--regex "^\\([a-zA-Z_]\\{1,\\}[a-zA-Z0-9_]+\\)=\\(.+\\)?$"
  "Pattern matching an environment variable assignment.")

(defvar ship-mate-command--executor 'funcall)

(defvar ship-mate-log--buffer-name "*ship-mate-log*")

;;;;; Hooks

(defvar ship-mate-environment-set-hook nil
  "Function to call when the environment was set.")

;;;; Commands

(defun ship-mate-command (cmd &optional arg)
  "Run CMD for the current project.

Each command will be stored in a per-project history. If the
history is non-empty, the user will not be prompted unless called
with a prefix argument ARG.

The `compilation-environment' is set from the project's
`ship-mate-environment'.

Execution behavior varies by ARG. See the various
`ship-mate-*-prefix' variables. By default 0 runs command using
`comint', 2 runs the command in another project, 3 runs the
command hidden and 5 lets you edit the environment first."
  (let* ((project-vc-name nil)
         (current (ship-mate-command--current-project arg))
         (root (project-root current))
         (name (project-name current))
         (project-buffers (project-buffers current))
         (lowercase (downcase name))

         ;; History.
         (table (plist-get ship-mate-commands cmd))
         (history (ship-mate-command--history cmd))
         (ship-mate--command-history (ring-elements history))

         ;; Reading user input.
         (initial (unless (ring-empty-p history)
                    (ring-ref history 0)))
         (ship-mate--current-command-name (symbol-name cmd))
         (comint (zerop (prefix-numeric-value arg)))
         (prompt (format "%s project (%s)%s"
                         (capitalize ship-mate--current-command-name)
                         name
                         (if comint " interactively: " ": ")))
         (prompt-var (intern (format "ship-mate-%s-prompt" cmd)))
         (command (or (and (ship-mate-command--has-run-p cmd current)
                           (not arg)
                           (not (project--value-in-dir prompt-var root))
                           initial)
                      (read-shell-command prompt initial 'ship-mate--command-history)))
         (command (when (stringp command)
                    (string-trim command)))

         ;; Binding external variables.
         (default-directory (project-root current))
         (compilation-save-buffers-predicate (lambda () (memq (current-buffer) project-buffers)))
         (compilation-buffer-name-function (funcall ship-mate-command-buffer-name-function-generator lowercase))
         (compilation-process-setup-function #'ship-mate-command--reserve))

    (when command

      ;; Record this as the last command.
      (ship-mate-command--record-last-command cmd current)

      ;; Mark the command as run
      (ship-mate-command--mark-as-run cmd current)

      ;; Amend history (don't extend).
      (ship-mate-command--update-history cmd command t)
      (puthash root history table)

      ;; Compile and set command for buffer.
      (let ((buffer (ship-mate-command--compile cmd command comint arg)))

        (with-current-buffer buffer
          (setq ship-mate--this-command cmd))

        buffer))))

(defun ship-mate-command--reserve ()
  "Reserve the current buffer for `ship-mate'."
  (setq ship-mate--reserved t))

(defun ship-mate-command--current-project (&optional arg)
  "Get the current project.

If ARG is the default prefix argument, this prompts for the
project first."
  (or (and (eq ship-mate-other-project-prefix (prefix-numeric-value arg))
           (when-let ((dir (project-prompt-project-dir)))
             (project--find-in-directory dir)))
      (project-current t)))

(defun ship-mate-command--compile (cmd command &optional comint arg)
  "Compile COMMAND in the context of CMD.

This uses `comint-mode' if COMINT is t.

If optional ARG is 5, the user is prompted to edit the
environment first."
  (let* ((env (or compilation-environment (ship-mate-environment--current-environment cmd)))
         (exec (lambda () (compile command comint)))
         (edited (if (eq ship-mate-edit-environment-prefix (prefix-numeric-value arg))
                     (ship-mate-environment--edit-in-minibuffer env)
                   env)))

    (if (null compilation-environment)
        (let ((compilation-environment edited))

          (apply ship-mate-command--executor (list exec)))

      (when (not (equal compilation-environment edited))
        (setq-local compilation-environment edited))

      (apply ship-mate-command--executor (list exec)))))

(defun ship-mate-command--fuzzy-match (command history &optional allow-full)
  "Match COMMAND against commands in HISTORY.

If there is a match this returns a plist of match, match count
and the index of the matched item.

If ALLOW-FULL is t, also count full matches."
  (and-let* ((elements (ring-elements history))
             (first-segment (string-trim (nth 0 (string-split command " "))))
             (matcher (lambda (it idx)
                        (let* ((el-parts (string-split it " "))
                               (matches (seq-count
                                         (lambda (part)
                                           (and (not (string-match-p
                                                      ship-mate-command--fuzzy-match-ignore
                                                      part))
                                                (string-match-p part command)))
                                         el-parts)))

                          ;; Lacking congruence reduces count.
                          (when (not (string= first-segment (nth 0 el-parts)))
                            (setq matches (1- matches)))

                          (if (and (or allow-full
                                       (not (string= it command))))
                              (list :index idx :matches matches :value it)
                            (list :index idx :matches 0 :value it)))))
             (matches (seq-map-indexed matcher elements))
             (sorted (seq-sort
                      (lambda (a b) (> (plist-get a :matches)
                                  (plist-get b :matches)))
                      matches))
             (top-match (nth 0 sorted)))

    (list
     :match (plist-get top-match :value)
     :count (plist-get top-match :matches)
     :index (ring-member history (plist-get top-match :value)))))

(defun ship-mate-command--buffer-name-function (project)
  "Return a function to name the compilation buffer for PROJECT.

If the command allows for multiple buffers, this will create a new
buffer if the base buffer has a running process."
  (let* ((cmd (or ship-mate--current-command-name "compile"))
         (name (format "*ship-mate-%s-%s*" cmd project)))

    (if (member cmd ship-mate-multiple)
        (lambda (_major-mode)
          (or (and-let* ((base (get-buffer name))
                         (process (get-buffer-process base))
                         ((process-live-p process)))
                (generate-new-buffer-name name))
              name))
      (lambda (_major-mode) name))))

(defun ship-mate-command--capture-history (command &rest _)
  "Update history using COMMAND.

If there is a match between COMMAND and the history of the last
`ship-mate' command, the command is inserted into that history.

If the history is already full and the quality of the match high
enough, prompt to instead replace the matched recorded command."
  (and-let* ((last (ship-mate-command--last-command)))

    (ship-mate-command--update-history last command nil)))

(defun ship-mate-command--update-history (cmd command &optional always-insert)
  "Update history of CMD using COMMAND.

This uses the fuzzy matcher
`ship-mate-command-fuzzy-match-function' to establish how (or if
at all) COMMAND should be inserted. If ALWAYS-INSERT is t,
COMMAND will be inserted even if there is no match."
  (and-let* ((history (ship-mate-command--history cmd)))

    (let ((specs (funcall ship-mate-command-fuzzy-match-function command history)))

      (if (and specs
               (plistp specs)
               (>= (plist-get specs :count) 3)
               (= (ring-length history) ship-mate-command-history-size))
          (progn
            (ship-mate-log-debug
             "Replacing `%s' with `%s' in history of `%s'"
             (plist-get specs :match)
             command
             cmd)
            (ring-remove history (plist-get specs :index))
            (ring-insert history command))

        (when (or always-insert
                  (and specs
                       (plistp specs)
                       (>= (plist-get specs :count) 2)))
          (unless (ring-member history command)
            (ship-mate-log-debug "Inserting `%s' into history of `%s'" command cmd))

          (ring-remove+insert+extend history command))))))

(defun ship-mate-command--capture (recompile &optional edit)
  "Only call RECOMPILE conditionally.

If we're in a `ship-mate-command' buffer, call recompile after
setting the `compile-history'. Otherwise check if the current
`compile-command' matches the history of the previous command; if
that is the case just call `ship-mate-command' with that last
command again.

As a last resort call `recompile'.

EDIT is passed as-is to all invocations of RECOMPILE."
  (cond
   ((ship-mate--command-buffer-p)
    (let* ((cmd ship-mate--this-command)
           (history (ship-mate-command--history ship-mate--this-command))
           (compile-history (and history (ring-elements history)))
           (compilation-process-setup-function #'ship-mate-command--reserve))

      (with-current-buffer (funcall-interactively recompile edit)

        ;; Update history.
        (when-let ((command (car-safe compilation-arguments)))
          (ship-mate-command--update-history cmd command t))

        ;; Needs to be re-set explicitly.
        (setq ship-mate--this-command cmd)

        (current-buffer))))

   ;; Don't break other derived modes or non-projects.
   ((or (derived-mode-p 'compilation-mode)
        (not (project-current)))
    (funcall-interactively recompile edit))

   (t
    (if-let* ((command compile-command)
              (last (ship-mate-command--last-command))
              (history (and last
                            (ship-mate-command--history last)))
              (matches (funcall ship-mate-command-fuzzy-match-function command history t)))

        (ship-mate-command last edit)

      (funcall-interactively recompile edit)))))

(defun ship-mate-command--mark-as-run (cmd project)
  "Mark CMD as having been run in PROJECT."
  (if-let ((meta (gethash project ship-mate--project-meta)))

      (let ((ran (plist-get meta :has-run)))

        (unless (memq cmd ran)
          (push cmd ran))

        (plist-put meta :has-run ran))

    (puthash project (list :has-run (list cmd)) ship-mate--project-meta)))

(defun ship-mate-command--has-run-p (cmd project)
  "Check if CMD has been run before in PROJECT."
  (and-let* ((meta (gethash project ship-mate--project-meta))
             (ran (plist-get meta :has-run)))

    (memq cmd ran)))

(defun ship-mate-command--record-last-command (cmd project)
  "Record CMD as the latest command for PROJECT."
  (if-let ((meta (gethash project ship-mate--project-meta)))

      (plist-put meta :last cmd)

    (puthash project (list :last cmd) ship-mate--project-meta)))

(defun ship-mate-command--last-command ()
  "Get the last command for the current project."
  (when-let* ((project (project-current))
              (meta (gethash project ship-mate--project-meta))
              (cmd (plist-get meta :last)))

    cmd))

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

This will read the default history from its dir-local value of
`ship-mate-CMD-default-cmd' (a string or a list of strings)
unless EMPTY is t."
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

(defun ship-mate-command--store-history-as-default (cmd &optional edit-first)
  "Store history of CMD as default.

If optional EDIT-FIRST is t, the user is prompted to edit the
history first."
  (when-let* ((history (ship-mate-command--existing-history cmd))
              (elements (if edit-first
                            (ship-mate-history--edit-in-minibuffer history)
                          (ring-elements history)))
              (variable (intern (format "ship-mate-%s-default-cmd" cmd)))
              (conf (current-window-configuration)))

    (modify-dir-local-variable nil variable elements 'add-or-replace)
    (save-buffer)

    (set-window-configuration conf)))

(defun ship-mate-command--valid-default-p (val)
  "Check if VAL is a valid project command default."
  (or (stringp val)
      (and (listp val)
           (cl-every #'stringp val))))

(defun ship-mate-command--buffers (&optional arg)
  "Get all `ship-mate' buffers in the current project.

If ARG is non-nil, this returns all `ship-mate' buffers."
  (let ((buffers (if arg
                     (buffer-list)
                   (when-let ((project (project-current)))
                     (project-buffers project)))))

    (cl-loop for buffer in buffers
             if (ship-mate--command-buffer-p buffer)
             collect buffer)))

(defun ship-mate-command--key-for-command (command &optional desired-key)
  "Get a key that can be used to bind COMMAND.

If DESIRED-KEY is passed, prefer that if possible. This will
otherwise go through all eligible letters in COMMAND taking the
first that isn't already bound."
  (if (ship-mate-command--valid-key-p desired-key)

      desired-key

    (when-let ((letter (thread-last
                         ""
                         (string-split (symbol-name command))
                         (seq-filter (lambda (it) (not (string-empty-p it))))
                         (seq-drop-while (lambda (it) (not (ship-mate-command--valid-key-p it))))
                         (car-safe))))

      letter)))

(defun ship-mate-command--valid-key-p (key)
  "Check if KEY could be bound."
  (and key
       (key-valid-p key)
       (not (keymap-lookup ship-mate-command-map key))))

(defun ship-mate-command-next-buffer (&optional arg)
  "Get the next buffer.

ARG is passed to `ship-mate-command--buffers'."
  (interactive "P")

  (let* ((buffers (ship-mate-command--buffers arg))
         (length (length buffers))
         (pos (cl-position (current-buffer) buffers))
         (next (mod (1+ pos) length)))

    (when (<= length 1)
      (user-error "There is no next buffer"))

    (switch-to-buffer (nth next buffers))))

(defun ship-mate-command-prev-buffer (&optional arg)
  "Get the previous buffer.

ARG is passed to `ship-mate-command--buffers'."
  (interactive "P")

  (let* ((buffers (ship-mate-command--buffers arg))
         (pos (cl-position (current-buffer) buffers))
         (length (length buffers))
         (prev (mod (+ length (1- pos)) length)))

    (when (<= length 1)
      (user-error "There is no previous buffer"))

    (switch-to-buffer (nth prev buffers))))

;;;; Environment

(defun ship-mate-environment--edit-in-minibuffer (environment)
  "Edit ENVIRONMENT in the minibuffer and return the result."
  (let* ((env (read-string "Edit environment: " (string-join environment " ")))
         (recreated (string-split env " " t)))

    (unless (ship-mate-environment--valid-env-p recreated)
      (user-error "Invalid environment"))

    recreated))

(defun ship-mate-environment--valid-env-p (value)
  "Check if VALUE is a valid environment."
  (let ((matcher (apply-partially #'string-match-p ship-mate-environment--regex)))

    (and (listp value)
         (or (null value)
             ;; Only providing default
             (and (seq-every-p 'stringp value)
                  (seq-every-p matcher value))
             ;; Providing per-command
             (and-let* ((values (mapcar #'cdr-safe value))
                        (values (delq nil values)))

               (seq-every-p 'listp values)

               (seq-every-p
                (lambda (val)
                  (seq-every-p matcher val))
                values))))))

(defun ship-mate-environment--current-environment (cmd)
  "Get the last environment for CMD or default.

This is the currently set `compilation-environment' if it's
non-nil.

Otherwise the result is based on the local value of
`ship-mate-environment'. If it was defined per-command, the common
environment (denoted by nil) and the command's environment are merged.
If it's a plain list, it is used for all commands."
  (if-let* ((buffer-name (funcall compilation-buffer-name-function nil))
            (buffer (get-buffer buffer-name))
            (local (buffer-local-value 'compilation-environment buffer)))

      local

    (when-let ((env (ship-mate--local-value 'ship-mate-environment)))

      (if (and (alist-get nil env)
               (alist-get cmd env))

          (let ((keys)
                (combined (append (alist-get cmd env) (alist-get nil env)))
                (merged))

            (dolist (item combined)

              (let ((key (car-safe (string-split item "="))))

                (unless (member key keys)
                  (push item merged)
                  (push key keys))))

            (reverse merged))

        (or (alist-get cmd env)
            (alist-get nil env)
            env)))))

;;;; History

(defun ship-mate-history--edit-in-minibuffer (history)
  "Edit HISTORY in minibuffer."
  (let* ((elements (ring-elements history))
         (combined (read-string "Edit history: " (string-join elements ";")))
         (recreated (string-split combined ";" t)))

    recreated))

;;;; Logging

(defvar ship-mate-log-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "q" 'quit-window)

    map)
  "Map used in `ship-mate-log-mode'.")

(define-minor-mode ship-mate-log-mode
  "Minor mode for logs."
  :global nil
  (read-only-mode))

(defun ship-mate-log--get-buffer ()
  "Get the log buffer."
  (if-let* ((buf (get-buffer ship-mate-log--buffer-name)))
      buf
    (with-current-buffer (get-buffer-create ship-mate-log--buffer-name)

      (ship-mate-log-mode)

      (current-buffer))))

(defun ship-mate-log--log (level face fmt &rest args)
  "Log to the log buffer.

This will format FMT using ARGS indicating LEVEL. The message
will be propertized using FACE."
  (when ship-mate-log
    (with-current-buffer (ship-mate-log--get-buffer)
      (let ((inhibit-read-only t)
            (level (upcase (symbol-name level)))
            (message (apply 'format fmt args)))

        (goto-char (point-max))
        (insert (propertize (format "[%-5s]: %s\n" level message) 'face face))))))

(defun ship-mate-log-info (fmt &rest args)
  "Log message on info level.

See `ship-mate-log--log' for the meaning of FMT and ARGS."
  (apply 'ship-mate-log--log (append (list 'info 'default fmt) args)))

(defun ship-mate-log-debug (fmt &rest args)
  "Log message on debug level.

See `ship-mate-log--log' for the meaning of FMT and ARGS."
  (apply 'ship-mate-log--log (append (list 'debug 'shadow fmt) args)))

;;;; Utility

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

    (with-current-buffer buffer
      (and (string-match-p "\\*ship-mate" (buffer-name buffer))
           (or (not (null ship-mate--this-command))
               ship-mate--reserved)))))

(defun ship-mate--command-buffer-predicate (buffer)
  "Predicate to check if BUFFER is a `ship-mate' buffer.

If the current prefix argument is non-nil, buffers from other
projects are included."
  (let* ((buffer (ship-mate--safe-get-buffer buffer))
         (project (project-current))
         (project-buffers (and project (project-buffers project))))

    (and (ship-mate--command-buffer-p buffer)
         (or ship-mate--complete-for-all (memq buffer project-buffers)))))

(defun ship-mate--complete-buffer (prompt &optional predicate)
  "Complete a `ship-mate' buffer using PROMPT.

This will set the predicate to command buffers unless PREDICATE
is passed."
  (let ((rbts-completion-table (apply-partially
                                #'completion-table-with-predicate
                                #'internal-complete-buffer
                                #'always
                                nil))
        (ship-mate--complete-for-all current-prefix-arg)
        (predicate (or predicate #'ship-mate--command-buffer-predicate)))

    (unless (seq-find predicate (buffer-list))
      (user-error "No `ship-mate' command buffer satisfying predicate exists"))

    (minibuffer-with-setup-hook
        (lambda () (setq-local minibuffer-completion-table rbts-completion-table))

      (get-buffer (read-buffer prompt nil t predicate)))))

(defun ship-mate--buffer-visible-p (buffer)
  "Check if the BUFFER is visible."
  (let ((windows (window-list-1 nil nil t)))

    (seq-find (lambda (it) (eq buffer (window-buffer it))) windows)))

(defun ship-mate--safe-get-buffer (buffer)
  "Get BUFFER safely."
  (if (consp buffer)
      (cdr buffer)
    buffer))

(defun ship-mate--warn (message)
  "Warn about MESSAGE."
  (display-warning 'ship-mate message))

(defun ship-mate--ensure-in-project ()
  "Signal an error if not in a project."
  (unless (project-current)
    (user-error "Not in a project!")))

;;;; Global minor mode

(defun ship-mate-mode--setup ()
  "Setup `ship-mate-mode'."
  (advice-add 'compilation-start :after #'ship-mate-command--capture-history)
  (advice-add 'recompile :around #'ship-mate-command--capture)

  (dolist (fun ship-mate-compile-functions)
    (advice-add fun :around 'ship-mate-with-bounded-compilation)))

(defun ship-mate-mode--teardown ()
  "Tear down `ship-mate-mode'."
  (advice-remove 'compilation-start #'ship-mate-command--capture-history)
  (advice-remove 'recompile #'ship-mate-command--capture)

  (dolist (fun ship-mate-compile-functions)
    (advice-remove fun 'ship-mate-with-bounded-compilation)))

;;;; Lighter

(defvar ship-mate-mode-lighter '((:eval (ship-mate-mode-lighter--title)))
  "The lighter for `ship-mate-mode'.")
(put 'ship-mate-mode-lighter 'risky-local-variable t)

(defvar ship-mate-mode-lighter--map
  (let ((map (make-sparse-keymap)))

    (define-key map [mode-line mouse-3] 'ship-mate-mode-lighter--menu)
    (define-key map [mode-line mouse-1] 'ship-mate-show-hidden)

    map)
  "Map used in mode line construct.")

(defun ship-mate-mode-lighter--menu ()
  "Menu for mode-line lighter."
  (interactive)

  (let* ((map (make-sparse-keymap))
         (rename (lambda (sym)
                   (thread-first
                     sym
                     (symbol-name)
                     (string-split "-")
                     (seq-subseq 2)
                     (string-join " ")
                     (capitalize))))
         (bind (lambda (_event func)
                 (define-key-after map
                   (vector func)
                   (list 'menu-item (funcall rename func) func)))))

    (define-key-after map [--actions] (list 'menu-item "Ship Mate"))

    (map-keymap bind ship-mate-command-map)

    (condition-case nil
        (popup-menu map)
      (quit nil))))

(defun ship-mate-mode-lighter--title ()
  "The mode-line title."
  `(:propertize ship-mate-lighter
                mouse-face mode-line-highlight
                help-echo "Ship Mate\nmouse-3: Menu"
                local-map ,ship-mate-mode-lighter--map))

;;;; API

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
(defun ship-mate-select-command (cmd &optional arg)
  "Complete and run CMD.

ARG is passed to the underlying command."
  (interactive
   (list (ship-mate--read-command "Select command: ")
         current-prefix-arg))

  (ship-mate--ensure-in-project)

  (ship-mate-command (intern cmd) arg))

;;;###autoload
(defun ship-mate-rerun-command (cmd &optional arg)
  "Re-run previous CMD.

ARG is passed to the underlying command. If there is no previous
command, this runs `ship-mate-select-command'."
  (interactive (list (ship-mate-command--last-command) current-prefix-arg))

  (ship-mate--ensure-in-project)

  (if cmd
      (ship-mate-command cmd arg)
    (call-interactively 'ship-mate-select-command)))

;;;###autoload
(defun ship-mate-show-results (buffer)
  "Pop to results BUFFER."
  (interactive (list (ship-mate--complete-buffer "Show results for: ")))

  (pop-to-buffer buffer))

;;;###autoload
(cl-defmacro ship-mate-create-command (name &key key default prompt multiple)
  "Create command NAME.

The command will be bound in `ship-mate-command-map' using
`ship-mate-command--key-for-command', preferring non-nil KEY.

 If DEFAULT is non-nil, set the initial value using it. If COMINT
is t, make sure the command is run in `comint-mode' instead.

The value of PROMPT will be set to a variable that determines
whether calling the command should always prompt.

if MULTIPLE is t, there may be multiple compilation buffers for this
command."
  (declare (indent defun))

  (let* ((function-name (intern (format "ship-mate-%s" name)))
         (default-var (intern (format "ship-mate-%s-default-cmd" name)))
         (prompt-var (intern (format "ship-mate-%s-prompt" name)))
         (key (ship-mate-command--key-for-command name key)))

    `(progn
       ,@(delq
          nil
          `((defvar-local ,default-var ,default ,(format "Default for `%s'." function-name))
            (defvar-local ,prompt-var ,prompt ,(format "Whether `%s' should prompt." function-name))

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

            ,(when (and multiple (not (memq name ship-mate-multiple)))
               `(push ,(symbol-name name) ship-mate-multiple))

            ,(if key
                 `(define-key ship-mate-command-map ,key ',function-name)
               `(ship-mate--warn ,(format "Failed to find eligible key for `%s'" name)))

            (put ',default-var 'safe-local-variable #'ship-mate-command--valid-default-p))))))

;;;###autoload
(defun ship-mate-refresh-history (cmd &optional clear)
  "Refresh history for CMD.

If optional CLEAR is t, clear the history instead of re-filling
it with the default value(s)."
  (interactive (list (ship-mate--read-command "Refresh command: ")
                     current-prefix-arg))

  (ship-mate-command--create-history (intern cmd) clear))

;;;###autoload
(defun ship-mate-store-history-as-default (cmd &optional arg)
  "Store history for CMD.

See `ship-mate-command--store-history-as-default' for the meaning
of ARG."
  (interactive (list (intern (ship-mate--read-command "Store command history: "))
                     current-prefix-arg))

  (unless (ship-mate-command--store-history-as-default cmd arg)
    (user-error "Failed to store command history")))

;;;###autoload
(define-minor-mode ship-mate-mode
  "Minor-mode for project-scoped compilation.

Enabling this mode will (1) advise `compilation-start' to update
project-local histories and (2) advise `recompile' to read this
scoped history as well as make all functions in
`ship-mate-compile-functions' bounded to the current
project (will only ask you save buffers in that project)."
  :lighter ship-mate-mode-lighter
  :global t
  (if ship-mate-mode
      (ship-mate-mode--setup)
    (ship-mate-mode--teardown)))

;;;###autoload
(defun ship-mate-show-logs ()
  "Show the logs."
  (interactive)

  (let ((buffer (ship-mate-log--get-buffer)))

    (pop-to-buffer buffer)))

(provide 'ship-mate)

;;; ship-mate.el ends here
