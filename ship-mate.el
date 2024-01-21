;;; ship-mate.el --- Consolidate projects and compilation -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/ship-mate
;; Version: 0.3.4
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

The function will be called with two arguments: the command to
match against and the history of the last command category. On a
match it should return a plist that includes MATCH, COUNT and
INDEX (of matched item)."
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

(defcustom ship-mate-prompt-for-hidden-buffer 2
  "Whether user should be prompted when hidden buffer is done.

If this is an integer, prompt after so many seconds. If it is
nil, show immediately."
  :group 'ship-mate
  :type '(choice (integer :tag "Delay for prompt")
                 (const :tag "Show immediately" nil)))

(defcustom ship-mate-hidden-compilation-prefix 3
  "Numeric prefix for hidden compilation."
  :group 'ship-mate
  :type 'integer)

(defcustom ship-mate-edit-environment-prefix 5
  "Numeric prefix for editing environment before compilation."
  :group 'ship-mate
  :type 'integer)

;;;; Variables

(defvar ship-mate-command-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd ".") #'ship-mate-select-command)
    (define-key map (kbd ",") #'ship-mate-hidden-recompile)
    (define-key map (kbd "/") #'ship-mate-hide-visible)

    (define-key map (kbd "!") #'ship-mate-show-results)
    (define-key map (kbd "@") #'ship-mate-show-hidden)

    (define-key map (kbd "%") #'ship-mate-edit-history)
    (define-key map (kbd "^") #'ship-mate-refresh-history)
    (define-key map (kbd "&") #'ship-mate-edit-environment)

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

;;;; Internal variables

(defvar ship-mate--command-history nil
  "The history of the currently executed command.")

(defvar ship-mate--last-compilation-type nil
  "The type of the last compilation.

This is either nil, `ship-mate' or `other'.")

(defvar-local ship-mate--this-command nil
  "The `ship-mate' command for this buffer.

This is set by `ship-mate-command'.")

(defvar ship-mate--current-command-name nil
  "The symbol name of the currently executed command.")

(defvar ship-mate--last-command nil
  "The symbol of the last executed command.")

(defvar ship-mate-command--fuzzy-match-ignore "^\\(\\|\s+\\|--\\)$"
  "Regular expression used by `ship-mate-command--fuzzy-match'.

Parts of a command matching this expression are ignored.")

(defvar ship-mate--complete-for-all nil
  "Whether buffer completion should include foreign buffers.")

(defvar-local ship-mate--hidden nil
  "Indicates whether a buffer is currently hidden.")

;;;; Commands

(defun ship-mate-command (cmd &optional arg)
  "Run CMD for the current project.

Each command will be stored in a per-project history. If the
history is non-empty, the user will not be prompted unless called
with a prefix argument ARG.

The `compilation-environment' is set from the project's
`ship-mate-environment'.

If the prefix argument ARG is 0, `comint-mode' will be used
instead of `compile-mode'. If it is 5, the user is prompted to
edit the environment first."
  (let* ((project-vc-name nil)
         (current (project-current t))
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
         (command (or (and (not arg) initial)
                      (read-shell-command prompt initial 'ship-mate--command-history)))

         ;; Binding external variables.
         (default-directory (project-root current))
         (compilation-save-buffers-predicate (lambda () (memq (current-buffer) project-buffers)))
         (compilation-buffer-name-function (funcall ship-mate-command-buffer-name-function-generator lowercase)))

    ;; Record this as the last command.
    (setq ship-mate--last-command cmd)

    ;; Amend history (don't extend).
    (ring-remove+insert+extend history command)
    (puthash root history table)

    ;; Compile and set command for buffer.
    (let ((buffer (ship-mate-command--compile command comint arg)))

      (with-current-buffer buffer
        (setq ship-mate--this-command cmd))

      buffer)))

(defun ship-mate-command--compile (command &optional comint arg)
  "Compile COMMAND.

This uses `comint-mode' if COMINT is t.

If optional ARG is 5, the user is prompted to edit the
environment first."
  (let* ((env (or compilation-environment (ship-mate-environment--current-environment)))
         (exec (lambda () (compile command comint)))
         (sub (eq ship-mate-hidden-compilation-prefix (prefix-numeric-value arg)))
         (edited (if (eq ship-mate-edit-environment-prefix (prefix-numeric-value arg))
                     (ship-mate-environment--edit-in-minibuffer env)
                   env)))

    (if (null compilation-environment)
        (let ((compilation-environment edited))

          (if sub
              (ship-mate-submarine--run exec)
            (funcall exec)))

      (when (not (equal compilation-environment edited))
        (setq-local compilation-environment edited))

      (if sub
          (ship-mate-submarine--run exec)
        (funcall exec)))))

(defun ship-mate-command--fuzzy-match (command history)
  "Match COMMAND against commands in HISTORY.

If there is a match this returns a plist of match, match count
and the index of the matched item."
  (and-let* ((elements (ring-elements history))
             (min-count 1)
             (top-matches 0)
             (matcher (lambda (it)
                        (let* ((el-parts (string-split it " "))
                               (matches (seq-count
                                         (lambda (part)
                                           (and (not (string-match-p
                                                      ship-mate-command--fuzzy-match-ignore
                                                      part))
                                                (string-match-p part command)))
                                         el-parts)))

                          (when (> matches min-count)
                            (setq top-matches matches)))))
             (match (seq-find matcher elements)))

    (list
     :match match
     :count top-matches
     :index (ring-member history match))))

(defun ship-mate-command--buffer-name-function (project)
  "Return a function to name the compilation buffer for PROJECT."
  (let* ((cmd (or ship-mate--current-command-name "compile"))
         (name (format "*ship-mate-%s-%s*" cmd project)))

    (lambda (_major-mode) name)))

(defun ship-mate-command--update-history (command &rest _)
  "Update history using COMMAND.

If there is a match between COMMAND and the history of the last
`ship-mate' command, the command is inserted into that history.

If the history is already full and the quality of the match high
enough, replace the matched recorded command instead of
inserting."
  (and-let* (ship-mate--last-command
             (history (ship-mate-command--history ship-mate--last-command))

             (replace-count 2)
             (specs (funcall ship-mate-command-fuzzy-match-function command history)))

    (if (and (plistp specs)
             (> (plist-get specs :count) replace-count)
             (= (ring-size history) ship-mate-command-history-size))
        (progn
          (ring-remove history (plist-get specs :index))
          (ring-insert history command))

      (ring-remove+insert+extend history command))))

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
           (compile-history (and history (ring-elements history))))

      (with-current-buffer (funcall-interactively recompile edit)
        (setq ship-mate--this-command cmd))))

   ;; Don't break other derived modes.
   ((derived-mode-p 'compilation-mode)
    (funcall-interactively recompile edit))

   (t
    (if-let* ((command compile-command)
              (history (and ship-mate--last-command
                            (ship-mate-command--history ship-mate--last-command)))
              (matches (funcall ship-mate-command-fuzzy-match-function command history)))

        (ship-mate-command ship-mate--last-command edit)

      (funcall-interactively recompile edit)))))

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

(defun ship-mate-command-next-buffer ()
  "Get the next buffer."
  (interactive)

  (let* ((buffers (ship-mate-command--buffers))
         (length (length buffers))
         (pos (cl-position (current-buffer) buffers))
         (next (mod (1+ pos) length)))

    (when (<= length 1)
      (user-error "There is no next buffer"))

    (switch-to-buffer (nth next buffers))))

(defun ship-mate-command-prev-buffer ()
  "Get the previous buffer."
  (interactive)

  (let* ((buffers (ship-mate-command--buffers))
         (pos (cl-position (current-buffer) buffers))
         (length (length buffers))
         (prev (mod (+ length (1- pos)) length)))

    (when (<= length 1)
      (user-error "There is no previous buffer"))

    (switch-to-buffer (nth prev buffers))))

;;;; Submarine

(defvar ship-mate-submarine--timer nil)
(defvar ship-mate-submarine--processes nil)

(defun ship-mate-submarine--recompile ()
  "Recompile without a window."
  (unless (eq 'ship-mate ship-mate--last-compilation-type)
    (user-error (if (eq 'other ship-mate--last-compilation-type)
                    "Previous compilation wasn't a `ship-mate' compilation"
                  "No previous compilation")))

  (let ((ship-mate--current-command-name (symbol-name ship-mate--last-command)))

    (ship-mate-submarine--run 'recompile)))

(defun ship-mate-submarine--in-progress ()
  "Check if a hidden compilation is in progress."
  (not (seq-empty-p ship-mate-submarine--processes)))

(defun ship-mate-submarine--run (exec)
  "Run EXEC in the background."
  (ship-mate-submarine--ensure-no-hidden-buffers)

  (message "Running `%s' command in the background" ship-mate--current-command-name)

  (let* ((display-buffer-alist '(("\\*ship-mate" (display-buffer-no-window))))
         (buffer (funcall exec)))

    (with-current-buffer buffer
      (setq ship-mate--hidden t))

    buffer))

(defun ship-mate-submarine--hide ()
  "Hide current compilation."
  (let* ((buffer (current-buffer))
         (window (get-buffer-window buffer))
         (process (ship-mate-submarine--get-process buffer)))

    (unless process
      (user-error "Can't hide `%s' as it has no process (anymore)" (current-buffer)))

    (message "Continuing `%s' command in the background" ship-mate--this-command)

    (with-current-buffer buffer
      (setq ship-mate--hidden t))

    (quit-window nil window)))

(defun ship-mate-submarine--get-process (buffer)
  "Get process for `ship-mate' BUFFER."
  (and buffer
       (ship-mate--command-buffer-p buffer)
       (get-buffer-process buffer)))

(defun ship-mate-submarine--ensure-no-hidden-buffers ()
  "Verify no hidden `ship-mate' buffer is visible.

If there are any, close them."
  (when-let ((windows (seq-filter
                       (lambda (it) (buffer-local-value 'ship-mate--hidden (window-buffer it)))
                       (window-list-1 nil nil t))))

    (dolist (win windows)
      (quit-window nil win))))

(defun ship-mate-submarine--check ()
  "Check on stopped processes.

This will surface buffers of hidden processes. Others are just
cleared."
  (ship-mate-submarine--clear-timer)

  (dolist (process ship-mate-submarine--processes)

    (let ((buffer (process-buffer process))
          (status (process-exit-status process)))

      (unless (process-live-p process)
        (if (ship-mate-submarine--hidden-buffer-p buffer)

            (progn (ship-mate-submarine--clear-process process)
                   (if ship-mate-prompt-for-hidden-buffer
                       (run-with-idle-timer ship-mate-prompt-for-hidden-buffer
                                            nil
                                            #'ship-mate-submarine--delayed-prompt
                                            (current-time)
                                            status
                                            process)
                     (ship-mate-submarine--surface process)))

          (ship-mate-submarine--clear-process process))))))

(defun ship-mate-submarine--hidden-buffer-p (buffer)
  "Check if BUFFER is a hidden buffer."
  (buffer-local-value 'ship-mate--hidden buffer))

(defun ship-mate-submarine--delayed-prompt (time status process)
  "Show a prompt to surface PROCESS.

TIME is the time the process finished, STATUS its status."
  (when-let* ((since (time-since time))
              (verb (if (eq 0 status) "finished successfully" (format "failed (exit status %d)" status)))
              (show (yes-or-no-p (format "Hidden compilation %s %.1fs ago. Show buffer?" verb (time-to-seconds since)))))

    (ship-mate-submarine--surface process)))

(defun ship-mate-submarine--watch-process (process)
  "Save PROCESS and set timer to check on it."
  (and-let* ((buffer (process-buffer process))
             ((ship-mate--command-buffer-p buffer)))

    (unless (memq process ship-mate-submarine--processes)
      (push process ship-mate-submarine--processes))

    (unless ship-mate-submarine--timer
      (setq ship-mate-submarine--timer (run-with-timer 0 0.1 #'ship-mate-submarine--check)))))

(defun ship-mate-submarine--clear-process (process)
  "Clear PROCESS.

If this was the final process, stops the timer.."
  (when (and process (memq process ship-mate-submarine--processes))
    (setq ship-mate-submarine--processes (delete process ship-mate-submarine--processes)))

  (ship-mate-submarine--clear-timer))

(defun ship-mate-submarine--clear-timer ()
  "Clear the timer if there are no more processes."
  (when ship-mate-submarine--timer
    (unless (ship-mate-submarine--in-progress)
      (cancel-timer ship-mate-submarine--timer)
      (setq ship-mate-submarine--timer nil))))

(defun ship-mate-submarine--surface (process)
  "Surface hidden compilation PROCESS.

If it is already shown, just clear timer and buffer."
  (unless process
    (user-error "Surfacing needs a process"))

  (let ((buffer (process-buffer process)))

    (ship-mate-submarine--clear-process process)

    (with-current-buffer buffer
      (setq ship-mate--hidden nil))

    (unless (ship-mate--buffer-visible-p buffer)
      (pop-to-buffer buffer))))

;;;; Dinghy mode

(defvar ship-mate-dinghy-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "C-c .") #'ship-mate-edit-environment)
    (define-key map (kbd "C-c ,") #'ship-mate-edit-history)
    (define-key map (kbd "C-c [") #'ship-mate-command-next-buffer)
    (define-key map (kbd "C-c ]") #'ship-mate-command-prev-buffer)
    (define-key map (kbd "C-c /") #'ship-mate-hide)

    map)
  "Map used in buffers that enable `ship-mate-dinghy-mode'.")

(define-minor-mode ship-mate-dinghy-mode
  "Minor mode to provide contextual information and bindings.

\\{ship-mate-dinghy-mode-map}"
  :lighter " smd"

  (unless (derived-mode-p 'compilation-mode)
    (user-error "`ship-mate-dinghy-mode' can only be enabled in compilation buffers"))

  (ship-mate-dinghy--reset-header-line-format))

(defvar-local ship-mate--command nil)

(defun ship-mate-dinghy--maybe-enable (&optional process)
  "Enable `ship-mate-dinghy-mode' if not disabled.

If PROCESS is passed, set the name."
  (if (and ship-mate-dinghy-enable
           (ship-mate--command-buffer-p))
      (progn
        (when process
          (setq ship-mate--command (process-command process)))

        (setq ship-mate--last-compilation-type 'ship-mate)

        (ship-mate-dinghy-mode))

    (setq ship-mate--last-compilation-type 'other)))

(defun ship-mate-dinghy--print-command ()
  "Print the command.

The printed command is just th"
  (if (and ship-mate--command (listp ship-mate--command))

      (let* ((rest (seq-drop-while (lambda (it) (string-match-p "^\\(\/\\|-\\)" it)) ship-mate--command))
             (full-command (string-join ship-mate--command " "))
             (likely-command (or (car-safe rest) full-command))

             (max-len 20)
             (likely-command (if (> (length likely-command) max-len)
                                 (concat (substring likely-command 0 max-len)
                                         "…")
                               likely-command)))

        (propertize likely-command
                    'face 'mode-line-emphasis
                    'help-echo full-command))

    (propertize "?" 'face 'mode-line-inactive)))

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

Prints the command of the process and environment variables."
  (when ship-mate-dinghy-mode
    (setq-local header-line-format
                ;; Command.
                (concat
                 (format "%s[%s]"
                         (propertize "cmd" 'face 'mode-line)
                         (ship-mate-dinghy--print-command))
                 " "
                 ;; Environment
                 (format "%s[%s]"
                         (propertize "env" 'face 'mode-line)
                         (ship-mate-dinghy--print-variables))))))

;;;; Editing

(defun ship-mate-edit--in-buffer (buffer-name elements mode)
  "Edit ELEMENTS in buffer BUFFER-NAME.

Sets MODE unless already set."
  (let* ((buffer (get-buffer-create buffer-name))
         (length (length elements)))

    (with-current-buffer buffer
      (erase-buffer)

      (seq-map-indexed
       (lambda (it i)
         (insert it)
         (unless (eq i (1- length))
           (insert "\n")))
       elements)

      (set-buffer-modified-p nil)

      (unless (buffer-local-value mode (current-buffer))
        (funcall mode))

      (pop-to-buffer buffer nil t))))

;;;; Editing the environmanet

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
  "Minor mode to edit the environment.

\\{ship-mate-environment-mode-map}"
  :lighter " sme"
  (setq-local header-line-format
              (substitute-command-keys
               "\\<ship-mate-environment-mode-map>\
`\\[ship-mate-environment-apply]' applies and recompiles, \
`\\[ship-mate-environment-clear]' clears all env variables, \
`\\[ship-mate-environment-abort]' reverts.")))

(defun ship-mate-environment--current-environment ()
  "Get the last environment for CMD or default."
  (if-let* ((buffer-name (funcall compilation-buffer-name-function nil))
            (buffer (get-buffer buffer-name)))

      (buffer-local-value 'compilation-environment buffer)

    (ship-mate--local-value 'ship-mate-environment)))

(defun ship-mate-environment--edit ()
  "Edit environment in the current buffer."
  (unless (ship-mate--command-buffer-p (current-buffer))
    (user-error "Can only edit `ship-mate' command buffers"))

  (setq ship-mate-environment--target-buffer (current-buffer))

  (ship-mate-environment--edit-in-buffer (buffer-local-value 'compilation-environment ship-mate-environment--target-buffer)))

(defun ship-mate-environment--edit-in-buffer (environment)
  "Create the buffer for editing the ENVIRONMENT."
  (ship-mate-edit--in-buffer ship-mate-environment--buffer-name
                             environment
                             'ship-mate-environment-mode))

(defun ship-mate-environment--edit-in-minibuffer (environment)
  "Edit ENVIRONMENT in the minibuffer and return the result."
  (let* ((env (read-string "Edit environment: " (string-join environment " ")))
         (recreated (string-split env " " t)))

    (unless (ship-mate-environment--valid-env-p recreated)
      (user-error "Invalid environment"))

    recreated))

(defun ship-mate-environment--validate ()
  "Validate the current edit state."
  (let ((new-state (ship-mate-environment--listify))
        (warnings nil))

    (unless (ship-mate-environment--valid-env-p new-state)
      (push "Invalid assignments" warnings))

    warnings))

(defun ship-mate-environment--listify ()
  "Listify the environment buffer."
  (ship-mate--listify-buffer (get-buffer ship-mate-environment--buffer-name)))

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

;;;; Editing history

(defvar ship-mate-history--buffer-name "*ship-mate-edit-history*"
  "The name of the buffer used for `ship-mate-edit-history'.")

(defvar ship-mate-history--command nil
  "The symbol of the command currently edited.")

(defvar ship-mate-history-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-c" #'ship-mate-history-apply)
    (define-key map "\C-c\C-q" #'ship-mate-history-clear)
    (define-key map "\C-c\C-k" #'ship-mate-history-abort)
    map)
  "Map used in buffer created by `ship-mate-edit-history'.")

(define-minor-mode ship-mate-history-mode
  "Minor mode to edit the history.

\\{ship-mate-history-mode-map}"
  :lighter " smh"
  (setq-local header-line-format
              (substitute-command-keys
               "\\<ship-mate-history-mode-map>\
`\\[ship-mate-history-apply]' applies, \
`\\[ship-mate-history-clear]' clears the history, \
`\\[ship-mate-history-abort]' reverts.")))

(defun ship-mate-history--edit ()
  "Edit the history of the current buffer."
  (unless (ship-mate--command-buffer-p (current-buffer))
    (user-error "Can only edit `ship-mate' command buffer"))

  (setq ship-mate-history--command (buffer-local-value 'ship-mate--this-command (current-buffer)))

  (ship-mate-history--edit-in-buffer))

(defun ship-mate-history--edit-in-buffer ()
  "Edit the history in a buffer."
  (let ((history (ship-mate-command--history ship-mate-history--command)))

    (ship-mate-edit--in-buffer ship-mate-history--buffer-name
                               (ring-elements history)
                               'ship-mate-history-mode)))

(defun ship-mate-history-apply ()
  "Apply the edited history."
  (interactive)

  (ship-mate-history--set-history (ship-mate-history--listify))
  (ship-mate-history--quit))

(defun ship-mate-history-clear ()
  "Clear the history."
  (interactive)

  (ship-mate-history--set-history nil)
  (ship-mate-history--quit))

(defun ship-mate-history-abort ()
  "Abort editing history."
  (interactive)

  (ship-mate-history--quit))

(defun ship-mate-history--set-history (new-elements)
  "Set the edited history to include NEW-ELEMENTS."
  (let ((history (ship-mate-command--history ship-mate-history--command)))

    (ring-resize history 0)
    (ring-resize history ship-mate-command-history-size)

    (dolist (it new-elements)
      (ring-insert history it))))

(defun ship-mate-history--quit ()
  "Quit the history editing buffer."
  (quit-window t (get-buffer-window ship-mate-history--buffer-name))

  (setq ship-mate-history--command nil))

(defun ship-mate-history--listify ()
  "Listify history buffer."
  (reverse (ship-mate--listify-buffer (get-buffer ship-mate-history--buffer-name))))

;;;; Utility

(defun ship-mate--listify-buffer (buffer)
  "Listify the content of BUFFER."
  (let* ((raw (with-current-buffer buffer
                (buffer-string))))

    (seq-filter (lambda (it) (not (string-empty-p it))) (string-split raw "\n"))))

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

;;;; Global minor mode

(defun ship-mate-mode--setup ()
  "Setup `ship-mate-mode'."
  (advice-add 'compilation-start :after #'ship-mate-command--update-history)
  (advice-add 'recompile :around #'ship-mate-command--capture)

  (add-hook 'compilation-start-hook 'ship-mate-dinghy--maybe-enable)
  (add-hook 'compilation-start-hook 'ship-mate-submarine--watch-process)

  (dolist (fun ship-mate-compile-functions)
    (advice-add fun :around 'ship-mate-with-bounded-compilation)))

(defun ship-mate-mode--teardown ()
  "Tear down `ship-mate-mode'."
  (advice-remove 'compilation-start #'ship-mate-command--update-history)
  (advice-remove 'recompile #'ship-mate-command--capture)

  (remove-hook 'compilation-start-hook 'ship-mate-dinghy--maybe-enable)
  (remove-hook 'compilation-start-hook 'ship-mate-submarine--watch-process)

  (dolist (fun ship-mate-compile-functions)
    (advice-remove fun 'ship-mate-with-bounded-compilation)))

;;;; Lighter

(defvar ship-mate-mode-lighter '((:eval (ship-mate-mode-lighter--title))
                                 (:eval (ship-mate-mode-lighter--hidden)))
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
         (rename (lambda (sym) (substring (symbol-name sym)
                                          (1+ (length "ship-mate")))))
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

(defun ship-mate-mode-lighter--hidden ()
  "Indicates a running hidden recompile."
  (when (ship-mate-submarine--in-progress)
    '(:propertize "!" face mode-line-emphasis)))

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

  (ship-mate-command (intern cmd) arg))

;;;###autoload
(defun ship-mate-show-results (buffer)
  "Pop to results BUFFER."
  (interactive (list (ship-mate--complete-buffer "Show results for: ")))

  (pop-to-buffer buffer))

;;;###autoload
(cl-defmacro ship-mate-create-command (name &key key default)
  "Create command NAME.

The command will be bound in `ship-mate-command-map' using
`ship-mate-command--key-for-command', preferring non-nil KEY.

 If DEFAULT is non-nil, set the initial value using it. If COMINT
is t, make sure the command is run in `comint-mode' instead."
  (declare (indent defun))

  (let* ((function-name (intern (format "ship-mate-%s" name)))
         (default-var (intern (format "ship-mate-%s-default-cmd" name)))
         (key (ship-mate-command--key-for-command name key)))

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

       ,(if key
            `(define-key ship-mate-command-map ,key ',function-name)
          `(ship-mate--warn ,(format "Failed to find eligible key for `%s'" name)))

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

  (ship-mate-submarine--recompile))

(defun ship-mate-show-hidden (process)
  "Show a hidden compilation PROCESS."
  (interactive (list (if (eq 1 (length ship-mate-submarine--processes))
                         (nth 0 ship-mate-submarine--processes)

                       (let ((buffers (mapcar (lambda (it) (process-buffer it)) ship-mate-submarine--processes)))

                         (get-buffer-process (ship-mate--complete-buffer
                                              "Show hidden buffer: "
                                              (lambda (it) (memq (ship-mate--safe-get-buffer it) buffers))))))))

  (ship-mate-submarine--surface process))

;;;###autoload
(defun ship-mate-edit-environment (buffer)
  "Edit the `compilation-environment' for BUFFER.

If BUFFER isn't a compilation buffer, this prompts to select one."
  (interactive
   (list (if (ship-mate--command-buffer-p)
             (current-buffer)
           (ship-mate--complete-buffer "Edit environment for buffer: "))))

  (with-current-buffer buffer
    (ship-mate-environment--edit)))

;;;###autoload
(defun ship-mate-edit-history (buffer)
  "Edit the history for BUFFER.

If BUFFER isn't a compilation buffer, this prompts to select one."
  (interactive
   (list (if (ship-mate--command-buffer-p)
             (current-buffer)
           (ship-mate--complete-buffer "Edit history for buffer: "))))

  (with-current-buffer buffer
    (ship-mate-history--edit)))

;;;###autoload
(defun ship-mate-hide ()
  "Hide the current compilation."
  (interactive)

  (ship-mate-submarine--hide))

(defun ship-mate-hide-visible ()
  "Hide a visible `ship-mate' buffer."
  (interactive)

  (when-let ((visible (seq-find
                       #'ship-mate--buffer-visible-p
                       (ship-mate-command--buffers))))

    (with-current-buffer visible
      (ship-mate-submarine--hide))))

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
  :lighter ship-mate-mode-lighter
  :global t
  (if ship-mate-mode
      (ship-mate-mode--setup)
    (ship-mate-mode--teardown)))

(provide 'ship-mate)

;;; ship-mate.el ends here
