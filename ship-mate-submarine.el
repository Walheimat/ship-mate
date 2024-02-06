;;; ship-mate-submarine.el --- Submerge running compilations -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/ship-mate
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

;;; Commentary:

;; Allows submerging running compilations. Submerged compilations will
;; resurface after completion if a prompt is accepted. They can also
;; be resurfaced manually.

;;; Code:

(require 'ship-mate)
(require 'ship-mate-dinghy)

;;;; Customization

(defcustom ship-mate-submarine-prompt-for-hidden-buffer 2
  "Whether user should be prompted when hidden buffer is done.

If this is an integer, prompt after so many seconds. If it is
nil, show immediately."
  :group 'ship-mate
  :type '(choice (integer :tag "Delay for prompt")
                 (const :tag "Show immediately" nil)))

(defcustom ship-mate-submarine-hidden-compilation-prefix 3
  "Numeric prefix for hidden compilation."
  :group 'ship-mate
  :type 'integer)

;;;; Variables

(defvar ship-mate-submarine--timer nil)
(defvar ship-mate-submarine--processes nil)

;;;; Functionality

(defun ship-mate-submarine--maybe-run (fun &rest args)
  "Maybe hijack FUN.

See `ship-mate-command--compile' for the meaning of ARGS.

If the prefix argument is 3, use `ship-mate-submarine--run' as
the executor of `ship-mate-command--compile'."
  (let* ((prefix-arg (nth 2 args))
         (sub (eq
               ship-mate-submarine-hidden-compilation-prefix
               (prefix-numeric-value prefix-arg)))
         (ship-mate-command--executor (if sub
                                          'ship-mate-submarine--run
                                        ship-mate-command--executor)))

    (apply fun args)))

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
         (window (get-buffer-window buffer t))
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
                   (if ship-mate-submarine-prompt-for-hidden-buffer
                       (run-with-idle-timer ship-mate-submarine-prompt-for-hidden-buffer
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

(defun ship-mate-submarine-lighter ()
  "Indicates a running hidden recompile."
  (when (ship-mate-submarine--in-progress)
    '(:propertize "!" face mode-line-emphasis)))

(defvar ship-mate-submarine--lighter '(:eval (ship-mate-submarine-lighter)))
(put 'ship-mate-submarine--lighter 'risky-local-variable t)

(defun ship-mate-submarine-mode--setup ()
  "Set up `ship-mate-submarine-mode'."
  (setf ship-mate-mode-lighter (cons ship-mate-mode-lighter (list ship-mate-submarine--lighter)))

  (add-hook 'compilation-start-hook 'ship-mate-submarine--watch-process)

  (advice-add
   'ship-mate-command--compile :around
   'ship-mate-submarine--maybe-run)

  (let ((map ship-mate-command-map))

    (define-key map (kbd "/") #'ship-mate-submarine-hidden-recompile)
    (define-key map (kbd "?") #'ship-mate-submarine-hide-visible)
    (define-key map (kbd "#") #'ship-mate-submarine-show-hidden))

  (let ((map ship-mate-dinghy-mode-map))

    (define-key map (kbd "C-c /") #'ship-mate-submarine-hide)))

(defun ship-mate-submarine-mode--teardown ()
  "Tear down the alterations done by setup."
  (setf ship-mate-mode-lighter (delq ship-mate-submarine--lighter ship-mate-mode-lighter))

  (remove-hook 'compilation-start-hook 'ship-mate-submarine--watch-process)

  (advice-remove 'ship-mate-command--compile 'ship-mate-submarine--maybe-run)

  (let ((map ship-mate-command-map))

    (define-key map (kbd "/") nil)
    (define-key map (kbd "?") nil)
    (define-key map (kbd "#") nil))

  (let ((map ship-mate-dinghy-mode-map))

    (define-key map (kbd "C-c /") nil)))

;;; API

;;;###autoload
(defun ship-mate-submarine-hidden-recompile (buffer)
  "Recompile BUFFER without raising it."
  (interactive (list (ship-mate--complete-buffer "Recompile: ")))

  (setq ship-mate--current-command-name (buffer-local-value 'ship-mate--this-command buffer))

  (with-current-buffer buffer
    (ship-mate-submarine--run 'recompile)))

;;;###autoload
(defun ship-mate-submarine-show-hidden (process)
  "Show a hidden compilation PROCESS."
  (interactive (list (if (eq 1 (length ship-mate-submarine--processes))
                         (nth 0 ship-mate-submarine--processes)

                       (let ((buffers (mapcar (lambda (it) (process-buffer it)) ship-mate-submarine--processes)))

                         (get-buffer-process (ship-mate--complete-buffer
                                              "Show hidden buffer: "
                                              (lambda (it) (memq (ship-mate--safe-get-buffer it) buffers))))))))

  (ship-mate-submarine--surface process))

;;;###autoload
(defun ship-mate-submarine-hide ()
  "Hide the current compilation."
  (interactive)

  (ship-mate-submarine--hide))

;;;###autoload
(defun ship-mate-submarine-hide-visible ()
  "Hide a visible `ship-mate' buffer."
  (interactive)

  (when-let ((visible (seq-find
                       #'ship-mate--buffer-visible-p
                       (ship-mate-command--buffers))))

    (with-current-buffer visible
      (ship-mate-submarine--hide))))

;;;###autoload
(define-minor-mode ship-mate-submarine-mode
  "Allow submerging when running `ship-mate-command'.

This mode also amends the lighter to indicate a running
compilation (hidden or not) and sets up bindigns for
`ship-mate-command-map' and `ship-mate-dinghy-mode-map'."
  :global t
  :group 'ship-mate
  (if ship-mate-submarine-mode
      (ship-mate-submarine-mode--setup)
    (ship-mate-submarine-mode--teardown)))

(provide 'ship-mate-submarine)

;;; ship-mate-submarine.el ends here
