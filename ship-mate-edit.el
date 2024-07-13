;;; ship-mate-edit.el --- Edit history and environment -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/ship-mate
;; Version: 0.6.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

;;; Commentary:

;; Conveniently edit a command's environment and history in a separate
;; buffer.

;;; Code:

(require 'ship-mate)
(require 'ship-mate-dinghy)

;;;; Variables

(defvar ship-mate-edit-environment-buffer-name "*ship-mate-edit-env*"
  "The name of the buffer used for `ship-mate-edit-environment'.")

(defvar ship-mate-edit-environment--target-buffer nil
  "The buffer `ship-mate-edit-environment' was called from.")

(defvar ship-mate-edit-environment-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-c" #'ship-mate-edit-environment-apply)
    (define-key map "\C-c\C-q" #'ship-mate-edit-environment-clear)
    (define-key map "\C-c\C-k" #'ship-mate-edit-environment-abort)
    map)
  "Map used in buffer created by `ship-mate-edit-environment'.")

(defvar ship-mate-edit-history-buffer-name "*ship-mate-edit-history*"
  "The name of the buffer used for `ship-mate-edit-history'.")

(defvar ship-mate-edit-history--command nil
  "The symbol of the command currently edited.")

(defvar ship-mate-edit-history-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-c" #'ship-mate-edit-history-apply)
    (define-key map "\C-c\C-q" #'ship-mate-edit-history-clear)
    (define-key map "\C-c\C-k" #'ship-mate-edit-history-abort)
    map)
  "Map used in buffer created by `ship-mate-edit-history'.")

;;;; Common

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

;;;; Editing environment

(define-minor-mode ship-mate-edit-environment-mode
  "Minor mode to edit the environment.

\\{ship-mate-edit-environment-mode-map}"
  :lighter " sme"
  (setq-local header-line-format
              (substitute-command-keys
               "\\<ship-mate-edit-environment-mode-map>\
`\\[ship-mate-edit-environment-apply]' applies and recompiles, \
`\\[ship-mate-edit-environment-clear]' clears all env variables, \
`\\[ship-mate-edit-environment-abort]' reverts.")))

(defun ship-mate-edit-environment--internal ()
  "Edit environment in the current buffer."
  (unless (ship-mate--command-buffer-p (current-buffer))
    (user-error "Can only edit `ship-mate' command buffers"))

  (setq ship-mate-edit-environment--target-buffer (current-buffer))

  (ship-mate-edit-environment--in-buffer (buffer-local-value 'compilation-environment ship-mate-edit-environment--target-buffer)))

(defun ship-mate-edit-environment--in-buffer (environment)
  "Create the buffer for editing the ENVIRONMENT."
  (ship-mate-edit--in-buffer ship-mate-edit-environment-buffer-name
                             environment
                             'ship-mate-edit-environment-mode))

(defun ship-mate-edit-environment--validate ()
  "Validate the current edit state."
  (let ((new-state (ship-mate-edit-environment--listify))
        (warnings nil))

    (unless (ship-mate-environment--valid-env-p new-state)
      (push "Invalid assignments" warnings))

    warnings))

(defun ship-mate-edit-environment--listify ()
  "Listify the environment buffer."
  (ship-mate-edit--listify-buffer (get-buffer ship-mate-edit-environment-buffer-name)))

(defun ship-mate-edit-environment-apply ()
  "Apply the edited environment."
  (interactive)

  (when-let ((warnings (ship-mate-edit-environment--validate)))
    (user-error (string-join warnings ", ")))

  (ship-mate-edit-environment--set-environment (ship-mate-edit-environment--listify))
  (ship-mate-edit-environment--quit))

(defun ship-mate-edit-environment--quit ()
  "Quit the editing."
  (quit-window t (get-buffer-window ship-mate-edit-environment-buffer-name t))

  (setq ship-mate-edit-environment--target-buffer nil))

(defun ship-mate-edit-environment-abort ()
  "Abort editing."
  (interactive)

  (ship-mate-edit-environment--quit))

(defun ship-mate-edit-environment-clear ()
  "Clear the environment."
  (interactive)

  (ship-mate-edit-environment--set-environment nil)
  (ship-mate-edit-environment--quit))

(defun ship-mate-edit-environment--set-environment (env)
  "Set `compilation-environment' to ENV.

This is set in buffer `ship-mate-edit-environment-buffer-name'."
  (with-current-buffer ship-mate-edit-environment--target-buffer
    (setq-local compilation-environment env)
    (run-hooks 'ship-mate-environment-set-hook)))

;;;; Editing history

(define-minor-mode ship-mate-edit-history-mode
  "Minor mode to edit the history.

\\{ship-mate-edit-history-mode-map}"
  :lighter " smh"
  (setq-local header-line-format
              (substitute-command-keys
               "\\<ship-mate-edit-history-mode-map>\
`\\[ship-mate-edit-history-apply]' applies, \
`\\[ship-mate-edit-history-clear]' clears the history, \
`\\[ship-mate-edit-history-abort]' reverts.")))

(defun ship-mate-edit-history--internal ()
  "Edit the history of the current buffer."
  (unless (ship-mate--command-buffer-p (current-buffer))
    (user-error "Can only edit `ship-mate' command buffer"))

  (setq ship-mate-edit-history--command (buffer-local-value 'ship-mate--this-command (current-buffer)))

  (ship-mate-edit-history--in-buffer))

(defun ship-mate-edit-history--in-buffer ()
  "Edit the history in a buffer."
  (let ((history (ship-mate-command--history ship-mate-edit-history--command)))

    (ship-mate-edit--in-buffer ship-mate-edit-history-buffer-name
                               (ring-elements history)
                               'ship-mate-edit-history-mode)))

(defun ship-mate-edit-history-apply ()
  "Apply the edited history."
  (interactive)
  (ship-mate-edit-history--set-history (ship-mate-edit-history--listify))
  (ship-mate-edit-history--quit))

(defun ship-mate-edit-history-clear ()
  "Clear the history."
  (interactive)

  (ship-mate-edit-history--set-history nil)
  (ship-mate-edit-history--quit))

(defun ship-mate-edit-history-abort ()
  "Abort editing history."
  (interactive)

  (ship-mate-edit-history--quit))

(defun ship-mate-edit-history--set-history (new-elements)
  "Set the edited HISTORY to include NEW-ELEMENTS."
  (let ((history (ship-mate-command--history ship-mate-edit-history--command)))

    (ring-resize history 0)
    (ring-resize history ship-mate-command-history-size)

    (dolist (it new-elements)
      (ring-insert history it))))

(defun ship-mate-edit-history--quit ()
  "Quit the history editing buffer."
  (quit-window t (get-buffer-window ship-mate-edit-history-buffer-name t))

  (setq ship-mate-edit-history--command nil))

(defun ship-mate-edit-history--listify ()
  "Listify history buffer."
  (reverse (ship-mate-edit--listify-buffer (get-buffer ship-mate-edit-history-buffer-name))))

;;;; Utility

(defun ship-mate-edit--listify-buffer (buffer)
  "Listify the content of BUFFER."
  (with-current-buffer buffer
    (let* ((raw (buffer-string)))

      (seq-filter (lambda (it) (not (string-empty-p it))) (string-split raw "\n")))))

;;;; API

;;;###autoload
(defun ship-mate-edit-environment (buffer)
  "Edit the `compilation-environment' for BUFFER.

If BUFFER isn't a compilation buffer, this prompts to select one."
  (interactive
   (list (if (ship-mate--command-buffer-p)
             (current-buffer)
           (ship-mate--complete-buffer "Edit environment for buffer: "))))

  (with-current-buffer buffer

    (ship-mate-edit-environment--internal)))

;;;###autoload
(defun ship-mate-edit-history (buffer)
  "Edit the history for BUFFER.

If BUFFER isn't a compilation buffer, this prompts to select one."
  (interactive
   (list (if (ship-mate--command-buffer-p)
             (current-buffer)
           (ship-mate--complete-buffer "Edit history for buffer: "))))

  (with-current-buffer buffer
    (ship-mate-edit-history--internal)))

;;;###autoload
(defun ship-mate-edit-setup-bindings ()
  "Set up bindings in two command maps."
  (let ((map ship-mate-command-map))

    (define-key map (kbd ",") #'ship-mate-edit-environment)
    (define-key map (kbd ".") #'ship-mate-edit-history))

  (let ((map ship-mate-dinghy-mode-map))
    (define-key map (kbd "C-c ,") #'ship-mate-edit-environment)
    (define-key map (kbd "C-c .") #'ship-mate-edit-history)))

(provide 'ship-mate-edit)

;;; ship-mate-edit.el ends here
