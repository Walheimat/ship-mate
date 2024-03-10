;;; ship-mate-dinghy.el --- Context info and bindings -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/ship-mate
;; Version: 0.5.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

;;; Commentary:

;;; `ship-mate-dinghy-mode' adds additional context (showing the
;;; current command and compilation environment) and functionality
;;; (binding useful commands to `C-c' keys) to `ship-mate' buffers.
;;;
;;; The mode is automatically enabled if you run
;;; `ship-mate-dinghy-global-mode' in all `ship-mate' buffers.

;;; Code:

(require 'ship-mate)

;;;; Variables

(defvar-local ship-mate-dinghy--command nil)

(defvar ship-mate-dinghy-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "C-c [") #'ship-mate-command-next-buffer)
    (define-key map (kbd "C-c ]") #'ship-mate-command-prev-buffer)

    map)
  "Map used in buffers that enable `ship-mate-dinghy-mode'.")

(defun ship-mate-dinghy--maybe-enable (&optional process)
  "Enable `ship-mate-dinghy-mode' if not disabled.

If PROCESS is passed, set the name."
  (if (ship-mate--command-buffer-p)
      (progn
        (when process
          (setq ship-mate-dinghy--command (process-command process)))

        (setq ship-mate--last-compilation-type 'ship-mate)

        (ship-mate-dinghy-mode))

    (setq ship-mate--last-compilation-type 'other)))

(defun ship-mate-dinghy--print-command ()
  "Print the command."
  (if (and ship-mate-dinghy--command (listp ship-mate-dinghy--command))

      (let* ((rest (seq-drop-while (lambda (it) (string-match-p "^\\(\/\\|-\\)" it)) ship-mate-dinghy--command))
             (full-command (string-join ship-mate-dinghy--command " "))
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
                       (string-match ship-mate-environment--regex it)
                       (propertize (match-string 1 it) 'help-echo (format "Variable set to: %s" (match-string 2 it))))
                     compilation-environment
                     " ")
                    'face 'mode-line-emphasis))
    (propertize "none" 'face 'mode-line-inactive)))

;;;; Local mode

(define-minor-mode ship-mate-dinghy-mode
  "Minor mode to provide contextual information and bindings.

\\{ship-mate-dinghy-mode-map}"
  :lighter " smd"

  (unless (derived-mode-p 'compilation-mode)
    (user-error "`ship-mate-dinghy-mode' can only be enabled in compilation buffers"))

  (ship-mate-dinghy--reset-header-line-format))

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

;;;; Global mode

(defun ship-mate-dinghy-global-mode--setup ()
  "Set up global mode."
  (add-hook 'ship-mate-environment-set-hook 'ship-mate-dinghy--reset-header-line-format)
  (add-hook 'compilation-start-hook 'ship-mate-dinghy--maybe-enable))

(defun ship-mate-dinghy-global-mode--teardown ()
  "Tear down global mode."
  (remove-hook 'ship-mate-environment-set-hook 'ship-mate-dinghy--reset-header-line-format)
  (remove-hook 'compilation-start-hook 'ship-mate-dinghy--maybe-enable))

;;;; API

;;;###autoload
(define-minor-mode ship-mate-dinghy-global-mode
  "Sets up `ship-mate-dinghy' mode for all command buffers."
  :global t
  :group 'ship-mate-dinghy
  (if ship-mate-dinghy-global-mode
      (ship-mate-dinghy-global-mode--setup)
    (ship-mate-dinghy-global-mode--teardown)))

(provide 'ship-mate-dinghy)

;;; ship-mate-dinghy.el ends here
