;;; ship-mate-test.el --- Tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'ship-mate nil t)

(ert-deftest ship-mate--plist-keys--errors-if-invalid ()
  (should-error (ship-mate--plist-keys '(:test a :best))))

(ert-deftest ship-mate--plist-keys--extracts-keys ()
  (should (equal '(:test :this :function) (ship-mate--plist-keys '(:test "whether" :this "hacky" :function "works")))))

(ert-deftest ship-mate-environment--valid-env-p ()
  :tags '(user-facing)

  (should-not (ship-mate-environment--valid-env-p '("hello" "world")))
  (should-not (ship-mate-environment--valid-env-p "hello=world"))
  (should-not (ship-mate-environment--valid-env-p '("hello=world" "test")))
  (should (ship-mate-environment--valid-env-p '("hello=world" "test=ing")))
  (should-not (ship-mate-environment--valid-env-p '("hello=world" "test=ing" ""))))

(ert-deftest ship-mate-with-bounded-compilation ()
  :tags '(bounded-comp)

  (bydi ((:always project-current)
         (:mock project-buffers :with buffer-list))

    (let ((fun (lambda () (funcall compilation-save-buffers-predicate))))

      (should (ship-mate-with-bounded-compilation fun)))))

(ert-deftest ship-mate-with-bounded-compilation--ignored-outside-project ()
  :tags '(bounded-comp)

  (bydi ((:ignore project-current))

    (let ((fun #'always))

      (should (ship-mate-with-bounded-compilation fun)))))

(ert-deftest ship-mate-command--buffer-name ()
  :tags '(command)

  (let* ((ship-mate-command--current-command-name "test")
         (fun (ship-mate-command--buffer-name-function "test")))
    (should (string-equal (funcall fun 'test-mode) "*ship-mate-test-test*")))

  (let ((fun (ship-mate-command--buffer-name-function "test")))
    (should (string-equal (funcall fun 'test-mode) "*ship-mate-compile-test*"))))

(ert-deftest ship-mate-command ()
  :tags '(command)

  (defvar ship-mate-test-default-cmd nil)

  (let ((ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (ship-mate-test-default-cmd "untest")
        (ship-mate-command-history nil)
        (entered-command nil))

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project--value-in-dir :return ship-mate-test-default-cmd)
           (:mock compile :return (current-buffer))
           (:mock read-shell-command :return entered-command)
           ship-mate-dinghy-mode)

      (setq entered-command "test")

      (ship-mate-command 'test t)
      (bydi-was-called-with read-shell-command (list "Test project (Test Project): " "untest" 'ship-mate-command-history))
      (bydi-was-called-with compile '("test" nil))

      (setq entered-command "best")
      (ship-mate-command 'test t)
      (bydi-was-called-with compile '("best" nil))

      (should (string-equal "test" (ring-ref (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test)) 1)))
      (should (string-equal "best" (ring-ref (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test)) 0))))))

(ert-deftest ship-mate-command--only-inserted-once ()
  :tags '(command)

  (let ((ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (ship-mate-command-history nil)
        (entered-command "test"))

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project--value-in-dir :return ship-mate-test-default-cmd)
           (:mock compile :return (current-buffer))
           (:mock read-shell-command :return entered-command)
           ship-mate-dinghy-mode)

      (ship-mate-command 'test)
      (should (eq 1 (ring-length (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test)))))
      (ship-mate-command 'test)
      (should (eq 1 (ring-length (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test))))))))

(ert-deftest project-command--history--inserts-multiple ()
  :tags '(command)

  (let ((ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (ship-mate-command-history nil)
        (ship-mate-test-default-cmd '("make test" "test make")))

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project--value-in-dir :return ship-mate-test-default-cmd))

      (let ((history (ship-mate-command--history 'test)))

        (should (string= "make test" (ring-ref history 0)))
        (should (string= "test make" (ring-ref history 1)))))))

(ert-deftest project-command--valid-default-p ()
  (should (ship-mate-command--valid-default-p "test"))
  (should (ship-mate-command--valid-default-p '("test" "make")))
  (should-not (ship-mate-command--valid-default-p '("test" make))))

(ert-deftest ship-mate-command--update-history ()
  :tags '(command)

  (let ((fake-history (make-ring 3))
        (ship-mate-command--last-command 'test))

    (ring-insert fake-history "make test")

    (bydi ((:mock ship-mate-command--history :return fake-history))

      (ship-mate-command--update-history "make new")

      (should (equal (ring-elements fake-history)
                     '("make test")))

      (ship-mate-command--update-history "make test FLAG=t")

      (should (equal (ring-elements fake-history)
                     '("make test FLAG=t" "make test")))

      (ship-mate-command--update-history "make way")

      (should (equal (ring-elements fake-history)
                     '("make test FLAG=t" "make test")))

      (ship-mate-command--update-history "make test")

      (should (equal (ring-elements fake-history)
                     '("make test" "make test FLAG=t"))))))

(ert-deftest ship-mate-command--capture ()
  :tags '(command)

  (let ((compile-history '("make test"))
        (compile-command "make best")
        (ship-mate-command--last-command nil)
        (history (make-ring 2))
        (matches nil))

    (bydi ((:mock ship-mate-command--history :return history)
           (:mock ship-mate-command--fuzzy-match-p :return matches)
           (:mock ship-mate--local-value :return environment)
           ship-mate-command)

      (ring-insert history "make history")

      (ship-mate-command--capture #'ignore)

      (bydi-was-not-called ship-mate-command)

      (setq ship-mate-command--last-command 'test
            matches t)

      (ship-mate-command--capture #'ignore)

      (bydi-was-called ship-mate-command))))

(ert-deftest ship-mate-create-command ()
  :tags '(user-facing command)

  (bydi ((:mock make-hash-table :return 'hash-table))
    (bydi-match-expansion
     (ship-mate-create-command test)
     '(progn
        (defvar-local ship-mate-test-default-cmd nil "Default for `ship-mate-test'.")
        (defun ship-mate-test (&optional arg) "Test the current project.\n\nSee `ship-mate-command' for behavior of ARG."
               (interactive "P")
               (ship-mate-command 'test arg))
        (setq ship-mate-commands (plist-put ship-mate-commands 'test hash-table))
        (define-key ship-mate-command-map "t" 'ship-mate-test)
        (put 'ship-mate-test-default-cmd 'safe-local-variable #'ship-mate-command--valid-default-p)))
    (bydi-match-expansion
     (ship-mate-create-command test :key "o" :default "make all")
     '(progn
        (defvar-local ship-mate-test-default-cmd "make all" "Default for `ship-mate-test'.")
        (defun ship-mate-test (&optional arg) "Test the current project.\n\nSee `ship-mate-command' for behavior of ARG."
               (interactive "P")
               (ship-mate-command 'test arg))
        (setq ship-mate-commands (plist-put ship-mate-commands 'test hash-table))
        (define-key ship-mate-command-map "o" 'ship-mate-test)
        (put 'ship-mate-test-default-cmd 'safe-local-variable #'ship-mate-command--valid-default-p)))))

(ert-deftest ship-mate-select-command ()
  :tags '(user-facing command)

  (bydi ((:mock completing-read :return "test")
         ship-mate-command)
    (call-interactively 'ship-mate-select-command)

    (bydi-was-called-with ship-mate-command (list 'test))))

(ert-deftest ship-mate--local-value ()
  (ert-with-temp-file project

    (bydi ((:always project-current)
           (:mock project-root :return project)
           (:mock project--value-in-dir :return 'text-mode))

      (should (equal (ship-mate--local-value 'major-mode) 'text-mode)))))

(ert-deftest ship-mate-mode--setup ()
  (let ((ship-mate-compile-functions '(recompile)))
    (bydi ((:risky-mock advice-add :with always)
           (:risky-mock add-hook :with always))
      (ship-mate-mode--setup)
      (bydi-was-called-n-times advice-add 4)
      (bydi-was-called-n-times add-hook 1))))

(ert-deftest ship-mate-mode--teardown()
  (let ((ship-mate-compile-functions '(recompile)))
    (bydi ((:risky-mock advice-remove :with always)
           (:risky-mock remove-hook :with always))
      (ship-mate-mode--teardown)
      (bydi-was-called-n-times advice-remove 4)
      (bydi-was-called-n-times remove-hook 1))))

(ert-deftest ship-mate-mode ()
  :tags '(user-facing)

  (bydi (ship-mate-mode--setup
         ship-mate-mode--teardown)

    (let ((ship-mate-mode nil))

      (ship-mate-mode)
      (bydi-was-called ship-mate-mode--setup)

      (ship-mate-mode -1)
      (bydi-was-called ship-mate-mode--teardown))))

;;; -- Env editing

(ert-deftest ship-mate-environment--edit--errors-for-non-comp ()
  :tags '(user-facing env)

  (should-error (ship-mate-environment--edit)))

(ert-deftest ship-mate-environment--creates-buffer ()
  :tags '(user-facing env)

  (ert-with-test-buffer (:name "env-test")
    (setq major-mode 'compilation-mode)

    (setq compilation-environment '("TES=TING" "MOC=KING"))

    (with-current-buffer (ship-mate-environment--edit)

      (should (string= "TES=TING\nMOC=KING" (buffer-string))))))

(ert-deftest ship-mate-environment--listify ()
  :tags '(env)

  (ert-with-test-buffer (:name "listify")

    (insert "TES=TING\n\nMOC=KING")

    (let ((ship-mate-environment--buffer-name (buffer-name)))

      (should (equal '("TES=TING" "MOC=KING") (ship-mate-environment--listify))))))

(ert-deftest ship-mate-environment--validate ()
  :tags '(env)

  (let ((list '("TES=TING" "MOC=KING")))

    (bydi ((:mock ship-mate-environment--listify :return list))

      (should-not (ship-mate-environment--validate))

      (setq list '("TESTING" "MOC=KING"))

      (should (equal '("Invalid assignments") (ship-mate-environment--validate))))))

(ert-deftest ship-mate-environment-apply--errors-if-validation-fails ()
  :tags '(user-facing env)

  (bydi ((:mock ship-mate-environment--validate :return '("Test error a" "Test error b")))
    (should-error (ship-mate-environment-apply) :type 'user-error)))

(ert-deftest ship-mate-environment-apply ()
  :tags '(user-facing env)

  (bydi ((:ignore ship-mate-environment--validate)
         (:mock ship-mate-environment--listify :return '("TES=TING"))
         (:ignore ship-mate-environment--quit)
         (:watch compilation-environment))

    (ert-with-test-buffer (:name "apply")
      (setq ship-mate-environment--target-buffer (current-buffer))

      (ship-mate-environment-apply)

      (bydi-was-set-to compilation-environment '("TES=TING"))

      (bydi-was-called ship-mate-environment--quit))))

(ert-deftest ship-mate-environment--quit ()
  :tags '(user-facing env)

  (ert-with-test-buffer (:name "quit")
    (let ((ship-mate-environment--buffer-name (buffer-name))
          (ship-mate-environment--target-buffer (current-buffer)))

      (bydi ((:spy quit-window)
             (:watch ship-mate-environment--target-buffer))

        (ship-mate-environment--quit)

        (bydi-was-called quit-window)
        (bydi-was-set-to ship-mate-environment--target-buffer nil)))))

(ert-deftest ship-mate-environment-abort ()
  :tags '(user-facing env)

  (bydi (ship-mate-environment--quit)

    (ship-mate-environment-abort)

    (bydi-was-called ship-mate-environment--quit)))

(ert-deftest ship-mate-environment-clear ()
  :tags '(user-facing env)

  (ert-with-test-buffer (:name "clear")
    (setq-local compilation-environment '("TES=TING"))

    (setq ship-mate-environment--target-buffer (current-buffer))

    (bydi (ship-mate-environment--quit)
      (ship-mate-environment-clear)

      (should-not compilation-environment)
      (bydi-was-called ship-mate-environment--quit))))

(ert-deftest ship-mate-edit-environment ()
  :tags '(user-facing env)

  (bydi (ship-mate-environment--edit)
    (ship-mate-edit-environment)

    (bydi-was-called ship-mate-environment--edit)))

;;; -- Dinghy

(ert-deftest ship-mate-dinghy-mode ()
  (bydi (ship-mate-dinghy--reset-header-line-format)

    (ert-with-test-buffer (:name "dinghy")
      (should-error (ship-mate-dinghy-mode))

      (setq major-mode 'compilation-mode)

      (ship-mate-dinghy-mode)

      (bydi-was-called ship-mate-dinghy--reset-header-line-format))))

(ert-deftest ship-mate-dinghy--maybe-enable ()
  (bydi (ship-mate-dinghy-mode)

    (let ((ship-mate-dinghy-enable nil))

      (ship-mate-dinghy--maybe-enable)

      (bydi-was-not-called ship-mate-dinghy-mode)

      (setq ship-mate-dinghy-enable t)

      (ship-mate-dinghy--maybe-enable)

      (bydi-was-called ship-mate-dinghy-mode))))

(ert-deftest ship-mate-dinghy--print-variables ()
  (let ((compilation-environment nil))

    (should (string= "none" (ship-mate-dinghy--print-variables)))

    (setq compilation-environment '("TES=TING" "MOC=KING"))

    (should (string= "TES MOC" (ship-mate-dinghy--print-variables)))

    (setq compilation-environment '("TES=TING" "MOC=KING" "TRY=ING" "MY=BEST"))

    (should (string= "active" (ship-mate-dinghy--print-variables)))))

(ert-deftest ship-mate-dinghy--reset-header-line-format ()
  (bydi ((:mock ship-mate-dinghy--print-variables :return "test"))
    (ert-with-test-buffer (:name "header")
      (setq-local ship-mate-dinghy-mode t)

      (ship-mate-dinghy--reset-header-line-format)

      (should (string= header-line-format "env[test]"))

      (ship-mate-dinghy--reset-header-line-format (current-buffer))

      (should (string= header-line-format "env[test]")))))

;;; ship-mate-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
