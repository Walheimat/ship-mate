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

(ert-deftest ship-mate--valid-env-p ()
  (should-not (ship-mate--valid-env-p '("hello" "world")))
  (should-not (ship-mate--valid-env-p "hello=world"))
  (should-not (ship-mate--valid-env-p '("hello=world" "test")))
  (should (ship-mate--valid-env-p '("hello=world" "test=ing"))))

(ert-deftest ship-mate-with-bounded-compilation ()
  (bydi ((:always project-current)
         (:mock project-buffers :with buffer-list))

    (let ((fun (lambda () (funcall compilation-save-buffers-predicate))))

      (should (ship-mate-with-bounded-compilation fun)))))

(ert-deftest ship-mate-with-bounded-compilation--ignored-outside-project ()
  (bydi ((:ignore project-current))

    (let ((fun #'always))

      (should (ship-mate-with-bounded-compilation fun)))))

(ert-deftest ship-mate-command--buffer-name ()
  (let* ((ship-mate--current-command "test")
         (fun (ship-mate-command--buffer-name-function "test")))
    (should (string-equal (funcall fun 'test-mode) "*ship-mate-test-test*")))

  (let ((fun (ship-mate-command--buffer-name-function "test")))
    (should (string-equal (funcall fun 'test-mode) "*ship-mate-compile-test*"))))

(ert-deftest ship-mate-command ()
  (defvar ship-mate-test-default-cmd nil)

  (let ((ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (ship-mate-test-default-cmd "untest")
        (ship-mate-command-history nil)
        (entered-command nil))

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project--value-in-dir :return ship-mate-test-default-cmd)
           compile
           (:mock read-shell-command :return entered-command))

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
  (let ((ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (ship-mate-command-history nil)
        (entered-command "test"))

    (bydi ((:always project-current)
p           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project--value-in-dir :return ship-mate-test-default-cmd)
           compile
           (:mock read-shell-command :return entered-command))

      (ship-mate-command 'test)
      (should (eq 1 (ring-length (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test)))))
      (ship-mate-command 'test)
      (should (eq 1 (ring-length (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test))))))))

(ert-deftest project-command--history--inserts-multiple ()
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
  (let ((fake-history (make-ring 3))
        (ship-mate--last-command-category 'test))

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

(ert-deftest ship-mate-command--rehydrate ()
  (let ((compile-history '("make test"))
        (compile-command "make best")
        (ship-mate--last-command-category nil)
        (history (make-ring 2))
        (matches nil))

    (bydi ((:mock ship-mate-command--history :return history)
           (:mock ship-mate-command--fuzzy-match-p :return matches)
           (:watch compile-history))

      (ring-insert history "make history")

      (ship-mate-command--rehydrate #'ignore)

      (bydi-was-not-set compile-history)

      (setq ship-mate--last-command-category 'test
            matches t)

      (ship-mate-command--rehydrate #'ignore)

      (bydi-was-set-to compile-history '("make history")))))

(ert-deftest ship-mate-create-command ()
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
    (bydi ((:always advice-add))
      (ship-mate-mode--setup)
      (bydi-was-called-n-times advice-add 4))))

(ert-deftest ship-mate-mode--teardown()
  (let ((ship-mate-compile-functions '(recompile)))
    (bydi ((:always advice-remove))
      (ship-mate-mode--teardown)
      (bydi-was-called-n-times advice-remove 4))))

(ert-deftest ship-mate-mode ()
  (bydi (ship-mate-mode--setup
         ship-mate-mode--teardown)

    (let ((ship-mate-mode nil))

      (ship-mate-mode)
      (bydi-was-called ship-mate-mode--setup)

      (ship-mate-mode -1)
      (bydi-was-called ship-mate-mode--teardown))))

;;; -- Env editing

(ert-deftest ship-mate-env--edit--errors-for-non-comp ()
  (should-error (ship-mate-env--edit)))

(ert-deftest ship-mate-env--creates-buffer ()
  (ert-with-test-buffer (:name "env-test")
    (setq major-mode 'compilation-mode)

    (setq compilation-environment '("TES=TING" "MOC=KING"))



    (with-current-buffer (ship-mate-env--edit)

      (should (string= "TES=TING\nMOC=KING" (buffer-string))))))

(ert-deftest ship-mate-env--listify ()
  (ert-with-test-buffer (:name "listify")

    (insert "TES=TING\nMOC=KING")

    (let ((ship-mate-env--buffer-name (buffer-name)))

      (should (equal '("TES=TING" "MOC=KING") (ship-mate-env--listify))))))

(ert-deftest ship-mate-env--validate ()
  (let ((list '("TES=TING" "MOC=KING")))

    (bydi ((:mock ship-mate-env--listify :return list))

      (should-not (ship-mate-env--validate))

      (setq list '("TESTING" "MOC=KING"))

      (should (equal '("Invalid assignments") (ship-mate-env--validate))))))

(ert-deftest ship-mate-env-apply--errors-if-validation-fails ()
  (bydi ((:mock ship-mate-env--validate :return '("Test error a" "Test error b")))
    (should-error (ship-mate-env-apply) :type 'user-error)))

(ert-deftest ship-mate-env-apply ()
  (bydi ((:ignore ship-mate-env--validate)
         (:mock ship-mate-env--listify :return '("TES=TING"))
         (:ignore ship-mate-env--quit)
         (:watch compilation-environment))

    (ert-with-test-buffer (:name "apply")
      (setq ship-mate-env--target-buffer (current-buffer))

      (ship-mate-env-apply)

      (bydi-was-set-to compilation-environment '("TES=TING"))

      (bydi-was-called ship-mate-env--quit))))

(ert-deftest ship-mate-env--quit ()
  (ert-with-test-buffer (:name "quit")
    (let ((ship-mate-env--buffer-name (buffer-name))
          (ship-mate-env--target-buffer (current-buffer)))

      (bydi ((:spy quit-window)
             (:watch ship-mate-env--target-buffer))

        (ship-mate-env--quit)

        (bydi-was-called quit-window)
        (bydi-was-set-to ship-mate-env--target-buffer nil)))))

(ert-deftest ship-mate-env-abort ()
  (bydi (ship-mate-env--quit)

    (ship-mate-env-abort)

    (bydi-was-called ship-mate-env--quit)))

(ert-deftest ship-mate-env-clear ()
  (ert-with-test-buffer (:name "clear")
    (setq-local compilation-environment '("TES=TING"))

    (setq ship-mate-env--target-buffer (current-buffer))

    (bydi (ship-mate-env--quit)
      (ship-mate-env-clear)

      (should-not compilation-environment)
      (bydi-was-called ship-mate-env--quit))))

(ert-deftest ship-mate-edit-environment ()
  (bydi (ship-mate-env--edit)
    (ship-mate-edit-environment)

    (bydi-was-called ship-mate-env--edit)))

;;; ship-mate-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
