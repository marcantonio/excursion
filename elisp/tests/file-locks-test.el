;;; -*- lexical-binding: t; -*-

(require 'excursion)

(excursion--gen-tests
 excursion-make-lock-file-name
 ((("/excursion:electron:/home/mas/excursion/Cargo.toml")
   "/excursion:electron:/home/mas/excursion/.#Cargo.toml")))

(excursion--gen-tests
 excursion-make-lock-file-name
 ((("/excursion:electron:/home/mas/excursion/Cargo.toml") nil))
 :suffix "create-lockfiles-nil"
 :bindings ((create-lockfiles nil)))

(defmacro with-tidy-lock (&rest body)
  "Clean up lock after BODY."
  `(unwind-protect
       (progn ,@body)
     (unlock-file file)))

(let* ((test-root-dir "/excursion:localhost#17001:~/test_root/lock-file/")
       (file (concat test-root-dir "foo")))
  (ert-deftest excursion-lock-file-acquire-lock-test ()
    (with-tidy-lock
     (should (not (file-locked-p file)))
     (excursion-lock-file file)
     (should (eq (file-locked-p file) t))))

  (ert-deftest excursion-lock-file-already-owned ()
    (with-tidy-lock
     (excursion-lock-file file)
     (should (null (excursion-lock-file file)))
     (should (eq (file-locked-p file) t))))

  (ert-deftest excursion-lock-file-foreign-lock-abort ()
    ;; Required so the real `ask-user-about-lock' doesn't get autoloaded over our
    ;; redefintion in batch mode. Not sure why this is only needed for this one.
    (when noninteractive
      (autoload-do-load (symbol-function 'ask-user-about-lock) 'ask-user-about-lock))
    (with-tidy-lock
     (let* ((lock (make-lock-file-name file))
            (info "other@host.9999"))
       (make-symbolic-link info lock 'ok-if-already-exists)
       (excursion--mock-calls
        (('ask-user-about-lock (signal 'file-locked (list file info))))
        (should-error (excursion-lock-file file) :type 'file-locked)
        (should (equal (file-locked-p file) "other"))))))

  (ert-deftest excursion-lock-file-foreign-lock-steal ()
    (with-tidy-lock
     (let* ((lock (make-lock-file-name file))
            (info "other@host.9999"))
       (make-symbolic-link info lock 'ok-if-already-exists))
     (excursion--mock-calls
      (('ask-user-about-lock t))
      (excursion-lock-file file)
      (should (equal (file-locked-p file) t)))))

  (ert-deftest excursion-lock-file-foreign-lock-proceed ()
    (with-tidy-lock
     (let* ((lock (make-lock-file-name file))
            (info "other@host.9999"))
       (make-symbolic-link info lock 'ok-if-already-exists))
     (excursion--mock-calls
      (('ask-user-about-lock nil))
      (should (null (excursion-lock-file file)))
      (should (equal (file-locked-p file) "other"))))))

(provide 'file-locks-test)
