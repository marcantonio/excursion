;;; -*- lexical-binding: t; -*-

(require 'excursion)

;; TODO: Check for updated timestamps in all of these

(let ((test-root-dir "/excursion:localhost#17001:~/test_root/make-symbolic-link/"))
  (ert-deftest make-symbolic-link-both-remote-test ()
    (let ((target (concat test-root-dir "foo"))
          (linkname (concat test-root-dir "bar")))
      (unwind-protect
          (progn
            (excursion-make-symbolic-link target linkname)
            (should (file-exists-p linkname))
            (should-error (excursion-make-symbolic-link target linkname)
                          :type 'file-already-exists)
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t))) ; say yes
              (excursion-make-symbolic-link target linkname 1))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) nil))) ; say no
              (should-error (excursion-make-symbolic-link target linkname 1)
                            :type 'file-already-exists))
            (excursion-make-symbolic-link target linkname t))
        (delete-file linkname))))

  (ert-deftest make-symbolic-link-remote-target-test ()
    (let ((target (concat test-root-dir "foo"))
          (linkname (concat temporary-file-directory "bar")))
      (unwind-protect
          (progn
            (excursion-make-symbolic-link target linkname)
            (should (not (file-exists-p linkname))) ; broken links fail
            (should (file-symlink-p linkname))
            (should-error (excursion-make-symbolic-link target linkname)
                          :type 'file-already-exists)
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t))) ; say yes
              (excursion-make-symbolic-link target linkname 1))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) nil))) ; say no
              (should-error (excursion-make-symbolic-link target linkname 1)
                            :type 'file-already-exists))
            (excursion-make-symbolic-link target linkname t))
        (delete-file linkname))))

  (ert-deftest make-symbolic-link-remote-linkname-test ()
    (let ((target "baz")
          (linkname (concat test-root-dir "bar")))
      (unwind-protect
          (progn
            (excursion-make-symbolic-link target linkname)

            (should (not (file-exists-p linkname))) ; broken links fail
            (should (file-symlink-p linkname))
            (should-error (excursion-make-symbolic-link target linkname)
                          :type 'file-already-exists)
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t))) ; say yes
              (excursion-make-symbolic-link target linkname 1))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) nil)))                 ; say no
              (should-error (excursion-make-symbolic-link target linkname 1)
                            :type 'file-already-exists))
            (excursion-make-symbolic-link target linkname t))
        (delete-file linkname)))))

(ert-deftest make-symbolic-link-local-test ()
  (let ((target "foo")
        (linkname (concat temporary-file-directory "bar")))
    (unwind-protect
        (progn
          (excursion-make-symbolic-link target linkname)
          (should (file-symlink-p linkname)))
      (delete-file linkname))))

(provide 'make-symbolic-link-test)
