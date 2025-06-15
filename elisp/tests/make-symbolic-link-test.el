;;; -*- lexical-binding: t; -*-

(require 'excursion)

;; TODO: Super ghetto until delete-file works
(defun del-file (path)
  (call-process
   "ssh" nil nil nil
   "-p" "7022"
   "-oBatchMode=yes"
   "electron.soda.fm"
   (concat "rm " path)))

;; TODO: Check for updated timestamps in all of these

(ert-deftest make-symbolic-link-both-remote-test ()
  (let ((target "/excursion:electron:/home/mas/excursion/Cargo.toml")
        (linkname "/excursion:electron:/home/mas/excursion/Cargo.ln"))
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
      (del-file (excursion--parse-filename linkname 'file)))))

(ert-deftest make-symbolic-link-remote-target-test ()
  (let ((target "/excursion:electron:/home/mas/excursion/Cargo.toml")
        (linkname "/home/mas/Code/excursion/Cargo.ln"))
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
      (if (excursion--file-p linkname)
          (del-file (excursion--parse-filename linkname 'file))
        (delete-file linkname)))))

(ert-deftest make-symbolic-link-remote-linkname-test ()
  (let ((target "foo")
        (linkname "/excursion:electron:/home/mas/excursion/Cargo.ln"))
    (unwind-protect
        (progn
          (excursion-make-symbolic-link target linkname)
          (should (not (file-exists-p linkname))) ; broken links fail
          (should (file-symlink-p linkname))
          (excursion-make-symbolic-link target linkname) ; broken links not signal here
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t))) ; say yes
            (excursion-make-symbolic-link target linkname 1))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) nil))) ; say no
            (excursion-make-symbolic-link target linkname 1)) ; broken links not signal here
          (excursion-make-symbolic-link target linkname t))
      (del-file (excursion--parse-filename linkname 'file)))))

(ert-deftest make-symbolic-link-local-test ()
  (let ((target "/home/mas/Code/excursion/Cargo.toml")
        (linkname "/home/mas/Code/excursion/Cargo.ln"))
    (unwind-protect
        (progn
          (excursion-make-symbolic-link target linkname)
          (should (file-symlink-p linkname)))
      (delete-file linkname))))

(provide 'make-symbolic-link-test)
