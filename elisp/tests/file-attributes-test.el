;;; -*- lexical-binding: t; -*-

(require 'excursion)

(let ((test-root-dir "/excursion:localhost#17001:~/test_root/file-attributes/"))
  (excursion--gen-tests
   excursion-file-attributes
   ((((concat test-root-dir "dir1"))
     '(t
       2 1001 1001
       (26750 58061 0 0)
       (26750 58061 0 0)
       :ignore
       :ignore "drwxrwxr-x" nil
       :ignore
       (-2 . 44)))
    (((concat test-root-dir "foo"))
     '(nil
       1 1001 1001
       (26750 58061 0 0)
       (26750 58061 0 0)
       :ignore
       0 "-rw-rw-r--" nil
       :ignore
       (-2 . 44)))
    (((concat test-root-dir "/bar"))
     '("foo"
       1 1001 1001
       (26752 13389 0 0)
       (26752 13389 0 0)
       :ignore
       3 "lrwxr-xr-x" nil
       :ignore
       (-2 . 44)))
    (((concat test-root-dir "/not-a-file")) nil))
   :eq-pred (lambda (actual expected)
              (and (or actual (not expected)) ; make sure actual is defined unless we expect nil
                   (cl-every                  ; because cl-every will return t on nil...
                    (lambda (a e)
                      (or (equal :ignore e)
                          (equal a e)))
                    actual expected))))

  (excursion--gen-tests
   excursion-file-readable-p
   ((((concat test-root-dir "foo")) t)
    (((concat test-root-dir "dir1")) t)
    (("/excursion:localhost#17001:/etc/shadow") nil)
    (("/excursion:localhost#17001:not-a-file") nil)
    (((concat test-root-dir "baz")) t)
    (((concat test-root-dir "qux")) nil)))

  (excursion--gen-tests
   excursion-file-writable-p
   ((((concat test-root-dir "foo")) t)
    (((concat test-root-dir "dir1")) t)
    (("/excursion:localhost#17001:/etc/shadow") nil)
    (("/excursion:localhost#17001:not-a-dir/foo") nil)
    (((concat test-root-dir "dir1/foo")) t)))

  (excursion--gen-tests
   excursion-file-exists-p
   ((((concat test-root-dir "dir1")) t)
    (((concat test-root-dir "no")) nil)
    (("/excursion:localhost#17001:/root") t)
    (("/excursion:localhost#17001:not-a-file") nil)))

  (excursion--gen-tests
   excursion-file-truename
   ((((concat test-root-dir "dir1")) "/excursion:localhost#17001:/home/user1/test_root/file-attributes/dir1")
    (((concat test-root-dir "bar")) "/excursion:localhost#17001:/home/user1/test_root/file-attributes/foo")
    (((concat test-root-dir "not-a-file")) "/excursion:localhost#17001:/home/user1/test_root/file-attributes/not-a-file")
    (((concat test-root-dir "baz/mm")) "/excursion:localhost#17001:/home/user1/test_root/file-attributes/dir1/mm"))
   :bindings ((default-directory "/home/mas/mm/")))

  ;; Fail on non-excursion files
  (ert-deftest excursion-file-truename-test ()
    :expected-result :failed
    (should (equal (excursion-file-truename "foo"))))

  (excursion--gen-tests
   excursion-file-directory-p
   ((((concat test-root-dir "dir1")) t)
    (((concat test-root-dir "foo")) nil)
    (((concat test-root-dir "baz")) t)
    (((concat test-root-dir "not-a-file")) nil))))

(provide 'file-attributes-test)
