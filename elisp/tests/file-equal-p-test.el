;;; -*- lexical-binding: t; -*-

(require 'excursion)

(let ((test-root-dir "/excursion:localhost#17001:~/test_root/file-equal-p/"))
  (excursion--gen-tests
   file-equal-p
   ((((concat test-root-dir "dir1/") (concat test-root-dir "dir1")) t)
    (("/excursion:localhost#17001:/home/user1/test_root/file-equal-p/dir1/"
      "/excursion:localhost#17001:~/test_root/file-equal-p/dir1/") t)
    (("/excursion:localhost#17001:~user1" "/home/user1") nil)
    (((concat test-root-dir "dir1")
      "/excursion:electron:/home/user1/test_root/file-equal-p/dir1") nil)
    (((concat test-root-dir "baz/foo") (concat test-root-dir "dir1/foo")) t))))

(provide 'file-equal-p-test)
