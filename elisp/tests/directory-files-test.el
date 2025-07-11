;;; -*- lexical-binding: t; -*-

(require 'excursion)

(let ((test-root-dir "/excursion:localhost#17001:~/test_root/directory_files"))
  (excursion--gen-tests
   excursion-directory-files
   (((test-root-dir) '("." ".." "a.foo" "b.foo" "c.bar"))
    ((test-root-dir t) '("." ".."
                         "/home/user1/test_root/directory_files/a.foo"
                         "/home/user1/test_root/directory_files/b.foo"
                         "/home/user1/test_root/directory_files/c.bar"))
    ((test-root-dir nil ".foo$") '("a.foo" "b.foo"))
    ((test-root-dir nil nil t) '("." ".." "c.bar" "a.foo" "b.foo"))
    ((test-root-dir nil nil nil 3) '("." ".." "a.foo"))
    ((test-root-dir t "ar" t 1) '("/home/user1/test_root/directory_files/c.bar")))))

(provide 'directory-files-test)
