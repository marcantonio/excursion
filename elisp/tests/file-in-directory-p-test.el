;;; -*- lexical-binding: t; -*-

(require 'excursion)

(let ((test-root-dir "/excursion:localhost#17001:~/test_root/file-in-directory-p/"))
  (excursion--gen-tests
   file-in-directory-p
   ((((concat test-root-dir "dir1/foo") (concat test-root-dir "dir1")) t)
    (((concat test-root-dir "dir1/fo") (concat test-root-dir "dir1")) t)    ; file doesn't need to exists...
    (((concat test-root-dir "dir2/foo") (concat test-root-dir "dir2")) nil) ; ...but the directory does
    (("/excursion:localhost#17001:/dev" "/") nil)                           ; mixing local and remote fails
    (((concat test-root-dir "dir1/foo")                                     ; mixing remotes fails
      "/excursion:neutron:/home/user1/test_root/file-in-directory-p/dir1") nil))))

(provide 'file-in-directory-p-test)
