;;; -*- lexical-binding: t; -*-

(require 'excursion)

(let ((test-root-dir "/excursion:localhost#17001:~/test_root/"))
  (excursion--gen-tests
   excursion-file-remote-p
   ((((concat test-root-dir "foo")) "/excursion:localhost#17001:")
    (((concat test-root-dir "foo") 'method) "excursion")
    (((concat test-root-dir "foo") 'host) "localhost#17001")
    (((concat test-root-dir "foo") 'method t) "excursion")
    (((concat test-root-dir "foo") 'host t) "localhost#17001")
    (((concat test-root-dir "foo") 'foo) "/excursion:localhost#17001:")
    (((concat test-root-dir "foo") 'foo t) "/excursion:localhost#17001:"))
   :suffix "connected"
   :setup ((excursion--remote-connection)))

  (excursion--gen-tests
   excursion-file-remote-p
   ((((concat test-root-dir "foo")) "/excursion:localhost#17001:")
    (((concat test-root-dir "foo") 'method) "excursion")
    (((concat test-root-dir "foo") 'host) "localhost#17001")
    (((concat test-root-dir "foo") 'method t))
    (((concat test-root-dir "foo") 'host t))
    (((concat test-root-dir "foo") 'foo) "/excursion:localhost#17001:")
    (((concat test-root-dir "foo") 'foo t)))
   :suffix "not-connected"
   :setup ((excursion-terminate))))

(provide 'file-remote-p-test)
