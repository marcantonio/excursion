;;; -*- lexical-binding: t; -*-

(require 'excursion)

(excursion--gen-tests
 file-attributes
 ((("/excursion:electron:~/otium")
   '(t
     2 1000 1000
     :ignore
     (26340 59827 900015 115737)
     (26340 59827 900015 115737)
     4096 "drwxrwxr-x" nil 570122
     (-2 . 64769)))
  (("/excursion:electron:~/.bashrc")
   '(nil
     1 1000 1000
     :ignore
     :ignore
     :ignore
     3838 "-rw-r--r--" nil 557949
     (-2 . 64769)))
  (("/excursion:electron:~/foo")
   '("/home/mas/.bashrc"
     1 1000 1000
     :ignore
     (26546 46476 452400 684356)
     (26546 46476 452400 684356)
     17 "lrwxrwxrwx" nil 556071
     (-2 . 64769)))
  (("/excursion:electron:~/nope") nil))
 :eq-pred (lambda (actual expected)
            (cl-every
             (lambda (a e)
               (or (equal :ignore e)
                   (equal a e)))
             actual expected)))

(excursion--gen-tests
 file-readable-p
 ((("/excursion:electron:~/.bashrc") t)
  (("/excursion:electron:otium") t)
  (("/excursion:electron:/etc/shadow") nil)
  (("/excursion:electron:nope") nil)
  (("/excursion:electron:excursion/foo") nil)))

(excursion--gen-tests
 file-writable-p
 ((("/excursion:electron:foo") t)
  (("/excursion:electron:otium") t)
  (("/excursion:electron:/etc/shadow") nil)
  (("/excursion:electron:nope/foo") nil)
  (("/excursion:electron:excursion/foo") t)))

(excursion--gen-tests
 file-exists-p
 ((("/excursion:electron:otium") t)
  (("/excursion:electron:/root") t)
  (("/excursion:electron:nope") nil)))

(excursion--gen-tests
 file-truename
 ((("/excursion:electron:~/otium") "/excursion:electron:/home/mas/otium")
  (("/excursion:electron:foo") "/excursion:electron:/home/mas/.bashrc")
  (("/excursion:electron:nope") "/excursion:electron:/home/mas/nope")
  (("foo") "/home/mas/mm/foo"))
 :bindings ((default-directory "/home/mas/mm/")))

;; Fail on non-excursion files
(ert-deftest excursion-file-truename-test ()
  :expected-result :failed
  (should (equal (excursion-file-truename "foo") "/home/mas/mm/foo")))

(excursion--gen-tests
 file-directory-p
 ((("/excursion:electron:~/otium") t)
  (("/excursion:electron:foo") nil)
  (("/excursion:electron:nope") nil)))

(provide 'file-attributes-test)
