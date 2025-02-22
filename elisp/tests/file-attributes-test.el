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
     (26472 46601 118689 298629)
     (26472 46601 118689 298629)
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
 ((("/excursion:electron:otium") t)
  (("/excursion:electron:/etc/shadow") nil)
  (("/excursion:electron:nope") nil)))

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
  (("foo") (concat default-directory "foo"))))

(provide 'file-attributes-test)
