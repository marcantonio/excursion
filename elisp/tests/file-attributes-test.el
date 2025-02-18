;;; -*- lexical-binding: t; -*-

(require 'excursion)

(excursion--gen-tests
 file-attributes
 ((("/excursion:electron:~/otium")
   '(t 2 1000 1000 (26472 42751 455515 384674) (26340 59827 900015 115737) (26340 59827 900015 115737) 4096 "drwxrwxr-x" nil 570122 (-2 . 64769)))
  (("/excursion:electron:~/.bashrc")
   '(nil 1 1000 1000 (26546 36361 295984 983444) (26472 46601 118689 298629) (26472 46601 118689 298629) 3838 "-rw-r--r--" nil 557949 (-2 . 64769)))
  (("/excursion:electron:~/foo")
   '("/home/mas/.bashrc" 1 1000 1000 (26546 46614 836355 686187) (26546 46476 452400 684356) (26546 46476 452400 684356) 17 "lrwxrwxrwx" nil 556071 (-2 . 64769)))
  (("/excursion:electron:~/nope") nil)))

(provide 'file-attributes-test)
