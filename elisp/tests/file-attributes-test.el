;;; -*- lexical-binding: t; -*-

(require 'excursion)

(excursion--gen-tests
 file-attributes
 ((("/excursion:electron:~/otium")
   '(t 2 1000 1000 (26472 42751 455515 384674) (26340 59827 900015 115737) (26340 59827 900015 115737) 4096 "drwxrwxr-x" nil 570122 (-2 . 64769)))
  (("/excursion:electron:~/excursion/mm.el")
   '(nil 1 1000 1000 (26512 22319 918740 34103) (26512 22316 407613 277435) (26512 22316 407613 277435) 1455 "-rw-rw-r--" nil 1052017 (-2 . 64769)))
  (("/excursion:electron:~/excursion/foo")
   '("mm.el" 1 1000 1000 (26546 38841 537796 974182) (26536 9825 660214 424133) (26536 9825 660214 424133) 5 "lrwxrwxrwx" nil 1052858 (-2 . 64769)))))

(provide 'file-attributes-test)
