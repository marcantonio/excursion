;;; -*- lexical-binding: t; -*-

(require 'excursion)

(excursion--gen-tests
 file-equal-p
 ((("/excursion:electron:/home/mas/excursion/"
    "/excursion:electron:/home/mas/excursion") t)
  (("/excursion:electron:/home/mas/excursion"
    "/excursion:electron:~/excursion") t)
  (("/excursion:electron:~mas"
    "/home/mas") nil)
  (("/excursion:electron:/home/mas/excursion"
    "/excursion:neutron:~/excursion") nil)))

(provide 'file-equal-p-test)
