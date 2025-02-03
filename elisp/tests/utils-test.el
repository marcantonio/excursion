;;; -*- lexical-binding: t; -*-

(require 'excursion)

;; excursion--parse-filename
(excursion--gen-tests
 excursion--parse-filename
 ((("/foo") nil)
  (("/excursion:/foo") nil)
  (("/excursion:electron:/foo") '("excursion" "electron" "/foo"))
  (("/excursion::/foo") '("excursion" "" "/foo"))
  (("/excursion:electron:") '("excursion" "electron" ""))
  (("/excursion:electron:/" 'method) "excursion")
  (("/excursion:electron:/" 'host) "electron")
  (("/excursion:electron:/" 'file) "/")
  (("/excursion:electron:/" 'd) nil)))

;; excursion--file-p
(excursion--gen-tests
 excursion--file-p
 ((("/foo") nil)
  (("/excursion:electron:/foo") t)))

(provide 'utils-test)
