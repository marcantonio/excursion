;;; -*- lexical-binding: t; -*-

(require 'excursion)

;; TODO: add a test for root level home dirs when we have a user defined on the connection
(excursion--gen-tests
 abbreviate-file-name
 ((("/excursion:electron:/home/mas/otium")
   "/excursion:electron:~/otium")
  (("/excursion:electron:otium")
   "/excursion:electron:otium")))

(provide 'abbreviate-file-name-test)
