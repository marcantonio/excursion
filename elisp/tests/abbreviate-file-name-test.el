;;; -*- lexical-binding: t; -*-

(require 'excursion)

;; TODO: add a test for root level home dirs when we have a user defined on the connection
(excursion--gen-tests
 excursion-abbreviate-file-name
 ((("/excursion:localhost#17001:/home/user1/otium") "/excursion:localhost#17001:~/otium")
  (("/excursion:localhost#17001:otium") "/excursion:localhost#17001:otium")))

(provide 'abbreviate-file-name-test)
