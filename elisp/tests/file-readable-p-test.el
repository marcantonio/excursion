;;; -*- lexical-binding: t; -*-

(require 'excursion)

(excursion--gen-tests
 file-readable-p
 ((("/excursion:electron:otium") t)
  (("/excursion:electron:/etc/shadow") nil)
  (("/excursion:electron:nope") nil)))

(provide 'file-readable-p-test)
