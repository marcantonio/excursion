;;; -*- lexical-binding: t; -*-

(require 'excursion)

(excursion--gen-tests
 file-remote-p
 ((("/excursion:electron:main.rs") "/excursion:electron:")
  (("/excursion:electron:main.rs" 'method) "excursion")
  (("/excursion:electron:main.rs" 'host) "electron")
  (("/excursion:electron:main.rs" 'method t) "excursion")
  (("/excursion:electron:main.rs" 'host t) "electron")
  (("/excursion:electron:main.rs" 'foo) "/excursion:electron:")
  (("/excursion:electron:main.rs" 'foo t) "/excursion:electron:"))
 :suffix "connected"
 :setup ((excursion--remote-connection)))

(excursion--gen-tests
 file-remote-p
 ((("/excursion:electron:main.rs") "/excursion:electron:")
  (("/excursion:electron:main.rs" 'method) "excursion")
  (("/excursion:electron:main.rs" 'host) "electron")
  (("/excursion:electron:main.rs" 'method t) nil)
  (("/excursion:electron:main.rs" 'host t) nil)
  (("/excursion:electron:main.rs" 'foo) "/excursion:electron:")
  (("/excursion:electron:main.rs" 'foo t) nil))
 :suffix "not-connected"
 :setup ((excursion-terminate)))

(provide 'file-remote-p-test)
