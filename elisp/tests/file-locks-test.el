;;; -*- lexical-binding: t; -*-

(require 'excursion)

(excursion--gen-tests
 excursion-make-lock-file-name
 ((("/excursion:electron:/home/mas/excursion/Cargo.toml")
   "/excursion:electron:/home/mas/excursion/.#Cargo.toml")))

(excursion--gen-tests
 excursion-make-lock-file-name
 ((("/excursion:electron:/home/mas/excursion/Cargo.toml") nil))
 :suffix "create-lockfiles-nil"
 :bindings ((create-lockfiles nil)))

(provide 'file-locks-test)
