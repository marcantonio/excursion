;;; -*- lexical-binding: t; -*-

(require 'excursion)

(excursion--gen-tests
 file-in-directory-p
 ((("/excursion:electron:/home/mas/excursion/Cargo.toml"
    "/excursion:electron:/home/mas/excursion") t)
  (("/excursion:electron:/home/mas/excursion/Cargo.tom"
    "/excursion:electron:/home/mas/excursion") t) ; file doesn't need to exists...
  (("/excursion:electron:/home/mas/excursio/Cargo.toml"
    "/excursion:electron:/home/mas/excursio") nil) ; ...but the directory does
  (("/excursion:electron:/dev" "/") nil)  ; mixing local and remote fails
  (("/excursion:electron:/home/mas/excursion/Cargo.toml"
    "/excursion:neutron:/home/mas/excursion") nil)))

(provide 'file-in-directory-p-test)
