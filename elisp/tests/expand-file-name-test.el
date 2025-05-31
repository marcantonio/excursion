;;; -*- lexical-binding: t; -*-

(require 'excursion)

(excursion--gen-tests
 expand-file-name
 ((("foo") "/excursion:electron:/home/mas/excursion/foo")
  (("/foo") "/foo")
  (("~/foo") "/home/mas/foo")
  (("~foo") "/excursion:electron:/home/mas/excursion/~foo")
  (("~mas") "/home/mas")

  (("foo" "bar") "/excursion:electron:/home/mas/excursion/bar/foo")
  (("/foo" "bar") "/foo")
  (("~foo" "bar") "/excursion:electron:/home/mas/excursion/bar/~foo")
  (("foo" "/bar") "/bar/foo")
  (("foo" "~/bar") "/home/mas/bar/foo")

  (("foo" "~mas") "/home/mas/foo")
  (("foo" "~bar") "/excursion:electron:/home/mas/excursion/~bar/foo")
  (("~foo" "~bar") "/excursion:electron:/home/mas/excursion/~bar/~foo")
  (("" "~mas") "/home/mas")
  (("/excursion:electron:foo") "/excursion:electron:/home/mas/foo")

  (("/excursion:electron:/foo") "/excursion:electron:/foo")
  (("/excursion:electron:~/foo") "/excursion:electron:/home/mas/foo")
  (("/excursion:electron:~foo") "/excursion:electron:/home/mas/~foo")
  (("/excursion:electron:~mas") "/excursion:electron:/home/mas")
  (("foo" "/excursion:electron:bar") "/excursion:electron:/home/mas/bar/foo")

  (("foo" "/excursion:electron:/bar") "/excursion:electron:/bar/foo")
  (("foo" "/excursion:electron:~/foo") "/excursion:electron:/home/mas/foo/foo")
  (("foo" "/excursion:electron:~foo") "/excursion:electron:/home/mas/~foo/foo")
  (("foo" "/excursion:electron:~mas") "/excursion:electron:/home/mas/foo")
  (("/foo" "/excursion:electron:bar") "/foo")

  (("/foo" "/excursion:electron:/bar") "/foo")
  (("/excursion:electron:foo" "/excursion:electron:bar") "/excursion:electron:/home/mas/foo")
  (("/excursion:electron:/foo" "/excursion:electron:bar") "/excursion:electron:/foo")
  (("/excursion:electron:/foo" "/excursion:electron:/bar") "/excursion:electron:/foo")
  (("" "/excursion:electron:bar") "/excursion:electron:/home/mas/bar")

  (("" "/excursion:electron:~mas") "/excursion:electron:/home/mas")
  (("/excursion:electron:") "/excursion:electron:/home/mas/")
  (("/excursion:electron:foo" "/excursion:electron:") "/excursion:electron:/home/mas/foo")
  (("/excursion:electron:" "/excursion:electron:") "/excursion:electron:/home/mas/")
  (("/excursion:electron:foo" "bar") "/excursion:electron:/home/mas/foo")

  (("src/../Cargo.toml") "/excursion:electron:/home/mas/excursion/Cargo.toml")
  (("../Cargo.toml") "/excursion:electron:/home/mas/Cargo.toml")
  (("../..") "/excursion:electron:/home")
  ((".." "..") "/excursion:electron:/home")
  (("/excursion:electron:~mas/..") "/excursion:electron:/home"))
 :suffix "remote-default-dir"
 :bindings ((default-directory "/excursion:electron:/home/mas/excursion/")))

(excursion--gen-tests
 expand-file-name
 ((("foo") "/home/mas/Code/excursion/foo")
  (("/foo") "/foo")
  (("~/foo") "/home/mas/foo")
  (("~foo") "/home/mas/Code/excursion/~foo")
  (("~mas") "/home/mas")

  (("foo" "bar") "/home/mas/Code/excursion/bar/foo")
  (("/foo" "bar") "/foo")
  (("~foo" "bar") "/home/mas/Code/excursion/bar/~foo")
  (("foo" "/bar") "/bar/foo")
  (("foo" "~/bar") "/home/mas/bar/foo")

  (("foo" "~mas") "/home/mas/foo")
  (("foo" "~bar") "/home/mas/Code/excursion/~bar/foo")
  (("~foo" "~bar") "/home/mas/Code/excursion/~bar/~foo")
  (("" "~mas") "/home/mas")
  (("/excursion:electron:foo") "/excursion:electron:/home/mas/foo")

  (("/excursion:electron:/foo") "/excursion:electron:/foo")
  (("/excursion:electron:~/foo") "/excursion:electron:/home/mas/foo")
  (("/excursion:electron:~foo") "/excursion:electron:/home/mas/~foo")
  (("/excursion:electron:~mas") "/excursion:electron:/home/mas")
  (("foo" "/excursion:electron:bar") "/excursion:electron:/home/mas/bar/foo")

  (("foo" "/excursion:electron:/bar") "/excursion:electron:/bar/foo")
  (("foo" "/excursion:electron:~/foo") "/excursion:electron:/home/mas/foo/foo")
  (("foo" "/excursion:electron:~foo") "/excursion:electron:/home/mas/~foo/foo")
  (("foo" "/excursion:electron:~mas") "/excursion:electron:/home/mas/foo")
  (("/foo" "/excursion:electron:bar") "/foo")

  (("/foo" "/excursion:electron:/bar") "/foo")
  (("/excursion:electron:foo" "/excursion:electron:bar") "/excursion:electron:/home/mas/foo")
  (("/excursion:electron:/foo" "/excursion:electron:bar") "/excursion:electron:/foo")
  (("/excursion:electron:/foo" "/excursion:electron:/bar") "/excursion:electron:/foo")
  (("" "/excursion:electron:bar") "/excursion:electron:/home/mas/bar")

  (("" "/excursion:electron:~mas") "/excursion:electron:/home/mas")
  (("/excursion:electron:") "/excursion:electron:/home/mas/")
  (("/excursion:electron:foo" "/excursion:electron:") "/excursion:electron:/home/mas/foo")
  (("/excursion:electron:" "/excursion:electron:") "/excursion:electron:/home/mas/")
  (("/excursion:electron:foo" "bar") "/excursion:electron:/home/mas/foo")

  (("src/../Cargo.toml") "/home/mas/Code/excursion/Cargo.toml")
  (("../Cargo.toml") "/home/mas/Code/Cargo.toml")
  (("../../..") "/home")
  ((".." "..") "/home/mas")
  (("/excursion:electron:~mas/..") "/excursion:electron:/home"))
 :suffix "local-default-dir"
 :bindings ((default-directory "/home/mas/Code/excursion/")))

(provide 'expand-file-name-test)
