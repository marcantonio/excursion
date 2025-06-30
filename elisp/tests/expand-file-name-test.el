;;; -*- lexical-binding: t; -*-

(require 'excursion)

;; TODO: Consider moving these to a client container?
(excursion--gen-tests
 excursion-expand-file-name
 ((("foo") "/excursion:localhost#17001:/home/user1/test_root/foo")
  (("/foo") "/foo")
  (("~foo") "/excursion:localhost#17001:/home/user1/test_root/~foo")
  (("~user1") "/excursion:localhost#17001:/home/user1/test_root/~user1")
  (("foo" "bar") "/excursion:localhost#17001:/home/user1/test_root/bar/foo")

  (("~foo" "bar") "/excursion:localhost#17001:/home/user1/test_root/bar/~foo")
  (("foo" "~bar") "/excursion:localhost#17001:/home/user1/test_root/~bar/foo")
  (("~foo" "~bar") "/excursion:localhost#17001:/home/user1/test_root/~bar/~foo")
  (("/excursion:localhost#17001:foo") "/excursion:localhost#17001:/home/user1/foo")
  (("/excursion:localhost#17001:/foo") "/excursion:localhost#17001:/foo")

  (("/excursion:localhost#17001:~/foo") "/excursion:localhost#17001:/home/user1/foo")
  (("/excursion:localhost#17001:~foo") "/excursion:localhost#17001:/home/user1/~foo")
  (("/excursion:localhost#17001:~user1") "/excursion:localhost#17001:/home/user1")
  (("foo" "/excursion:localhost#17001:bar") "/excursion:localhost#17001:/home/user1/bar/foo")
  (("foo" "/excursion:localhost#17001:/bar") "/excursion:localhost#17001:/bar/foo")

  (("foo" "/excursion:localhost#17001:~/foo") "/excursion:localhost#17001:/home/user1/foo/foo")
  (("foo" "/excursion:localhost#17001:~foo") "/excursion:localhost#17001:/home/user1/~foo/foo")
  (("foo" "/excursion:localhost#17001:~user1") "/excursion:localhost#17001:/home/user1/foo")
  (("/foo" "/excursion:localhost#17001:bar") "/foo")
  (("/foo" "/excursion:localhost#17001:/bar") "/foo")

  (("/excursion:localhost#17001:foo" "/excursion:localhost#17001:bar") "/excursion:localhost#17001:/home/user1/foo")
  (("/excursion:localhost#17001:/foo" "/excursion:localhost#17001:bar") "/excursion:localhost#17001:/foo")
  (("/excursion:localhost#17001:/foo" "/excursion:localhost#17001:/bar") "/excursion:localhost#17001:/foo")
  (("" "/excursion:localhost#17001:bar") "/excursion:localhost#17001:/home/user1/bar")
  (("" "/excursion:localhost#17001:~user1") "/excursion:localhost#17001:/home/user1")

  (("/excursion:localhost#17001:") "/excursion:localhost#17001:/home/user1/")
  (("/excursion:localhost#17001:foo" "/excursion:localhost#17001:") "/excursion:localhost#17001:/home/user1/foo")
  (("/excursion:localhost#17001:" "/excursion:localhost#17001:") "/excursion:localhost#17001:/home/user1/")
  (("/excursion:localhost#17001:foo" "bar") "/excursion:localhost#17001:/home/user1/foo")
  (("src/../Cargo.toml") "/excursion:localhost#17001:/home/user1/test_root/Cargo.toml")

  (("../Cargo.toml") "/excursion:localhost#17001:/home/user1/Cargo.toml")
  (("../..") "/excursion:localhost#17001:/home")
  ((".." "..") "/excursion:localhost#17001:/home")
  (("/excursion:localhost#17001:~user1/..") "/excursion:localhost#17001:/home"))
 :suffix "remote-default-dir"
 :bindings ((default-directory "/excursion:localhost#17001:/home/user1/test_root")))

(let ((username (user-login-name))
      (home-dir (expand-file-name "~")))
  (excursion--gen-tests
   excursion-expand-file-name
   ((("foo") (concat home-dir "/foo"))
    (("/foo") "/foo")
    (("~/foo") (concat home-dir "/foo"))
    (("~not-a-user") (concat home-dir "/~not-a-user"))
    (((concat "~"  username)) home-dir)

    (("foo" "bar") (concat home-dir "/bar/foo"))
    (("/foo" "bar") "/foo")
    (("~not-a-user" "bar") (concat home-dir "/bar/~not-a-user"))
    (("foo" "/bar") "/bar/foo")
    (("foo" "~/bar") (concat home-dir "/bar/foo"))

    (("foo" (concat "~" username)) (concat home-dir "/foo"))
    (("foo" "~also-not-a-user") (concat home-dir "/~also-not-a-user/foo"))
    (("~not-a-user" "~also-not-a-user") (concat home-dir "/~also-not-a-user/~not-a-user"))
    (("" (concat "~" username)) home-dir)
    (("/excursion:localhost#17001:foo") "/excursion:localhost#17001:/home/user1/foo")

    (("/excursion:localhost#17001:/foo") "/excursion:localhost#17001:/foo")
    (("/excursion:localhost#17001:~/foo") "/excursion:localhost#17001:/home/user1/foo")
    (("/excursion:localhost#17001:~foo") "/excursion:localhost#17001:/home/user1/~foo")
    (("/excursion:localhost#17001:~user1") "/excursion:localhost#17001:/home/user1")
    (("foo" "/excursion:localhost#17001:bar") "/excursion:localhost#17001:/home/user1/bar/foo")

    (("foo" "/excursion:localhost#17001:/bar") "/excursion:localhost#17001:/bar/foo")
    (("foo" "/excursion:localhost#17001:~/foo") "/excursion:localhost#17001:/home/user1/foo/foo")
    (("foo" "/excursion:localhost#17001:~foo") "/excursion:localhost#17001:/home/user1/~foo/foo")
    (("foo" "/excursion:localhost#17001:~user1") "/excursion:localhost#17001:/home/user1/foo")
    (("/foo" "/excursion:localhost#17001:bar") "/foo")

    (("/foo" "/excursion:localhost#17001:/bar") "/foo")
    (("/excursion:localhost#17001:foo" "/excursion:localhost#17001:bar") "/excursion:localhost#17001:/home/user1/foo")
    (("/excursion:localhost#17001:/foo" "/excursion:localhost#17001:bar") "/excursion:localhost#17001:/foo")
    (("/excursion:localhost#17001:/foo" "/excursion:localhost#17001:/bar") "/excursion:localhost#17001:/foo")
    (("" "/excursion:localhost#17001:bar") "/excursion:localhost#17001:/home/user1/bar")

    (("" "/excursion:localhost#17001:~user1") "/excursion:localhost#17001:/home/user1")
    (("/excursion:localhost#17001:") "/excursion:localhost#17001:/home/user1/")
    (("/excursion:localhost#17001:foo" "/excursion:localhost#17001:") "/excursion:localhost#17001:/home/user1/foo")
    (("/excursion:localhost#17001:" "/excursion:localhost#17001:") "/excursion:localhost#17001:/home/user1/")
    (("/excursion:localhost#17001:foo" "bar") "/excursion:localhost#17001:/home/user1/foo")

    (("/excursion:localhost#17001:~user1/..") "/excursion:localhost#17001:/home"))
   :suffix "local-default-dir"
   :bindings ((default-directory home-dir))))

(provide 'expand-file-name-test)
