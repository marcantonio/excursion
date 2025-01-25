;;; -*- lexical-binding: t; -*-

(require 'excursion)

(ert-deftest expand-file-name-test ()
  (excursion-terminate)
  (let ((cases
         '((("foo" "bar") "/home/mas/excursion/bar/foo")
           (("/foo" "bar") "/foo")
           (("~/foo" "~ms") "/home/mas/foo")
           (("~/foo" "bar") "/home/mas/foo")
           (("~foo" "bar") "/home/mas/excursion/bar/~foo")
           (("~baz" "bar") "/home/mas/excursion/bar/~baz")
           (("~/foo" "bar") "/home/mas/foo")
           (("foo" "~bar") "/home/mas/excursion/~bar/foo")
           (("/foo" "~bar") "/foo")
           (("~/foo" "~bar") "/home/mas/foo")
           (("~foo" "~bar") "/home/mas/excursion/~bar/~foo")
           (("~baz" "~/bar") "/home/mas/bar/~baz")
           (("~mas" "~bar") "/home/mas")
           (("~/foo" "~bar") "/home/mas/foo")
           (("foo" "~/bar") "/home/mas/bar/foo")
           (("/foo" "~/bar") "/foo")
           (("~/foo" "~/bar") "/home/mas/foo")
           (("~foo" "~/bar") "/home/mas/bar/~foo")
           (("~baz" "~/bar") "/home/mas/bar/~baz")
           (("~mas" "~/bar") "/home/mas")
           (("~/foo" "~/bar") "/home/mas/foo")
           (("foo" "/bar") "/bar/foo")
           (("/foo" "/bar") "/foo")
           (("~/foo" "/bar") "/home/mas/foo")
           (("~foo" "/bar") "/bar/~foo")
           (("~mas" "/bar") "/home/mas")
           (("~baz" "/bar") "/bar/~baz")
           (("~/foo" "~mas") "/home/mas/foo")
           (("foo" "~mas") "/home/mas/foo")
           (("/foo" "~mas") "/foo")
           (("~/foo" "~mas") "/home/mas/foo")
           (("~foo" "~mas") "/home/mas/~foo")
           (("~mas" "~mas") "/home/mas")
           (("~baz" "~mas") "/home/mas/~baz")
           (("~/foo" "~mas") "/home/mas/foo")
           (("~/foo" "~mas") "/home/mas/foo")
           (("foo" "~ms") "/home/mas/excursion/~ms/foo")
           (("/foo" "~ms") "/foo")
           (("~/foo" "~ms") "/home/mas/foo")
           (("~foo" "~ms") "/home/mas/excursion/~ms/~foo")
           (("~mas" "~ms") "/home/mas")
           (("~baz" "~ms") "/home/mas/excursion/~ms/~baz")
           (("foo" "") "/home/mas/excursion/foo")
           (("foo" nil) "/home/mas/excursion/foo")
           (("~/foo" nil) "/home/mas/foo")
           (("/foo" nil) "/foo")
           (("~foo" nil) "/home/mas/excursion/~foo")
           (("~mas" nil) "/home/mas"))))
    (dolist (case cases)
      (let* ((args (car case))
             (file (car args))
             (dir (cadr args))
             (expected (cadr case))
             (default-directory "/excursion:/home/mas/excursion/"))
        (should
         (equal
          (excursion-expand-file-name file dir)
          (concat "/excursion:" expected)))))))

(provide 'expand-file-name-test)
