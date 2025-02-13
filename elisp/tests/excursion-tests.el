;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defmacro excursion--gen-tests (fn-name cases &key suffix setup bindings)
  "Macro to generate tests for FN-NAME against every case in CASES.

Each test case in should be a two-element list of the form:
  ((args...) expected)

Named args:

:suffix
  Text to append to FN-NAME when the test name is derived. Useful to delineate different environments for the same function.

:setup
  Code to run before each test case is executed.

:bindings
  Inserted into a `let' wrapped around the test code.
"
  (let* ((sym (symbol-name fn-name))
         (test-name (if suffix (format "%s-%s" sym suffix) sym))
         (tests
          (cl-loop for case in cases
                   for i from 1
                   for args = (car case)
                   for expected = (cadr case)
                   collect
                   (let ((full-test-name
                          (intern (format "%s-%s-test" test-name i))))
                     `(ert-deftest ,full-test-name ()
                        ,@setup
                        (let ,bindings
                          (should (equal (,fn-name ,@args) ,expected))))))))
    `(progn ,@tests)))

(setq load-prefer-newer t)

(require 'expand-file-name-test)
(require 'file-remote-p-test)
(require 'frame-test)
(require 'utils-test)

(provide 'excursion-tests)
