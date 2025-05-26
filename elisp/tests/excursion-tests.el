;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

;; TODO: It's hard to see why a test failed because of the `apply'
(cl-defmacro excursion--gen-tests (fn cases &key suffix setup bindings eq-pred)
  "Macro to generate tests for FN against every case in CASES.

Each test case in should be a two-element list of the form:
((args...) expected)

Named args:
:suffix
  Text to append to FN when the test name is derived. Useful to
  delineate different environments for the same function.

:setup
  Code to run before each test case is executed.

:bindings
  Inserted into a `let' wrapped around the test code.

:eq-pred
  Function to use for equality test with `should'.
"
  (let* ((s (symbol-name fn))
         (eq-fn (or eq-pred (symbol-function 'equal)))
         (test-name (if suffix (format "%s-%s" s suffix) s))
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
                          (should (apply ,eq-fn (list (,fn ,@args) ,expected)))))))))
    `(progn ,@tests)))

(setq load-prefer-newer t)

(require 'abbreviate-file-name-test)
(require 'connection-test)
(require 'directory-files-test)
(require 'expand-file-name-test)
(require 'file-attributes-test)
(require 'file-equal-p-test)
(require 'file-in-directory-p-test)
(require 'file-locks-test)
(require 'file-remote-p-test)
(require 'frame-test)
(require 'utils-test)

(provide 'excursion-tests)
