;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

;; TODO: Remove :setup and :bindings
(cl-defmacro excursion--gen-tests (fn cases &key suffix setup bindings eq-pred fixture-fn)
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

:fixture-fn
  Function to wrap `fn' setup and teardown.
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
                          (let ((actual ,(if (and fixture-fn (functionp fixture-fn))
                                             `(,fixture-fn #',fn ,@args)
                                           `(,fn ,@args)))
                                (expected-value ,expected))
                            (ert-info ((format "Expected: %S\nActual: %S" expected-value actual))
                              (should (apply ,eq-fn (list actual expected-value)))))))))))
    `(progn ,@tests)))

(defmacro excursion--mock-calls (bindings &rest body)
  "Override functions while executing BODY. BINDINGS should be a
list of cons cells in the form of (FUNCTION RESULT). For each
entry, FUNCTION is a symbol naming a function to mock, and RESULT
is the value that FUNCTION will return whenever it is called."
  (let ((pairs (mapcar (lambda (binding)
                         `((symbol-function ,(car binding))
                           (lambda (&rest _) ,(cadr binding))))
                       bindings)))
    `(cl-letf (,@pairs) ,@body)))

(setq load-prefer-newer t)

(require 'abbreviate-file-name-test)
(require 'connection-test)
(require 'directory-files-test)
(require 'expand-file-name-test)
(require 'file-attributes-test)
(require 'file-equal-p-test)
(require 'file-in-directory-p-test)
(require 'file-locks-test)
(require 'file-modtime-test)
(require 'file-remote-p-test)
(require 'frame-test)
(require 'make-auto-save-file-name-test)
(require 'make-symbolic-link-test)
(require 'utils-test)

(provide 'excursion-tests)
