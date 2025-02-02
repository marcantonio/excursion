;;; -*- lexical-binding: t; -*-

(require 'excursion)

(defmacro gen-test2 (fn-name args result &optional iteration)
  "Create a test that takes 2 arguments."
  (let ((arg1 (car args))
        (arg2 (cadr args))
        (test-name (intern (format
                            "%s-%s-test"
                            (symbol-name fn-name)
                            iteration))))
    `(ert-deftest ,test-name ()
       (should
        (equal (,fn-name ,arg1 ,arg2) ,result)))))

(let* ((cases
        '((("/foo") nil)
          (("/excursion:/foo") nil)
          (("/excursion:electron:/foo") '("excursion" "electron" "/foo"))
          (("/excursion::/foo") '("excursion" "" "/foo"))
          (("/excursion:electron:") '("excursion" "electron" ""))
          (("/excursion:electron:/" 'method) "excursion")
          (("/excursion:electron:/" 'host) "electron")
          (("/excursion:electron:/" 'file) "/")
          (("/excursion:electron:/" 'd) nil)))
       (i 1))
  (dolist (case cases)
    (let ((args (car case))
          (result (cadr case)))
      (eval `(gen-test2 excursion--parse-filename ,args ,result ,i)))
    (setq i (1+ i))))

(ert-deftest excursion--file-p-test ()
  (let ((cases
         '((("/foo") nil)
           (("/excursion:electron:/foo") t))))
    (dolist (case cases)
      (let* ((args (car case))
             (file (car args))
             (expected (cadr case)))
        (should
         (equal (excursion--file-p file) expected))))))

(provide 'utils-test)
