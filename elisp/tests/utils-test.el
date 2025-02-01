;;; -*- lexical-binding: t; -*-

(require 'excursion)

(ert-deftest excursion--parse-filename-test ()
  (let ((cases
         `((("/foo") nil)
           (("/excursion:/foo") nil)
           (("/excursion:electron:/foo") ,'("excursion" "electron" "/foo"))
           (("/excursion::/foo") ,'("excursion" "" "/foo"))
           (("/excursion:electron:") ,'("excursion" "electron" ""))
           (("/excursion:electron:/" ,'method) "excursion")
           (("/excursion:electron:/" ,'host) "electron")
           (("/excursion:electron:/" ,'file) "/")
           (("/excursion:electron:/" ,'d) nil))))
    (dolist (case cases)
      (let* ((args (car case))
             (file (car args))
             (part (cadr args))
             (expected (cadr case)))
        (should
         (equal (excursion--parse-filename file part) expected))))))

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
