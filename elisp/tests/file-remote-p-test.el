;;; -*- lexical-binding: t; -*-

(require 'excursion)

(ert-deftest file-remote-p-connected-test ()
             (excursion--remote-connection)
             (let ((cases
                    `((("/excursion:electron:main.rs") "/excursion:electron:")
                      (("/excursion:electron:main.rs" ,'method) "excursion")
                      (("/excursion:electron:main.rs" ,'host) "electron")
                      (("/excursion:electron:main.rs" ,'method t) "excursion")
                      (("/excursion:electron:main.rs" ,'host t) "electron")
                      (("/excursion:electron:main.rs" ,'foo) "/excursion:electron:")
                      (("/excursion:electron:main.rs" ,'foo t) "/excursion:electron:"))))
               (dolist (case cases)
                 (let* ((args (car case))
                        (filename (car args))
                        (identification (cadr args))
                        (connected (caddr args))
                        (expected (cadr case)))
                   (should
                    (equal
                     (file-remote-p filename identification connected) expected))))))

(ert-deftest file-remote-p-not-connected-test ()
             (excursion-terminate)
             (let ((cases
                    `((("/excursion:electron:main.rs") "/excursion:electron:")
                      (("/excursion:electron:main.rs" ,'method) "excursion")
                      (("/excursion:electron:main.rs" ,'host) "electron")
                      (("/excursion:electron:main.rs" ,'method t) nil)
                      (("/excursion:electron:main.rs" ,'host t) nil)
                      (("/excursion:electron:main.rs" ,'foo) "/excursion:electron:")
                      (("/excursion:electron:main.rs" ,'foo t) nil))))
               (dolist (case cases)
                 (let* ((args (car case))
                        (filename (car args))
                        (identification (cadr args))
                        (connected (caddr args))
                        (expected (cadr case)))
                   (excursion-terminate)
                   (should
                    (equal
                     (file-remote-p filename identification connected) expected))))))

(provide 'file-remote-p-test)
