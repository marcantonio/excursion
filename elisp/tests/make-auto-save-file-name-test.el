;;; -*- lexical-binding: t; -*-

(require 'excursion)

(ert-deftest excursion-make-auto-save-file-name-test ()
  (with-temp-buffer
    (set-visited-file-name "/excursion:electron:foo")
    (should (equal (make-auto-save-file-name) "mm"))))

(with-temp-buffer
  (set-visited-file-name "/excursion:electron:foo")
  (make-auto-save-file-name))
