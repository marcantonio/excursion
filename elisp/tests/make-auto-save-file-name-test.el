;;; -*- lexical-binding: t; -*-

(require 'excursion)

(ert-deftest make-auto-save-file-name-test ()
  (with-temp-buffer
    (set-visited-file-name "/excursion:localhost#17001:foo")
    (set-buffer-modified-p nil)
    (should (equal (excursion-make-auto-save-file-name)
                   (concat (expand-file-name excursion-auto-save-directory)
                           "#!excursion:localhost#17001:!home!user1!foo#")))))

(ert-deftest make-auto-save-file-name-no-trailing-/-test ()
  (with-temp-buffer
    (set-visited-file-name "/excursion:localhost#17001:foo")
    (set-buffer-modified-p nil)
    (let* ((excursion-auto-save-directory (concat excursion-auto-save-directory "/test"))
           (success? (should (equal (excursion-make-auto-save-file-name)
                                    (concat (expand-file-name excursion-auto-save-directory)
                                            "/#!excursion:localhost#17001:!home!user1!foo#")))))
      (delete-directory excursion-auto-save-directory)
      success?)))

;; I don't think this could ever happen
(ert-deftest make-auto-save-file-name-no-file-test ()
  (with-temp-buffer
    (set-buffer-modified-p nil)
    (should (equal (excursion-make-auto-save-file-name) nil))))

(provide 'make-auto-save-file-name-test)
