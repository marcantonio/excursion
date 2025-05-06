(with-temp-buffer
  (set-visited-file-name "/excursion:electron:~/otium")
  (verify-visited-file-modtime))

(require 'ert)
(require 'dired)

(ert-deftest my-verify-visited-file-modtime/non-file-buffer ()
  "Test with buffer not visiting a file"
  (with-temp-buffer
    (should (my-verify-visited-file-modtime))))

(ert-deftest my-verify-visited-file-modtime/new-file-buffer ()
  "Test with new buffer that has never been saved"
  (with-temp-buffer
    (set-visited-file-name "unsaved-file.txt")
    (should (my-verify-visited-file-modtime))))

(ert-deftest my-verify-visited-file-modtime/matching-times ()
  "Test with file that hasn't changed"
  (with-temp-file "testfile.txt"
    (insert "original content"))
  (with-current-buffer (find-file-noselect "testfile.txt")
    (should (my-verify-visited-file-modtime))
    (kill-buffer)))

(ert-deftest my-verify-visited-file-modtime/modified-externally ()
  "Test with file modified outside Emacs"
  (with-temp-file "testfile.txt"
    (insert "original content"))
  (with-current-buffer (find-file-noselect "testfile.txt")
    (save-buffer)  ; Record initial modtime
    (with-temp-file "testfile.txt"
      (insert "modified content"))
    (should-not (my-verify-visited-file-modtime))
    (kill-buffer)))

(ert-deftest my-verify-visited-file-modtime/file-deleted ()
  "Test with file that was deleted after visiting"
  (let ((filename (make-temp-file "deleteme")))
    (with-current-buffer (find-file-noselect filename)
      (delete-file filename)
      (should-not (my-verify-visited-file-modtime))
      (kill-buffer))))

(ert-deftest my-verify-visited-file-modtime/dired-buffer ()
  "Test with dired buffer (should always return t)"
  (let ((test-dir (make-temp-file "testdir" t)))
    (unwind-protect
        (with-current-buffer (dired-noselect test-dir)
          (should (my-verify-visited-file-modtime)))
      (delete-directory test-dir t))))

(ert-deftest my-verify-visited-file-modtime/nonexistent-file ()
  "Test with buffer visiting never-existent file"
  (with-temp-buffer
    (set-visited-file-name "this-file-does-not-exist.txt")
    (should (my-verify-visited-file-modtime))))
