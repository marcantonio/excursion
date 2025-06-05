;;; -*- lexical-binding: t; -*-

(require 'excursion)

;; Test with buffer not visiting a file
(excursion--gen-tests
 verify-visited-file-modtime
 ((() t))
 :suffix "no-visited-file"
 :fixture-fn (lambda (fn &optional args)
               (with-temp-buffer
                 (funcall fn args))))

;; Test with no recorded modification time
(excursion--gen-tests
 verify-visited-file-modtime
 ((() t))
 :suffix "zero-modtime"
  :fixture-fn (lambda (fn &optional args)
               (with-temp-buffer
                 (setq buffer-file-name "/excursion:electron:never-existed-file")
                 (set-visited-file-modtime 0)
                 (funcall fn args))))

;; Test with file that never existed
(excursion--gen-tests
 verify-visited-file-modtime
 ((() t))
 :suffix "never-existed"
 :fixture-fn (lambda (fn &optional args)
               (with-temp-buffer
                 (setq buffer-file-name "/excursion:electron:never-existed-file")
                 (funcall fn args))))

;; Test with file that has been deleted
(excursion--gen-tests
 verify-visited-file-modtime
 ((() nil))
 :suffix "deleted-file"
 :fixture-fn (lambda (fn &optional args)
               (with-temp-buffer
                 (setq buffer-file-name "/excursion:electron:~/deleted-file")
                 (set-visited-file-modtime '(26340 59827))
                 (funcall fn args))))

;; Test with existing file and matching modtime
(excursion--gen-tests
 verify-visited-file-modtime
 ((() t))
 :suffix "matching-modtime"
 :fixture-fn (lambda (fn &optional args)
               (with-temp-buffer
                 (setq buffer-file-name "/excursion:electron:~/.bashrc")
                 ;; Set visited time to match actual file modtime
                 (let ((actual-modtime (file-attribute-modification-time
                                        (file-attributes "/excursion:electron:~/.bashrc"))))
                   (set-visited-file-modtime actual-modtime))
                 (funcall fn args))))

;; Test with existing file and mismatched modtime
(excursion--gen-tests
 verify-visited-file-modtime
 ((() nil))
 :suffix "mismatched-modtime"
 :setup ((with-temp-buffer
           (setq buffer-file-name "/excursion:electron:~/.bashrc")
           (set-visited-file-modtime '(1 0)))))

;; Test with explicit buffer argument
(excursion--gen-tests
 verify-visited-file-modtime
 (((test-buffer) t))
 :suffix "explicit-buffer"
 :setup ((let ((test-buffer (generate-new-buffer "test")))
           (with-current-buffer test-buffer
             (setq buffer-file-name "/excursion:electron:~/.bashrc")
             (let ((actual-modtime (file-attribute-modification-time
                                   (file-attributes "/excursion:electron:~/.bashrc"))))
               (set-visited-file-modtime actual-modtime))))))

;; Test dired-like buffer (not visiting file but with non-zero modtime)
(excursion--gen-tests
 verify-visited-file-modtime
 ((() t))
 :suffix "dired-like"
 :setup ((with-temp-buffer
           (setq buffer-file-name nil)
           (set-visited-file-modtime '(26340 59827)))))

(provide 'verify-visited-file-modtime-test)
