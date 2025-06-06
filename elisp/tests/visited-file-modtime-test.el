;;; -*- lexical-binding: t; -*-

(require 'excursion)

;;; verify-visited-file-modtime

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
 :fixture-fn (lambda (fn &optional args)
               (with-temp-buffer
                 (setq buffer-file-name "/excursion:electron:~/.bashrc")
                 (set-visited-file-modtime '(1 0))
                 (funcall fn args))))

;; Test with explicit buffer argument
(excursion--gen-tests
 verify-visited-file-modtime
 ((() t))
 :suffix "explicit-buffer"
 :fixture-fn (lambda (fn &optional args)
               (with-temp-buffer
                 (setq buffer-file-name "/excursion:electron:~/.bashrc")
                 (let ((actual-modtime (file-attribute-modification-time
                                        (file-attributes "/excursion:electron:~/.bashrc"))))
                   (set-visited-file-modtime actual-modtime))
                 (funcall fn (current-buffer)))))

;; Test dired-like buffer (not visiting file but with non-zero modtime)
(excursion--gen-tests
 verify-visited-file-modtime
 ((() t))
 :suffix "dired-like"
 :fixture-fn (lambda (fn &optional args)
               (with-temp-buffer
                 (setq buffer-file-name nil)
                 (set-visited-file-modtime '(26340 59827))
                 (funcall fn args))))

;;; set-visited-file-modtime

;; Test setting modtime with explicit time argument
(excursion--gen-tests
 set-visited-file-modtime
 ((('(26340 59827)) nil))
 :suffix "explicit-time"
 :fixture-fn (lambda (fn &optional args)
               (with-temp-buffer
                 (setq buffer-file-name "/excursion:electron:~/.bashrc")
                 (set-visited-file-modtime '(1 0))
                 (funcall fn args)
                 ;; Verify the modtime was set to the specified value
                 (equal (visited-file-modtime) '(26340 59827)))))

;; Test setting modtime with no argument
(excursion--gen-tests
 set-visited-file-modtime
 ((() t))
 :suffix "no-arg-existing-file"
 :fixture-fn (lambda (fn &optional args)
               (with-temp-buffer
                 (setq buffer-file-name "/excursion:electron:~/.bashrc")
                 (set-visited-file-modtime '(1 0))
                 ;; Should update from real file
                 (funcall fn args)
                 ;; Verify the modtime was updated to match file
                 (let ((actual-modtime (file-attribute-modification-time
                                        (file-attributes "/excursion:electron:~/.bashrc"))))
                   (equal actual-modtime (visited-file-modtime))))))

;; Test with buffer not visiting a file
(excursion--gen-tests
 set-visited-file-modtime
 ((() nil))
 :suffix "no-visited-file"
 :fixture-fn (lambda (fn &optional args)
               (with-temp-buffer
                 (condition-case err
                     (progn
                       (funcall fn args)
                       t) ; t if it succeeds
                   (error nil))))) ; nil on error

;; Test with non-existent file
(excursion--gen-tests
 set-visited-file-modtime
 ((() nil))
 :suffix "non-existent-file"
 :fixture-fn (lambda (fn &optional args)
               (with-temp-buffer
                 (setq buffer-file-name "/excursion:electron:never-existed-file")
                 (funcall fn args))))

(provide 'visited-file-modtime-test)
