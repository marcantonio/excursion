;;; -*- lexical-binding: t; -*-

(defvar excursion-host "localhost")

(defvar excursion-port 7001)

(defvar excursion--process-name "excursion")

(defvar excursion--frame-types '((?^ . Data)
                                 (?! . Err)
                                 (?\( . Open)
                                 (?&  . Save)))

(defun excursion--get-frame-type (char)
  (cdr (assoc char excursion--frame-types)))

(defun excursion-terminate ()
  (interactive)
  (let ((pattern "^excursion")
        (processes (process-list)))
    (dolist (process processes)
      (when (string-match pattern (process-name process))
        (delete-process process)))))

(defun excursion-open-remote-file (filename)
  (interactive "GFilename: ")
  (let ((request (format "(%s|%s" (length filename) filename))
        (process (excursion--remote-connection)))
    (process-put process 'filename filename)
    (process-send-string excursion--process-name request)))

(defun excursion-open-remote-directory (directory)
  (interactive "DDirectory: ")
  (let ((request (format "~%s|%s" (length directory) directory))
        (process (excursion--remote-connection)))
    (process-put process 'directory directory)
    (process-send-string excursion--process-name request)))

(defun excursion--split-at-first (delimiter string)
  (let ((pos (string-match (regexp-quote delimiter) string)))
    (when pos
      (list (substring string 0 pos)
            (substring string (+ pos (length delimiter)))))))

;; Split this into a valid-frame-p and something simpler
(defun excursion--destructure-frame (frame)
  (let* ((frame-halves (excursion--split-at-first "|" frame))
         (preamble (car frame-halves))
         (data (cadr frame-halves)))
    (when (and preamble (not (string-empty-p preamble)))
      (let ((type (excursion--get-frame-type (aref preamble 0)))
            (len (string-to-number (substring preamble 1))))
        (when (and type len data)
          `((type . ,type)
            (len . ,len)
            (data . ,data)))))))

;; (defun excursion--filter (process contents)
;;   (when (buffer-live-p (process-buffer process))
;;     (let* ((frame (excursion--destructure-frame contents))
;;            (filename (process-get process 'filename))
;;            (buffer (generate-new-buffer (file-name-nondirectory filename)))
;;            (type (cdr (assoc 'type frame)))
;;            (data (cdr (assoc 'data frame))))
;;       (cond ((eq type 'Data)
;;              (excursion--setup-buffer buffer filename data)
;;              (switch-to-buffer buffer))
;;             (t (message "received %s: %s " type data))))))

(defun excursion--filter (process contents)
  (when (buffer-live-p (process-buffer process))
    (let* ((frame (excursion--destructure-frame contents))
           (directory (process-get process 'directory))
           (buffer (generate-new-buffer (file-name-nondirectory directory)))
           (type (cdr (assoc 'type frame)))
           (data (cdr (assoc 'data frame))))
      (cond ((eq type 'Data)
             (excursion--setup-buffer buffer directory data)
             (switch-to-buffer buffer))
            (t (message "received %s: %s " type data))))))

(defun excursion--setup-buffer (buffer filename contents)
  (with-current-buffer buffer
    (setq buffer-file-name filename)
    (setq default-directory (concat "/excursion:" (file-name-directory filename)))
    (set-auto-mode)
    (insert contents)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun excursion--new-connection (filter-fn)
  (condition-case err
      (let ((process (open-network-stream excursion--process-name "*excursion*" excursion-host excursion-port)))
        (set-process-filter process 'filter-fn)
        process)
    (file-error
     (error "failed to open remote connection: %s" (error-message-string err)))))

(defun excursion--remote-connection ()
  (if-let ((process (get-process excursion--process-name)))
      process
    (condition-case err
        (let ((process (open-network-stream excursion--process-name "*excursion*" excursion-host excursion-port)))
          (set-process-filter process 'excursion--filter)
          process)
      (file-error
       (error "failed to open remote connection: %s" (error-message-string err))))))

(defun excursion-file-handler (operation &rest args)
  ;(message ">>>%s" operation)
  (cond
   (t (let ((inhibit-file-name-handlers
             (cons 'excursion-file-handler
                   (and (eq inhibit-file-name-operation operation)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation operation))
        (apply operation args)))))

(add-to-list 'file-name-handler-alist '("\\`/excursion:" . excursion-file-handler))
;; (file-exists-p "/excursion:foo")
;; (expand-file-name "/excursion:foo")
;; (directory-files "/excursion:/home/mas")

(progn
  (excursion-terminate)
  ;;(excursion-open-remote-file "./scripts/nc_test.bash"))
  (excursion-open-remote-directory "/home/mas")
  (message "here")
  (excursion-open-remote-directory "/bin")
  (message ">here"))
  ;; (excursion-open-remote-file "./ss.txt"))
