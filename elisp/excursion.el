;;; -*- lexical-binding: t; -*-

(defvar excursion-host "localhost")

(defvar excursion-port 7001)

(defvar excursion--process-name "excursion")

(defvar excursion--frame-types '((?^ . Data)
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
  (let ((request (format "(%s|%s" (length filename) filename)))
    (excursion--remote-connection)
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

(defun excursion-filter (process string)
  (when (buffer-live-p (process-buffer process))
    (let ((frame (excursion--destructure-frame string)))
      (message (cdr (assoc 'data frame))))))

(defun excursion--remote-connection ()
  (when (null (get-process excursion--process-name))
    (condition-case err
        (progn
          (open-network-stream
           excursion--process-name "*excursion*" excursion-host excursion-port)
          (set-process-filter (get-process excursion--process-name) 'excursion-filter))
      (file-error
       (error "failed to open remote connection: %s" (error-message-string err))))))

(progn
  (excursion-terminate)
  ;; (excursion-open-remote-file "./scripts/nc_test.bash"))
  (excursion-open-remote-file "./ss.txt"))
