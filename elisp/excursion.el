;;; -*- lexical-binding: t; -*-

(defvar excursion-host "localhost")

(defvar excursion-port 7001)

(defvar excursion--process-name "excursion")

(defvar excursion--frame-types '((?^ . Data)
                                 (?\( . Open)
                                 (?&  . Save)))

(defun excursion--get-frame-type (char)
  "Use the CHAR to lookup the frame type."
  (cdr (assoc char excursion--frame-types)))

(defun excursion-terminate ()
  "Terminate the excursion"
  (interactive)
  (let ((pattern "^excursion")
        (processes (process-list)))
    (dolist (process processes)
      (when (string-match pattern (process-name process))
        (delete-process process)))))

(defun excursion-open-remote-file (filename)
  "Open FILENAME remotely. Will create a new connection if
necessary."
  (interactive "GFilename: ")
  ;; Concatentate filename length and the filename
  (let ((request (format "(%s|%s" (length filename) filename))
        (process (excursion--remote-connection)))
    ;; Put the filename on the process object
    (process-put process 'filename filename)
    (process-send-string excursion--process-name request)))

(defun excursion--split-at-first (delimiter string)
  "Split STRING at the first occurrence of DELIMITER and return
both."
  (let ((pos (string-match (regexp-quote delimiter) string)))
    (when pos
      (list (substring string 0 pos)
            (substring string (+ pos (length delimiter)))))))

(defun excursion--destructure-frame (frame)
  "Validate FRAME and return it as an alist."
  (let* ((frame-halves (excursion--split-at-first "|" frame))
         (preamble (car frame-halves))
         (data (cadr frame-halves)))
    ;; XXX: Err on bad frame
    (when (and preamble (not (string-empty-p preamble)))
      ;; Get the frame type and length
      (let ((type (excursion--get-frame-type (aref preamble 0)))
            (len (string-to-number (substring preamble 1))))
        ;; Return the frame as an alist
        (when (and type len data)
          `((type . ,type)
            (len . ,len)
            (data . ,data)))))))

(defun excursion--filter (process contents)
  "Handle all input and output for the socket."
  (when (buffer-live-p (process-buffer process))
    ;; New buffer for file
    (let* ((frame (excursion--destructure-frame contents))
           (filename (process-get process 'filename))
           (buffer (generate-new-buffer (file-name-nondirectory filename))))
      (excursion--setup-buffer buffer filename (cdr (assoc 'data frame)))
      (switch-to-buffer buffer))))

(defun excursion--setup-buffer (buffer filename contents)
  "Set up BUFFER for FILENAME and inject CONTENTS."
  (with-current-buffer buffer
    (setq buffer-file-name filename)
    (setq default-directory (concat "/excursion:" (file-name-directory filename)))
    ;; Automatically detect major mode
    (set-auto-mode)
    (insert contents)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun excursion--remote-connection ()
  "Get an existing connection or create a new one."
  (if-let ((process (get-process excursion--process-name)))
      ;; Existing process
      process
    (condition-case err
        ;; Create a new process if one doesn't exist
        (let ((process (open-network-stream excursion--process-name "*excursion*" excursion-host excursion-port)))
          (set-process-filter process 'excursion--filter)
          process)
      (file-error
       (error "failed to open remote connection: %s" (error-message-string err))))))

(progn
  (excursion-terminate)
  (excursion-open-remote-file "./scripts/nc_test.bash"))
  ;; (excursion-open-remote-file "./ss.txt"))
