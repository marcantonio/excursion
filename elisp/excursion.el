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
  (let ((request (format "(%s|%s" (length filename) filename))
        (process (excursion--remote-connection)))
    (process-put process 'filename filename)
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

(defun excursion--filter (process contents)
  (when (buffer-live-p (process-buffer process))
    (let* ((frame (excursion--destructure-frame contents))
           (filename (process-get process 'filename))
           (buffer (generate-new-buffer (file-name-nondirectory filename))))
      (excursion--setup-buffer buffer filename (cdr (assoc 'data frame)))
      (switch-to-buffer buffer))))

(defun excursion--setup-buffer (buffer filename contents)
  (with-current-buffer buffer
    (setq buffer-file-name filename)
    (setq default-directory (concat "/excursion:" (file-name-directory filename)))
    (set-auto-mode)
    (insert contents)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun excursion--remote-connection ()
  (if-let ((process (get-process excursion--process-name)))
      process
    (condition-case err
        (let ((process (open-network-stream excursion--process-name "*excursion*" excursion-host excursion-port)))
          (set-process-filter process 'excursion--filter)
          process)
      (file-error
       (error "failed to open remote connection: %s" (error-message-string err))))))

(progn
  (excursion-terminate)
  (excursion-open-remote-file "./scripts/nc_test.bash"))
  ;; (excursion-open-remote-file "./ss.txt"))
