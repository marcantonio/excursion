;;; -*- lexical-binding: t; -*-

(defvar excursion-host "localhost")
(defvar excursion-port 7001)
(defconst excursion--process-name "excursion")
(defconst excursion--frame-types '(("^" . Data)
                                   ("!" . Err)
                                   ("(" . Open)
                                   ("&"  . Save)))
(defvar excursion--queue nil)
(defvar excursion--data nil)
(defvar excursion-timeout 2)

;;(add-to-list 'file-name-handler-alist '("\\`/excursion:" . excursion-file-handler))

;;; Commands

(defun excursion-open-remote-file (filename)
  "Open FILENAME remotely. Will create a new connection if
necessary."
  (interactive "GFilename: ")
  ;; Concatentate filename length and the filename
  (let* ((request (format "(%s|%s" (length filename) filename))
         (process (excursion--remote-connection)))
    (excursion--queue-nq
     excursion--queue
     (lambda (data)
       (let ((buffer (generate-new-buffer (file-name-nondirectory filename))))
         (excursion--setup-buffer buffer filename data)
         (switch-to-buffer buffer))))
    (process-send-string excursion--process-name request)))

(defun excursion-open-remote-directory (directory)
  (interactive "DDirectory: ")
  (let ((request (format "~%s|%s" (length directory) directory))
        (process (excursion--remote-connection)))
    (excursion--queue-nq
     excursion--queue
     (lambda (data)
       (let ((buffer (generate-new-buffer (file-name-nondirectory directory))))
         (excursion--setup-buffer buffer directory data)
         (switch-to-buffer buffer))))
    (process-send-string excursion--process-name request)))

(defun excursion-terminate ()
  "Terminate the excursion"
  (interactive)
  (let ((pattern "^excursion")
        (processes (process-list)))
    (dolist (process processes)
      (when (string-match pattern (process-name process))
        (delete-process process)))))

;;; File operations

(defun excursion-file-handler (operation &rest args)
  (cond ((eq operation 'expand-file-name)
         (apply #'excursion-expand-file-name args))
        (t (let ((inhibit-file-name-handlers
                  (cons 'excursion-file-handler
                        (and (eq inhibit-file-name-operation operation)
                             inhibit-file-name-handlers)))
                 (inhibit-file-name-operation operation))
             (apply operation args)))))

(defun excursion-insert-file-contents (filename &optional visit beg end replace)
  (let* ((path (excursion-expand-file-name filename))
         (request (format "(%s|%s" (length path) path))
         (contents (excursion--make-request request)))
    (insert contents)
    (when visit
      (setq buffer-file-name filename
            buffer-read-only t)
      (set-visited-file-modtime)
      (set-buffer-modified-p nil))
    (cons path (length contents))))


(defun excursion-expand-file-name (filename &optional directory)
  (let* ((filename_len (length filename))
         (request
          (if (not directory)
              (format "*%s|%s" filename_len filename)
            (format "*%s;%s|%s" filename_len (length directory)
                    (concat filename directory)))))
    (excursion--make-request request)))

;;; Connection

(defun excursion--remote-connection ()
  "Get an existing connection or create a new one."
  (if-let ((process (get-process excursion--process-name)))
      ;; Existing process
      process
    (condition-case err
        ;; Create a new process if one doesn't exist
        (let ((process (open-network-stream excursion--process-name "*excursion*" excursion-host excursion-port)))
          (setq excursion--queue (excursion--queue-create))
          (setq excursion--data "")
          (process-put process 'results nil)
          (set-process-filter process 'excursion--filter)
          (set-process-sentinel process
                                (lambda (process event)
                                  (princ
                                   (format "Process: %s had the event '%s'" process event))))
          process)
      (file-error
       (error "failed to open remote connection: %s" (error-message-string err))))))

(defun excursion--filter (process contents)
  "Handle all output from the socket."
  (when (buffer-live-p (process-buffer process))
    ;; Append all data to excursion--data
    (setq excursion--data (concat excursion--data contents))
    ;; Attempt to read frames from the buffer
    (while-let ((frame (excursion--read-frame))
                (comp-fn (excursion--queue-dq excursion--queue))
                (type (cdr (assoc 'type frame)))
                (data (cdr (assoc 'data frame))))
      (cond ((eq type 'Data)
             (if (functionp comp-fn)
                 ;; Call frame completion handler
                 (funcall comp-fn data)
               ;; Put results on process
               (process-put process 'results data)))
            (t (message "ERR received %s: %s " type data))))))

(defun excursion--make-request (request)
  "Send REQUEST to process"
  (with-timeout (excursion-timeout (message "timeout failure"))
    (let ((process (excursion--remote-connection)))
      (excursion--queue-nq excursion--queue 'store)
      (process-send-string excursion--process-name request)
      (let ((result nil))
        ;; This blocks hard
        (while (not result)
          ;; consider (with-local-quit)
          (accept-process-output process 0.1 nil t)
          (setq result (process-get process 'results)))
        (process-put process 'results nil)
        result))))

;; Assumes excursion--data points to the start of a frame
(defun excursion--read-frame ()
  "Attempt to construct a frame from `excursion--data'. Return as an alist."
  (unless (and (not (null excursion--data))
               (string-empty-p excursion--data))
    ;; Match the type and length fields
    (string-match "^\\(.\\)\\([0-9]+\\)|" excursion--data)
    ;; Get the frame type, specified data length, and the start and end of the data
    (if-let* ((type-match (match-string 1 excursion--data))
              (type (excursion--get-frame-type type-match))
              (len-match (match-string 2 excursion--data))
              (data-len (string-to-number len-match))
              (data-start (match-end 0))
              (data-end (+ data-start data-len)))
        ;; Make sure there is a least enough for one frame
        (when (>= (length excursion--data) data-end)
          (let ((data (substring excursion--data data-start data-end)))
            ;; Truncate over the old data
            (setq excursion--data (substring excursion--data data-end))
            ;; Return as an alist
            `((type . ,type)
              (data-len . ,data-len)
              (data . ,data)))))))

(defun excursion--get-frame-type (str)
  "Use STR to lookup the frame type."
  (cdr (assoc str excursion--frame-types)))

;;; Completion queue

(defun excursion--queue-create ()
  "Create an empty queue."
  (list nil))

(defun excursion--queue-nq (q item)
  "Add ITEM to Q."
  (setcar q (nconc (car q) (cons item nil))))

(defun excursion--queue-dq (q)
  "Remove item from Q."
  (let ((item (car (car q))))
    (setcar q (cdr (car q)))
    item))

(defun excursion--queue-len (q)
  (length q))

;;; Util

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

;; (file-exists-p "/excursion:foo")
;; (expand-file-name "/excursion:foo")
;; (directory-files "/excursion:/home/mas")

;; (progn
;;   (excursion-terminate)
;;   ;;(excursion-expand-file-name "~/../mas/mm"))
;;   (excursion-open-remote-file "./Cargo.toml")
;;   (excursion-open-remote-file "./scripts/nc_test.bash")
;;   (excursion-open-remote-directory "/home/mas"))
