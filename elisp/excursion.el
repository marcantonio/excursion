;;; -*- lexical-binding: t; -*-

(defvar excursion-host "localhost")
(defvar excursion-port 7001)
(defvar excursion-timeout 2)

(defconst excursion--process-name "excursion")
(defconst excursion--frame-types '(("^" . Data)
                                   ("!" . Err)
                                   ("(" . Open)
                                   ("&"  . Save)))
(defvar excursion--data nil)
(defvar excursion--prefix "/excursion:")

(add-to-list 'file-name-handler-alist '("\\`/excursion:" . excursion-file-handler))

;;; Commands

;; (defun excursion-open-remote-file (filename)
;;   "Open FILENAME remotely. Will create a new connection if
;; necessary."
;;   (interactive "GFilename: ")
;;   ;; Concatentate filename length and the filename
;;   (let* ((request (format "(%s|%s" (length filename) filename))
;;          (process (excursion--remote-connection)))
;;     (excursion--queue-nq
;;      excursion--queue
;;      (lambda (data)
;;        (let ((buffer (generate-new-buffer (file-name-nondirectory filename))))
;;          (excursion--setup-buffer buffer filename data)
;;          (switch-to-buffer buffer))))
;;     (process-send-string excursion--process-name request)))

;; (defun excursion-open-remote-directory (directory)
;;   (interactive "DDirectory: ")
;;   (let ((request (format "~%s|%s" (length directory) directory))
;;         (process (excursion--remote-connection)))
;;     (excursion--queue-nq
;;      excursion--queue
;;      (lambda (data)
;;        (let ((buffer (generate-new-buffer (file-name-nondirectory directory))))
;;          (excursion--setup-buffer buffer directory data)
;;          (switch-to-buffer buffer))))
;;     (process-send-string excursion--process-name request)))

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
  (message "op: %s %s" operation args)
  (cond ((eq operation 'expand-file-name)
         (apply #'excursion-expand-file-name args))
        ;; ((eq operation 'insert-file-contents)
        ;;  (apply operation args))
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

;; (progn
;;   (excursion-terminate)
;;   (let* ((filename "src/main.rs")
;;          (buffer (generate-new-buffer (file-name-nondirectory filename))))
;;     (with-current-buffer buffer
;;       (setq default-directory (concat "/excursion:" (file-name-directory filename)))
;;       (excursion-insert-file-contents filename t)
;;       (set-auto-mode)
;;       (goto-char (point-min)))
;;     (switch-to-buffer buffer)))

(defun excursion-expand-file-name (filename &optional directory)
  "Excursion's expand-file-name"
  (let ((directory
         ;; First fix up the directory. The remote handles all expansions, but we can make
         ;; sure the directory and default-directory are right first
         (cond ((and
                 (null directory)
                 (not (string-prefix-p "/" filename))) default-directory)
               ((or
                 (string-prefix-p "/" filename)
                 (string-prefix-p "~/" filename)) "")
               ((or
                 (string-prefix-p "~" filename)
                 (string-prefix-p "~" directory)) directory)
               ((not (string-prefix-p "/" directory))
                (concat default-directory directory))
               (t directory))))
    ;; Make the request
    (let ((res
           (excursion--make-request
            (format "*%s;%s|%s"
                    (length filename)
                    (length directory)
                    (concat filename directory)))))
      ;; Add our prefix if it's missing
      (if (excursion--file-p res)
          res
        (concat excursion--prefix res)))))

;;; Connection

(defun excursion--remote-connection ()
  "Get an existing connection or create a new one."
  (if-let ((process (get-process excursion--process-name)))
      ;; Existing process
      process
    (condition-case err
        ;; Create a new process if one doesn't exist
        (let ((process (open-network-stream
                        excursion--process-name
                        "*excursion*"
                        excursion-host
                        excursion-port)))
          (setq excursion--data "")
          (process-put process 'results nil)
          (set-process-filter process 'excursion--filter)
          (set-process-sentinel
           process
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
                (type (cdr (assoc 'type frame)))
                (data (cdr (assoc 'data frame))))
      (cond ((eq type 'Data)
             ;; Put results on process
             (process-put process 'results data))
            (t (message "ERR received %s: %s " type data))))))

(defun excursion--make-request (request)
  "Send REQUEST to process"
  (with-timeout (excursion-timeout
                 (progn
                   (message "timeout failure")
                   'timeout))
    ;; It's important to forget `default-directory' here. Otherwise, we get into a loop
    ;; when functions like `expand-file-name' are called before everything is loaded. For
    ;; example, if the first thing you do is call `expand-file-name', and
    ;; `default-directory' has `/excursion:' as a prefix. Then, as part of that call,
    ;; `open-network-stream' will be called. Somewhere in there `(expand-file-name
    ;; "~/.emacs.d/elisp" "/excursion:whatever")' is called. This, in turn, tries to call
    ;; `open-network-stream', etc. Could consider limiting this to when `excursion--file-p'
    ;; is true
    (let* ((default-directory nil)
           (process (excursion--remote-connection)))
      (process-send-string excursion--process-name request)
      (let ((result nil))
        ;; This blocks hard
        (while (not result)
          ;; consider (with-local-quit)
          (accept-process-output process 0.1 nil t)
          (setq result (process-get process 'results)))
        (process-put process 'results nil)
        result))))

(defun excursion--parse-preamble (frame)
  "Extract type and ength from FRAME. Returns nil if either is missing."
  (if (string-empty-p frame)
      nil
    (let ((ty (string (aref frame 0))) ; first char as a string
          (delim-pos (cl-position ?| frame))) ; index of `|'
      (if (and delim-pos (> delim-pos 1))
          (let ((len (substring frame 1 delim-pos)))
            (list ty len (+ delim-pos 1)))
        nil))))

;; Assumes excursion--data points to the start of a frame
(defun excursion--read-frame ()
  "Attempt to construct a frame from `excursion--data'. Return as an
alist or nil if the whole frame hasn't arrived."
  (unless (and (not (null excursion--data))
               (string-empty-p excursion--data))
    ;; Get the frame type, specified data length, and the start and end of the data
    (let ((preamble (excursion--parse-preamble excursion--data)))
      (when (not (null preamble))
        (let* ((type (excursion--get-frame-type (car preamble)))
               (data-len (string-to-number (cadr preamble)))
               (data-start (caddr preamble))
               (data-end (+ data-start data-len)))
          ;; Make sure there is a least enough for one frame
          (when (>= (length excursion--data) data-end)
            (let ((data (substring excursion--data data-start data-end)))
              ;; Truncate over the old data
              (setq excursion--data (substring excursion--data data-end))
              ;; Return as an alist
              `((type . ,type)
                (data-len . ,data-len)
                (data . ,data)))))))))

(defun excursion--get-frame-type (str)
  "Use STR to lookup the frame type."
  (cdr (assoc str excursion--frame-types)))

;;; Util

(defun excursion--file-p (file)
  "True is FILE starts with excursion's prefix."
  (string-prefix-p excursion--prefix file))

(defun excursion--setup-buffer (buffer filename contents)
  "Set up BUFFER for FILENAME and inject CONTENTS."
  (with-current-buffer buffer
    (setq buffer-file-name filename)
    (setq default-directory (concat excursion--prefix
                                    (file-name-directory filename)))
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

(provide 'excursion)
