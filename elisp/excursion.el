;;; -*- lexical-binding: t; -*-

;;; >=28.1 file-name-concat, remote-file-name-inhibit-locks, make-lock-file-name.
;;; >=29.1 directory-abbrev-apply
;;; Add initial cache call

;; For dev
(let* ((excursion-path (file-name-directory (or load-file-name (buffer-file-name))))
       (excursion-test-path (concat excursion-path "tests/")))
  (add-to-list 'load-path excursion-path)
  (add-to-list 'load-path excursion-test-path))

(require 'cl-lib)
(require 'excursion-frame)

(defvar excursion-host "localhost")
(defvar excursion-port 7001)
(defvar excursion-timeout 2)
(defvar excursion-directory (concat user-emacs-directory "excursion/"))
(defvar excursion-auto-save-directory (concat excursion-directory "auto-saves"))

(defconst excursion--process-name "excursion")

(defvar excursion--data nil)
(defvar excursion--prefix "/excursion:")
(defvar excursion--user-home-dir "/home/mas/")

;; Make sure we are reinserted before tramp so it doesn't try to handle our prefix
(add-to-list 'file-name-handler-alist '("\\`/excursion:" . excursion-file-handler))
(with-eval-after-load 'tramp
  (setq file-name-handler-alist
        (rassq-delete-all 'excursion-file-handler file-name-handler-alist))
  (add-to-list 'file-name-handler-alist '("\\`/excursion:" . excursion-file-handler)))

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

;; verify-visited-file-modtime
;; load
;; file-name-all-completions

;; get-file-buffer
;; file-name-sans-versions

(defun excursion-file-handler (operation &rest args)
  "Excursion's file handler. Will pass OPERATION and ARGS to the
excursion equivalent or to the orignal handler if not in the
list."
  (cond
   ((eq operation 'expand-file-name) (apply #'excursion-expand-file-name args))
   ((eq operation 'file-remote-p) (apply #'excursion-file-remote-p args))
   ((eq operation 'file-attributes) (apply #'excursion-file-attributes args))
   ((eq operation 'file-symlink-p) (apply #'excursion-file-symlink-p args))
   ((eq operation 'directory-file-name) (apply #'excursion-directory-file-name args))
   ((eq operation 'file-name-as-directory) (apply #'excursion-file-name-as-directory args))
   ((eq operation 'file-name-directory) (apply #'excursion-file-name-directory args))
   ((eq operation 'file-name-nondirectory) (apply #'excursion-file-name-nondirectory args))
   ((eq operation 'file-name-case-insensitive-p) (apply #'excursion-file-name-case-insensitive-p args))
   ((eq operation 'abbreviate-file-name) (apply #'excursion-abbreviate-file-name args))
   ((eq operation 'file-readable-p) (apply #'excursion-file-readable-p args))
   ((eq operation 'file-writable-p) (apply #'excursion-file-writable-p args))
   ((eq operation 'file-exists-p) (apply #'excursion-file-exists-p args))
   ((eq operation 'verify-visited-file-modtime) (apply #'excursion-verify-visited-file-modtime args))
   ((eq operation 'file-truename) (apply #'excursion-file-truename args))
   ((eq operation 'file-directory-p) (apply #'excursion-file-directory-p args))
   ((eq operation 'directory-files) (apply #'excursion-directory-files args))
   ((eq operation 'make-auto-save-file-name) (funcall #'excursion-make-auto-save-file-name))
   ((eq operation 'make-lock-file-name) (apply #'excursion-make-lock-file-name args))
   ((eq operation 'file-locked-p) (apply #'excursion-file-locked-p args))
   ((eq operation 'lock-file) (apply #'excursion-lock-file args))
   ((eq operation 'file-in-directory-p) (apply #'excursion-file-in-directory-p args))
   ((eq operation 'file-equal-p) (apply #'excursion-file-equal-p args))
   ((eq operation 'substitute-in-file-name) (apply #'excursion-substitute-in-file-name args))
   ((eq operation 'load) (apply #'excursion-load args))
   ((eq operation 'insert-file-contents) (apply #'excursion-insert-file-contents args))
   (t
    (message "#%s %s" operation args)
    (let ((inhibit-file-name-handlers
           (cons 'excursion-file-handler
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
          (inhibit-file-name-operation operation))
      (apply operation args)))))

(defun excursion--run-stock-handler (operation args)
  "Run OPERATION on ARGS via the default file handler."
  (let ((inhibit-file-name-handlers
         ;; Inhibit tramp too. Not sure if this is right, but it will error on excursion
         ;; being an unknown method
         (append '(excursion-file-handler tramp-file-name-handler)
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun excursion-insert-file-contents (filename &optional visit beg end replace)
  (let* ((path (cdr (excursion--split-prefix (expand-file-name filename))))
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
;;   (let* ((filename "../Cargo.toml")
;;          (buffer (generate-new-buffer (file-name-nondirectory filename))))
;;     (with-current-buffer buffer
;;       (setq default-directory (concat "/excursion:electron:~/excursion/elisp/"))
;;       (excursion-insert-file-contents filename t)
;;       (set-auto-mode)
;;       (goto-char (point-min)))
;;     (switch-to-buffer buffer)))

;; TODO: handle quoting: https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Expansion.html#index-file_002dname_002dquote
(defun excursion-expand-file-name (filename &optional directory)
  "Excursion's expand-file-name"
  (let* ((parts (excursion--split-prefix filename))
         (prefix (car parts))
         (filepath (cdr parts))
         (expanded-file
          (cond
           ;; If there is no prefix and it's absolute, treat it as a local file
           ((and (string-empty-p prefix)
                 (file-name-absolute-p filepath))
            (excursion--run-stock-handler #'expand-file-name (list filepath)))
           ;; If we have prefix and a `~' it might be absolute. Go remote to find out
           ((and (not (string-empty-p prefix))
                 (string-prefix-p "~" filepath))
            (let ((~expanded (excursion--make-request
                              (format "*%s|%s" (length filepath) filepath))))
              ;; If it's still not expanded, treat it as relative and just add the user's
              ;; home dir
              (if (not (file-name-absolute-p ~expanded))
                  (concat excursion--user-home-dir ~expanded)
                ~expanded)))
           ;; If there's a prefix and it's absolute, do nothing
           ((and (not (string-empty-p prefix))
                 (string-prefix-p "/" filepath))
            filepath)
           ;; For everything that's left, just fix up the directory
           (t (let ((directory
                     (cond
                      ;; Always treat prefixed relative paths wrt the user's home dir
                      ((and (not (string-empty-p prefix))
                            (not (file-name-absolute-p filepath)))
                       excursion--user-home-dir)
                      ;; If directory is passed, expand it
                      ((not (null directory))
                       (expand-file-name directory))
                      ;; Use `default-directory' as a last resort
                      (t (expand-file-name default-directory)))))
                (file-name-concat directory filepath))))))
    ;; File is expanded, but we might need to re-add the prefix
    (if (excursion--file-p expanded-file)
        expanded-file
      (concat prefix expanded-file))))

(defun excursion-file-remote-p (filename &optional identification connected)
  "Excursion's file-remote-p"
  ;; Only expand if we are connected
  (let* ((is-connected (excursion--connected-p))
         (filename (if is-connected (expand-file-name filename) filename)))
    (when (excursion--file-p filename)
      ;; If it's not connected, and we care, return nil
      (when (or is-connected (not connected))
        ;; Otherwise, parse the file path
        (cond ((equal identification 'method) "excursion")
              ((equal identification 'host) (excursion--get-host filename))
              (t (excursion--full-prefix filename)))))))

(defun excursion-file-attributes (filename &optional id-format)
  "Excursion's file-attributes"
  (let* ((filepath (cdr (excursion--split-prefix
                         (expand-file-name filename))))
         (result (excursion--make-request
                  (format ":%s|%s"
                          (length filepath)
                          filepath))))
    ;; Parse unless it's an empty string, which indicates file not found
    (unless (and (stringp result) (string-empty-p result))
      (list (or (string-prefix-p "d" (nth 7 result)) ; is dir or symlink
                (and (string-prefix-p "l" (nth 7 result))
                     (nth 10 result)))
            (string-to-number (nth 0 result))                   ; num links
            (string-to-number (nth 1 result))                   ; uid
            (string-to-number (nth 2 result))                   ; gid
            (seconds-to-time (string-to-number (nth 3 result))) ; atime
            (seconds-to-time (string-to-number (nth 4 result))) ; mtime
            (seconds-to-time (string-to-number (nth 5 result))) ; ctime
            (string-to-number (nth 6 result))                   ; size
            (nth 7 result)                                      ; mode string
            nil                                                 ; unused
            (string-to-number (nth 8 result))                   ; inode
            ;; TODO: This is the actual remote device id which might not be unique. Tramp
            ;; uses -1 plus a unique integer. I don't want to overthink this for now, but
            ;; there's more we can when this is an issue
            (cons -2 (string-to-number (nth 9 result)))))))      ; device id

(defun excursion-file-symlink-p (filename)
  "Excursion's file-symlink-p."
  (let ((f (file-attribute-type (file-attributes filename))))
    (and (stringp f) f)))

(defun excursion-directory-file-name (dirname)
  "Excursion's directory-file-name"
  (let* ((parts (excursion--split-prefix dirname))
         (prefix (car parts))
         (filepath (cdr parts)))
    (concat prefix (excursion--run-stock-handler
                    #'directory-file-name (list filepath)))))

(defun excursion-file-name-as-directory (filename)
  "Excursion's file-name-as-directory."
  (let* ((parts (excursion--split-prefix filename))
         (prefix (car parts))
         (filepath (cdr parts)))
    (concat prefix (excursion--run-stock-handler
                    #'file-name-as-directory (list filepath)))))

(defun excursion-file-name-directory (filename)
  "Excursion's file-name-directory."
  (let* ((parts (excursion--split-prefix filename)))
    (concat (car parts)
            (excursion--run-stock-handler #'file-name-directory (list (cdr parts))))))

(defun excursion-file-name-nondirectory (filename)
  "Excursion's file-name-nondirectory."
  (let ((parts (excursion--split-prefix filename)))
    (excursion--run-stock-handler #'file-name-nondirectory (list (cdr parts)))))

(defun excursion-file-name-case-insensitive-p (filename)
  "Excursion's file-name-case-insensitive-p."
  nil)

(defun excursion-abbreviate-file-name (filename)
  "Excursions's abbreviate-file-name."
  (let* ((case-fold-search ; for `directory-abbrev-apply'
          (file-name-case-insensitive-p filename))
         (parts (excursion--split-prefix filename))
         (prefix (car parts))
         (filepath (cdr parts))
         (home-dir (concat
                    (cdr (excursion--split-prefix
                          (excursion-expand-file-name
                           (concat prefix "~")))) "/")))
    (when (and
           (string-prefix-p home-dir filepath) ; the home dir is at the beginning
           (not (string= (file-name-directory  ; and it's not a root dir
                          (directory-file-name home-dir)) "/")))
      (setq filepath (concat "~/" (substring filepath (length home-dir)))))
    (concat prefix
            (excursion--run-stock-handler #'directory-abbrev-apply (list filepath)))))

;; TODO: Consider lumping this in with `excursion-file-attributes' when we have a cache
(defun excursion-file-exists-p (filename)
  "Excursion's file-exists-p."
  (excursion--check-file "e" filename))

;; TODO: Consider lumping this in with `excursion-file-attributes' when we have a cache
(defun excursion-file-readable-p (filename)
  "Excursion's file-readable-p."
  (excursion--check-file "r" filename))

;; TODO: Consider lumping this in with `excursion-file-attributes' when we have a cache
(defun excursion-file-writable-p (filename)
  "Excursion's file-writable-p."
  (excursion--check-file "w" filename))

(defun excursion--check-file (op filename)
  "Calls stat2 with OP on FILENAME."
  (let* ((filepath (cdr (excursion--split-prefix (expand-file-name filename))))
         (result (excursion--make-request
                  (format "_%s;1|%s%s" (length filepath) filepath op))))
    (string= result "1")))

;; TODO: handle quoting: https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Expansion.html#index-file_002dname_002dquote
(defun excursion-verify-visited-file-modtime (&optional buffer)
  "Excursion's verify-visited-file-modtime."
  (let* ((buffer (or buffer (current-buffer)))
         (filename (buffer-file-name buffer)))
    (if (not filename)
        t
      (let ((modtime (visited-file-modtime)))
        (cond
         ((time-equal-p modtime 0) t)
         ((not (file-exists-p filename)) nil)
         (t (time-equal-p modtime
                          (file-attribute-modification-time
                           (file-attributes filename)))))))))

;; TODO: Finish tests once this runs
;; (with-temp-buffer
;;   (set-visited-file-name "/excursion:electron:~/otium")
;;   (verify-visited-file-modtime))
;; (get-file-buffer)
;; (make-auto-save-file-name)
;; (auto-save-mode)
;; (seq-find (lambda (x) (equal x 'auto-save-mode)) minor-mode-list)
;; (setq auto-save-default t)

;; TODO: handle quoting: https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Expansion.html#index-file_002dname_002dquote
(defun excursion-file-truename (filename)
  "Excursion's file-truename."
  (let* ((prefix (car (excursion--split-prefix filename t)))
         (f (file-attribute-type (file-attributes filename))))
    (if (symbolp f)
        (expand-file-name filename)
      (concat prefix f))))

(defun excursion-file-directory-p (filename)
  "Excursion's file-directory-p."
  (let* ((f (file-attribute-type (file-attributes filename))))
    (and (symbolp f) f)))

(defun excursion-directory-files (directory &optional full-name match-regexp nosort count)
  "Excursion's directory-files."
  (if (file-directory-p directory)
      (let* ((filepath (cdr (excursion--split-prefix (expand-file-name directory))))
             ;; Remote will take care of expanding
             (result (excursion--make-request
                      (format "~%s;1|%s%s"
                              (length filepath)
                              filepath
                              (if full-name "1" "0"))))
             (filtered (if match-regexp
                           (seq-filter
                            (lambda (e) (string-match-p match-regexp e))
                            (if (listp result) result (list result))) ; make sure it's a list
                         result))
             (sorted (if nosort
                         filtered
                       (seq-sort #'string< filtered))))
        (if (and (integerp count) (> count 0))
            (seq-take sorted count)
          sorted))
    ;; Tramp errors in this case, so we will too
    (when (not (file-exists-p directory))
      (error "No such file or directory"))))

;; TODO: Finish tests once this runs
(defun excursion-make-auto-save-file-name ()
  "Excursion's make-auto-save-file-name. Will create `excursion-auto-save-directory' if needed."
  (when-let ((auto-save-default)
             (excursion-auto-save-directory (expand-file-name excursion-auto-save-directory))
             (filename (expand-file-name (buffer-file-name))))
    (unless (file-exists-p excursion-auto-save-directory)
      (with-file-modes #o0700
        (make-directory excursion-auto-save-directory t)))
    (concat excursion-auto-save-directory "/" (subst-char-in-string ?/ ?# filename) "#")))

;; (setq auto-save-default t)
;; ;;(make-auto-save-file-name)
;; (excursion-make-auto-save-file)
;; (lock-file "/excursion:electron:/home/mas/excursion/Cargo.toml")

;; (excursion-make-lock-file-name "/excursion:electron:/home/mas/excursion/Cargo.toml")
;; (excursion--get-lock-file "/excursion:electron:/home/mas/excursion/Cargo.toml")
;; (excursion-file-locked-p "/excursion:electron:/home/mas/excursion/Cargo.toml")

;; (excursion-lock-file "/excursion:electron:/home/mas/excursion/Cargo.toml")

(defun excursion-lock-file (file)
  (unless (eq (file-locked-p file) t) ; t means we own the lock
    (when (and buffer-file-truename
               (not (verify-visited-file-modtime))
               (file-exists-p file))
      (message ">>%s" buffer-file-truename))))

;;(excursion-lock-file "/excursion:electron:/home/mas/excursion/Cargo.toml")

(defun excursion-file-locked-p (file)
  "Excursion's file-locked-p."
  (save-match-data
    (when-let ((lock-info (excursion--get-lock-file file))
               (re "^\\([[:alnum:]_-]+\\)@\\([[:alnum:]_.-]+\\)\\.\\([0-9]+\\)$")
               (_ (string-match re lock-info)))
      ;; Return t if locked by me, or the user with the lock
      (or
       (and (equal (match-string 1 lock-info) (user-login-name)) ; user
            (equal (match-string 2 lock-info) (system-name))     ; host
            (equal (match-string 3 lock-info) (emacs-pid)))      ; pid
       (match-string 1 lock-info)))))

(defun excursion-make-lock-file-name (file)
  "Excursion's make-lock-file-name."
  (and create-lockfiles
       (not remote-file-name-inhibit-locks)
       (excursion--run-stock-handler #'make-lock-file-name (list file))))

(defun excursion--get-lock-file (file)
  "Get lock file info for FILE or return nil."
  (file-symlink-p (make-lock-file-name file)))

(defun excursion-file-in-directory-p (file dir)
  "Excursion's file-in-directory-p."
  (when (excursion--remote-equal-p file dir)
    (excursion--run-stock-handler #'file-in-directory-p (list file dir))))

(defun excursion-file-equal-p (file1 file2)
  "Excursion's file-equal-p."
  (when (excursion--remote-equal-p file1 file2)
    (excursion--run-stock-handler #'file-equal-p (list file1 file2))))

(defun excursion-substitute-in-file-name (filename)
  (excursion--run-stock-handler #'substitute-in-file-name (list filename)))

(defun excursion-load (file &optional noerror nomessage nosuffix must-suffix)
  (excursion--run-stock-handler #'load (list file noerror nomessage nosuffix must-suffix)))

;;(load "/excursion:electron:~/excursion/elisp/mm.el")

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
          (process-put process 'host "electron") ; TODO: make this excursion-host
          (process-put process 'results nil)
          (setq excursion--data "")
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
  "Reads frames from CONTENTS. Can process multiple in one call. The
results of are put onto PROCESS and may be a list or a scalar
depending on the call."
  (when (buffer-live-p (process-buffer process))
    ;; Append all data to excursion--data
    (setq excursion--data (concat excursion--data contents))
    ;; Attempt to read frames from the buffer
    (when-let ((frame (excursion--read-frame))
               (type (excursion--frame-type frame))
               (data (excursion--frame-data frame)))
      (cond ((eq type 'Data)
             (let ((segments nil)
                   (i 0))
               ;; Build up a list of segments
               (while-let ((segment (excursion--frame-get-segment frame i)))
                 (push segment segments)
                 (cl-incf i))
               ;; Put results on process. If there's only 1 element, just return that
               (process-put process 'results
                            (if (equal (length segments) 1)
                                (car segments)
                              (nreverse segments)))))
            ((eq type 'Err) (error data))
            (t (error "Error: unexpected frame received %s: %s " type data))))))

;; Assumes excursion--data points to the start of a frame
(defun excursion--read-frame ()
  "Attempt to construct a frame from `excursion--data'. Return as an
alist or nil if the whole frame hasn't arrived."
  (unless (and (not (null excursion--data))
               (string-empty-p excursion--data))
    ;; Get the frame type, specified data length, and the start and end of the data
    (when-let
        ((preamble (excursion--parse-preamble excursion--data))
         (type-char (car preamble))
         (data-len (cadr preamble))
         (segment-lens (caddr preamble))
         (data-start (cadddr preamble))
         (data-end (+ data-start data-len)))
      ;; Make sure there is a least enough for one frame
      (when (>= (length excursion--data) data-end)
        (let ((data (substring excursion--data data-start data-end)))
          ;; Truncate over the old data
          (setq excursion--data (substring excursion--data data-end))
          ;; Make a new frame
          (excursion--frame-create-from type-char segment-lens data))))))

(defun excursion--parse-preamble (data)
  "Extract type, lengths and payload offset from DATA. Returns nil
if any part is missing or invalid."
  (unless (string-empty-p data)
    (let* ((type (aref data 0))               ; first char as type
           (delim-pos (cl-position ?| data))) ; index of `|'
      (when (and delim-pos (> delim-pos 1))
        ;; Split the len field by `;', sum, and return the sum and the lengths
        (let* ((len-str (substring data 1 delim-pos))
               (seg-lens (mapcar #'string-to-number
                                 (split-string len-str ";")))
               (len (apply '+ seg-lens)))
          (list type len seg-lens (+ delim-pos 1)))))))

(defun excursion--make-request (request)
  "Send REQUEST to process."
  (with-timeout (excursion-timeout
                 (progn
                   (message "timeout failure")
                   (excursion-terminate)
                   (excursion--remote-connection)
                   'timeout))
    ;; It's important to forget `default-directory' here. Otherwise, we get into a loop
    ;; when functions like `expand-file-name' are called before everything is loaded. For
    ;; example, if the first thing you do is call `expand-file-name', and
    ;; `default-directory' has `/excursion::' as a prefix. Then, as part of that call,
    ;; `open-network-stream' will be called. Somewhere in there `(expand-file-name
    ;; "~/.emacs.d/elisp" "/excursion::whatever")' is called. This, in turn, tries to call
    ;; `open-network-stream', etc. Could consider limiting this to when
    ;; `excursion--file-p' is true. Also note that sequential binding is required here so
    ;; default-directory is set first.
    (let* ((default-directory nil)
           (process (excursion--remote-connection)))
      (process-send-string process request)
      ;; This blocks hard
      (excursion--wait-for-data process))))

(defun excursion--wait-for-data (process)
  (let (results)
    (while (not results)
      ;; consider (with-local-quit)
      (accept-process-output process 0.1 nil t)
      (setq results (process-get process 'results)))
    (process-put process 'results nil) ; clear 'results
    results))

(defun excursion--connected-p ()
  "Checks if the current connection is open."
  (when-let ((process (get-process excursion--process-name))
             (process-live-p process)
             (status (process-status process)))
    (memq status '(open))))

(defun excursion--get-host (&optional file)
  "Get the hostname from the connection or FILE if passed. Will
return nil if no file is passed and there's no connection."
  (or (excursion--parse-filename file 'host)
      (when-let ((process (get-process excursion--process-name)))
        (process-get process 'host))))

(defun excursion--full-prefix (&optional file)
  "Build the whole remote prefix with the connection string from the
process or FILE is non-nil."
  (when-let ((host (excursion--get-host file)))
    (concat excursion--prefix host ":")))

;;; Util

(defun excursion--split-prefix (str &optional required)
  "Splits STR into a prefix and filepath and returns them as a
cons. If there is no prefix, the car will be `nil' unless
REQUIRED is t, in which case an error is signaled."
  (save-match-data
    (if (string-match "^/excursion:[^:]+:" str)
        (let ((prefix (match-string 0 str)))
          (cons prefix (substring str (match-end 0))))
      (if (not required)
          (cons "" str)
        (user-error "Invalid excursion file: %s" str)))))

;; TODO: Make this handle the host too
(defun excursion--file-p (file &optional required)
  "True if FILE starts with excursion's prefix. Will error if
REQUIRED is set."
  (or (string-prefix-p excursion--prefix file)
      (when required (user-error "Invalid excursion file: %s" file))))

(defun excursion--parse-filename (filename &optional part)
  "Parse FILENAME and return the method, host, and file as a
list. PART may be included to just return either the `'method',
`'host', or `'file' portions.

Will return nil if it's not an excursion file, the prefix
is malformed, or if PART is unknown."
  (when (and (not (null filename))
             (excursion--file-p filename))
    (save-match-data
      (let ((re "^/\\([^:]+\\):\\([^:]*\\):\\(.*\\)$"))
        (when (string-match re filename)
          (let ((method (match-string 1 filename))
                (host (match-string 2 filename))
                (file (match-string 3 filename)))
            (pcase part
              ('method method)
              ('host host)
              ('file file)
              ('nil (list method host file)))))))))

(defun excursion--remote-equal-p (file1 file2)
  "Test that the method and host parts of FILE1 and FILE2 are the
same."
  (let ((p1 (excursion--parse-filename file1))
        (p2 (excursion--parse-filename file2)))
    (and (string= (car p1)
                  (car p2))
         (string= (cadr p1)
                  (cadr p2)))))

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
;; (excursion-expand-file-name "/excursion:electron:~/otium")
(provide 'excursion)
