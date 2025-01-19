(progn
  (excursion-terminate)
  (let ((pairs
         '((("foo" "bar") "/home/mas/excursion/bar/foo")
           (("/foo" "bar") "/foo")
           (("~/foo" "~ms") "/home/mas/foo")
           (("~/foo" "bar") "/home/mas/foo")
           (("~foo" "bar") "/home/mas/excursion/bar/~foo")
           (("~baz" "bar") "/home/mas/excursion/bar/~baz")
           (("~/foo" "bar") "/home/mas/foo")
           (("foo" "~bar") "/home/mas/excursion/~bar/foo")
           (("/foo" "~bar") "/foo")
           (("~/foo" "~bar") "/home/mas/foo")
           (("~foo" "~bar") "/home/mas/excursion/~bar/~foo")
           (("~baz" "~/bar") "/home/mas/bar/~baz")
           (("~mas" "~bar") "/home/mas")
           (("~/foo" "~bar") "/home/mas/foo")
           (("foo" "~/bar") "/home/mas/bar/foo")
           (("/foo" "~/bar") "/foo")
           (("~/foo" "~/bar") "/home/mas/foo")
           (("~foo" "~/bar") "/home/mas/bar/~foo")
           (("~baz" "~/bar") "/home/mas/bar/~baz")
           (("~mas" "~/bar") "/home/mas")
           (("~/foo" "~/bar") "/home/mas/foo")
           (("foo" "/bar") "/bar/foo")
           (("/foo" "/bar") "/foo")
           (("~/foo" "/bar") "/home/mas/foo")
           (("~foo" "/bar") "/bar/~foo")
           (("~mas" "/bar") "/home/mas")
           (("~baz" "/bar") "/bar/~baz")
           (("~/foo" "~mas") "/home/mas/foo")
           (("foo" "~mas") "/home/mas/foo")
           (("/foo" "~mas") "/foo")
           (("~/foo" "~mas") "/home/mas/foo")
           (("~foo" "~mas") "/home/mas/~foo")
           (("~mas" "~mas") "/home/mas")
           (("~baz" "~mas") "/home/mas/~baz")
           (("~/foo" "~mas") "/home/mas/foo")
           (("~/foo" "~mas") "/home/mas/foo")
           (("foo" "~ms") "/home/mas/excursion/~ms/foo")
           (("/foo" "~ms") "/foo")
           (("~/foo" "~ms") "/home/mas/foo")
           (("~foo" "~ms") "/home/mas/excursion/~ms/~foo")
           (("~mas" "~ms") "/home/mas")
           (("~baz" "~ms") "/home/mas/excursion/~ms/~baz")))
        (count 1))
    (condition-case err
        (catch 'done
          (dolist (pair pairs)
            (let* ((args (car pair))
                   (file (car args))
                   (dir (cadr args))
                   (remote-result (cadr pair))
                   (local-result (excursion-expand-file-name file dir)))
              ;;(message ">%s" pair)
              (when (not (equal local-result remote-result))
                (message ">>>>%d \"%s\" \"%s\"\n  local:  %s\n  remote: %s"
                         count file dir local-result remote-result)
                (throw 'done "doneso")))
            (setq count (1+ count)))
          (error
           (message "An error occurred: %s" (error-message-string err)))))))
