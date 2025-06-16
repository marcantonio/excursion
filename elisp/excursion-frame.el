;;; -*- lexical-binding: t; -*-

(defconst excursion--frame-types
  '((?^  . Data)
    (?!  . Err)
    (?\( . Open)
    (?-  . Rm)
    (?&  . Save)
    (?:  . Stat)
    (?>  . Sym)))

(cl-defstruct (excursion--frame
               (:constructor excursion--frame-create))
  "Excursion protocol frame.

Fields:
- type:      Frame type as a symbol
- len:       Total lenght of the data payload
- positions: A list of cons cells containing the start and
             end offsets for each segment
- data:      Payload data containing all segments"
  type len positions data)

(defun excursion--frame-create-from (ty lengths data)
  "Create a frame of type TY, with segment LENGTHS from DATA."
  (let* ((i 0)
         ptmp                           ; intermediate space for building positions
         (type (alist-get ty excursion--frame-types))
         (len (apply '+ lengths))
         (positions (dolist (len lengths (vconcat (nreverse ptmp)))
                      (push (cons i (+ i len)) ptmp)
                      (setq i (+ i len)))))
    (unless type (error "ERR invalid type"))
    (excursion--frame-create
     :type type
     :len len
     :positions positions
     :data data)))

;;;
(defun excursion--frame-get-type (str)
  "Use STR to lookup the frame type."
  (cdr (assoc str excursion--frame-types)))

(cl-defmethod excursion--frame-get-segment ((f excursion--frame) idx)
  "Return frame segment at IDX. Return nil if out of range."
  (let ((positions (excursion--frame-positions f)))
    (when (< idx (length positions))
      (let ((pair (aref positions idx)))
        (substring (excursion--frame-data f) (car pair) (cdr pair))))))

(provide 'excursion-frame)
