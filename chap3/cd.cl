(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
  (format t "~{~a:~10t~a~%~}~%" cd)))

(defun read-prompt (message)
  (format *query-io* "~a: " message)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
    (read-prompt "Title")
    (read-prompt "Artist")
    (or (parse-integer (read-prompt "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped [Y/N]")))

;;;Ugly
(defun prompt-cds ()
  (loop (add-record (prompt-for-cd))
    (if (not (y-or-n-p "Another? [Y/N]")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(add-record (make-cd "Roses" "kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Fly" "Dixie Chicks" 9 t))

