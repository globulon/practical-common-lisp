;;;use of global variable ????????
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

(defun select-by (match-criteria)
  (remove-if-not match-criteria *db*))

(defun artist-selector (name)
  #'(lambda(cd) (equal (getf cd :artist) name)))

(defun title-selector (value)
  #'(lambda (cd) (equal (getf cd :title) value)))

(defun select-by-artist (name)
  (select-by (artist-selector  name)))

(defun select-by-title (exp)
  (select-by (title-selector exp)))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda(cd)
      (and
       (if title  (equal (getf cd :title)  title ) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun select (predicate)
  (remove-if-not predicate *db*))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title (setf (getf row :title) title))
	       (if artist (setf (getf row :artist) artist))
	       (if rating (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row ) *db*)))

(add-record (make-cd "Roses" "kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Fly" "Dixie Chicks" 9 t))


