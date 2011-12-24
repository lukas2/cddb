(defvar *db* nil)
(defvar *file* "./data.txt")

(defun add-record (cd) (push cd *db*))

(defun make-cd ( title artist rating ripped )
  (list :title title :artist artist :rating rating :ripped ripped))

(defun dump-db ()
  (dolist (cd *db*)
    (print (format t "~{~a:~10t~a~%~}%" cd))))

(defun prompt-read ( prompt )
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
      (if (not (y-or-n-p "Another? [y/n]: ")) (save-and-return))))

(defun save-and-return ()
  (save-db *file*)
  (quit))

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


(add-record (make-cd "Achtung, Baby!" "U2" 8 nil ))
(add-record (make-cd "Dangerous" "Michael Jackson" 8 t))
(add-record (make-cd "Automatic for the people" "R.E.M." 9 t))

(add-cds)
(dump-db)

