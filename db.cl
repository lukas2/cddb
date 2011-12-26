(defvar *db* nil)
(defvar *file* "./data.txt")

(defun add-record (cd) (push cd *db*))

(defun add-record-unless-exists (cd)
  (if (select (where :rating (getf cd :rating) :artist (getf cd :artist) :title (getf cd :title) :ripped (getf cd :ripped)))
    ()
    (add-record cd )
    )
  )

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
      (if (not (y-or-n-p "Another? [y/n]: ")) (save-and-quit))))

(defun save-and-quit ()
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

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(load-db *file*)

; seed data
(add-record-unless-exists (make-cd "Achtung, Baby!" "U2" 8 nil ))
(add-record-unless-exists (make-cd "Dangerous" "Michael Jackson" 8 t))
(add-record-unless-exists (make-cd "Automatic for the people" "R.E.M." 9 t))

; (update (where :artist "U2") :rating 11)
; (print (select (where :rating 9 )))

(add-cds)
(dump-db)
