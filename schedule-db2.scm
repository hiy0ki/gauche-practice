(use srfi-1)
(use util.match)
(use dbm.fsdbm)
(use gauche.parameter)


(define *db-name* "./schedule")

(dbm-open <fsdbm> :path *db-name* :rw-mode :write)

(define (schedule-write schedule-data)
  (receive (out tempfile)
           (sys-mkstemp "schedule")
           (write schedule-data out)
           (close-output-port out)
           (sys-rename tempfile *db-name*)))

(define (initialize)
  (let ((db (dbm-open <fsdbm>
                      :path *db-name*
                      :rw-mode :create)))
    (dbm-close db)))

(define (schedule . args)
  (match args
         [() (list-schedule)]
         [(day) (show-schedule day)]
         [(day plan) (edit-schedule day plan)]
         [_ (display ">>>error<<<")]))

(define (schedule-print day plan)
  (print #`",|day|: ,|plan|"))

(define (list-schedule)
  (let ((db (dbm-open <fsdbm>
                      :path *db-name*
                      :rw-mode :read)))
    (dbm-for-each db schedule-print)
    (dbm-close db)))

(define (schedule-find day)
  (call-with-input-file *db-name*
    (lambda (in)
      (let* ((schedule-data (read in))
             (item (assoc day schedule-data)))
        (values item schedule-data)))))

(define (show-schedule day)
  (let* ((db (dbm-open <fsdbm>
                       :path *db-name*
                       :rw-mode :read))
         (plan (dbm-get db day #f)))
    (if plan
        (schedule-print day plan)
        (print ">>>empty<<<"))
    (dbm-close db)))

(define (edit-schedule day plan)
  (let ((db (dbm-open <fsdbm>
                      :path *db-name*
                      :rw-mode :write)))
    (if (eq? plan 'delete)
        (dbm-delete! db day)
        (dbm-put! db day plan))
    (dbm-close db)))

;; parameter sample
(define x (make-parameter 20))



