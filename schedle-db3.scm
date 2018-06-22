(use util.match)
(use gauche.parameter)
(use dbi)

(define *db-name* "dbi:mysql:schedule;host=localhost")
(define *db-user* "")
(define *db-pass* "")

(define db (make-parameter #f))

(define-syntax with-db
  (syntax-rules ()
    ((with-db (db dsn) . body)
     (parameterize
      ((db (dbi-connect dsn :username *db-user* :password *db-pass*)))
      (guard (e (else (dbi-close (db)) (raise e)))
             (begin0
              (begin . body)
              (dbi-close (db))))))))

(define (initialize)
  (with-db (db *db-name*)
           (dbi-do (db)
                   "drop table if exists plans")
           (dbi-do (db)
                   "create table plans (day DATE, plan varchar(255), primary key (day))")))

(define (schedule . args)
  (with-db (db *db-name*)
           (match args
                  [() (list-schedule)]
                  [(day) (show-schedule day)]
                  [(day plan) (edit-schedule day plan)]
                  [_ (display ">>>error<<<")])))

(define (schedule-print day plan)
  (print #`",|day|: ,|plan|"))

(define (list-schedule)
  (dbm-for-each (db) schedule-print))

(define (show-schedule day)
  (let ((plan (dbm-get (db) day #f)))
    (if plan
        (schedule-print day plan)
        (print ">>>empty<<<"))))

(define (edit-schedule day plan)
  (if (eq? plan 'delete)
      (dbm-delete! (db) day)
      (dbm-put! (db) day plan)))





