(use util.match)
(use gauche.parameter)
(use gauche.sequence)
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
  (let* ((result (dbi-do (db)
                         "select day, plan from plans"))
         (getter (relation-accessor result))
         (plan-list (map
                     (lambda (row)
                       (cons (getter row "day")
                             (getter row "plan")))
                     result)))
    (for-each
     (lambda (p)
       (schedule-print (car p) (cdr p)))
     plan-list)))

(define (schedule-get key)
  (let* ((query (dbi-prepare (db)
                             "select day, plan from plans where day = ?"))
         (result (dbi-execute query key))
         (getter (relation-accessor result))
         (plan-list (map
                     (lambda (row)
                       (cons (getter row "day")
                             (getter row "plan")))
                     result)))
    plan-list))

(define (show-schedule key)
  (let ((plan-list (schedule-get key)))
    (if (pair? plan-list)
        (let ((key (caar plan-list))
              (plan (cdar plan-list)))
          (schedule-print key plan))
        (print ">>>empty<<<"))))

(define (edit-schedule key plan)
  (let ((plan-list (schedule-get key)))
    (cond [(null? plan-list)
           (let ((query (dbi-prepare (db)
                                     "insert into plans (day, plan) values (?, ?)")))
             (dbi-execute query key plan))]
          [(eq? plan 'delete)
           (let ((query (dbi-prepare (db)
                                     "delete from plans where day = ?")))
             (dbi-execute query key))]
          [else
           (let ((query (dbi-prepare (db)
                                     "update plans set plan = ? where day = ?")))
             (dbi-execute query plan key))])))




