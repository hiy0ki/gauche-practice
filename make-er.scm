;;; 対象のDBの全テーブルのER図を作成する
(use dbi)
(use gauche.parameter)
(use gauche.sequence)

(define *db-name* "dbi:mysql:information_schema;host=localhost")
(define *db-user* "root")
(define *db-pass* "Daijyukirohi21!")
(define db (make-parameter #f))

;; dbからtableのlistを取得する
;; tableのリストからschemaを取得
;; schemaをもとにgraphizのファイル(.dot)を出力する
;; dotファイルからpngファイルを出力する
(define-syntax with-db
  (syntax-rules ()
    ((with-db (db dsn) . body)
     (parameterize
      ((db (dbi-connect dsn :username *db-user* :password *db-pass*)))
      (guard (e (else (dbi-close (db)) (raise e)))
             (begin0
              (begin . body)
              (dbi-close (db))))))))

(define (get-tables)
  (with-db (db *db-name*)
           (let* ((result (dbi-do (db) "select table_name from tables where table_schema = 'schedule'"))
                  (getter (relation-accessor result))
                  (table-name-list (map
                     (lambda (row)
                       (getter row "table_name"))
                     result)))
           table-name-list)))

(define (show-plan)
  (let* ((con (dbi-connect *db-name* :username *db-user* :password *db-pass*))
         (result (dbi-do con "select * from plans")))
    (dbi-close con)
    (dbi-get-value result 0)))
         


(define (main args)
  0)
