;;; �оݤ�DB�����ơ��֥��ER�ޤ��������
(use dbi)
(use gauche.parameter)
(use gauche.sequence)

(define *db-name* "dbi:mysql:information_schema;host=localhost")
(define *db-user* "root")
(define *db-pass* "Daijyukirohi21!")
(define db (make-parameter #f))

;; db����table��list���������
;; table�Υꥹ�Ȥ���schema�����
;; schema���Ȥ�graphiz�Υե�����(.dot)����Ϥ���
;; dot�ե����뤫��png�ե��������Ϥ���
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
