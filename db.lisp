#!/usr/bin/ol

(import (lib sqlite))

; для упрощения работы и т.д,  пусть у нас будет одна большая база даных с множеством таблиц
(define database (make-sqlite3)) ; make new database connection
(sqlite3-open (c-string "database.sqlite") database)


; возвращает результат запроса как одну строку, для циклической обработки строк используйте db:for-each
(define (db:query query . args) ; select multiple values
   (let ((statement (make-sqlite3-stmt)))
      (if (less? 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (runtime-error "error query preparation" query))
      (let loop ((n 1) (args args))
         (if (null? args) #true
            (let ((arg (car args)))
               (cond
                  ((integer? arg)
                     ;todo: if > max-int-value use sqlite3_bind_int64
                     (sqlite3-bind-int    statement n arg))
                  ((rational? arg)
                     (sqlite3-bind-double statement n arg))
                  ((string? arg)
                     (sqlite3-bind-text   statement n arg (size arg) #f))
                  (else
                     (runtime-error "Unsupported parameter type" arg)))
               (loop (+ n 1) (cdr args)))))
      (case (sqlite3-step statement)
         (SQLITE-ROW
            statement)
         (SQLITE-DONE
            (sqlite3-finalize statement)
            #false) ; no query results present
         (else
            (print "Error!")
            (sqlite3-finalize statement)
            (runtime-error "Can't execute SQL statement" #t)))))

; циклически обрабатывает все строки - результаты запроса
(define (db:for-each statement f)
   (if statement
   (let loop ()
      (let ((n (sqlite3_column_count statement)))
      ;(print "n: " n)
      (if (less? 0 n) (begin
         (apply f
            (let subloop ((i (- n 1)) (args '()))
               ;(print "args: " args)
               ;(print "sqlite3_column_type statement i: " (sqlite3_column_type statement i))
               ;(print "i: " i)
               ;(print "?: " (< i 0))
               (if (< i 0) args
                  (subloop (- i 1) (cons
                     (case (sqlite3_column_type statement i)
                        (SQLITE-NULL    #false)
                        (SQLITE-INTEGER (sqlite3_column_int statement i))
                        ;(SQLITE-FLOAT   (sqlite3_column_double statement i))
                        (SQLITE-TEXT    (sqlite3_column_text statement i))
                        (else (runtime-error "Unsupported column type " i)))
                     args)))))
         ;(print "--------------------")
         (case (sqlite3-step statement)
            (SQLITE-ROW
               (loop))
            (SQLITE-DONE
               (sqlite3-finalize statement)
               #false)
            (else
               (sqlite3-finalize statement)
               (runtime-error "Can't execute SQL statement" #t)))))))))

(define (db:map statement f)
   (if statement
   (reverse
   (let loop ((t null))
      (let ((n (sqlite3_column_count statement)))
      ;(print "n: " n)
      (if (less? 0 n) ; we already have result
         (let ((v (apply f
                     (let subloop ((i (- n 1)) (args '()))
                        (if (< i 0) args
                           (subloop (- i 1) (cons
                              (case (sqlite3_column_type statement i)
                                 (SQLITE-NULL    #false)
                                 (SQLITE-INTEGER (sqlite3_column_int statement i))
                                 ;(SQLITE-FLOAT   (sqlite3_column_double statement i))
                                 (SQLITE-TEXT    (sqlite3_column_text statement i))
                                 (else (runtime-error "Unsupported column type " i)))
                              args)))))))
            ; выберем следующую строку
            (case (sqlite3-step statement)
               (SQLITE-ROW
                  (loop (cons v t)))
               (SQLITE-DONE
                  (sqlite3-finalize statement)
                  (cons v t))
               (else
                  (sqlite3-finalize statement)
                  (runtime-error "Can't execute SQL statement" #t))))))))
   '()))


; возвращает только одно значение из запроса (если было запрошено одно, иначе целую строку)
(define (db:value query . args) ; select only one value
   (let ((statement (make-sqlite3-stmt)))
      (if (less? 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (runtime-error "error query preparation" query))
      (let loop ((n 1) (args args))
         (if (null? args) #true
            (let ((arg (car args)))
               (cond
                  ((integer? arg)
                     ;todo: if > max-int-value use sqlite3_bind_int64
                     (sqlite3-bind-int    statement n arg))
                  ((rational? arg)
                     (sqlite3-bind-double statement n arg))
                  ((string? arg)
                     (sqlite3-bind-text   statement n arg (size arg) #f))
                  (else
                     (runtime-error "Unsupported parameter type" arg)))
               (loop (+ n 1) (cdr args)))))
      (case (sqlite3-step statement)
         (SQLITE-DONE
            (let ((changes (sqlite3_changes database))
                  (result (sqlite3-last-insert-rowid database)))
               (sqlite3-finalize statement)
               (print "changes: " changes)
               (if (eq? changes 0)
                  #false
                  result)))
         (SQLITE-ROW
            (let ((n (sqlite3_column_count statement)))
            ;(print "n: " n)
            (if (less? 0 n)
               (let ((result
                        (let subloop ((i (- n 1)) (args '()))
                           ;(print "args: " args)
                           ;(print "sqlite3_column_type statement i: " (sqlite3_column_type statement i))
                           ;(print "i: " i)
                           ;(print "?: " (< i 0))
                           (if (< i 0) args
                              (subloop (- i 1) (cons
                                 (case (sqlite3_column_type statement i)
                                    (SQLITE-NULL    #false)
                                    (SQLITE-INTEGER (sqlite3_column_int statement i))
                                    ;(SQLITE-FLOAT   (sqlite3_column_double statement i))
                                    (SQLITE-TEXT    (sqlite3_column_text statement i))
                                    (else (runtime-error "Unsupported column type " i)))
                                 args))))))
                  (sqlite3-finalize statement)
                  (if (eq? n 1)
                     (car result)
                     result)))))
         (else
            (sqlite3-finalize statement)
            (runtime-error "Can't execute SQL statement" #t)))))

;(define-syntax db:transaction
;(syntax-rules ()
;   ((db:transaction .rest)
;      (begin
;         (db:value "BEGIN")
;         .rest
;         (db:value "COMMIT")))))


; применяет запрос к функции
(define (db:apply statement f)
   (apply f statement))

; tests:
;(print
;(db:value "UPDATE accounts SET key=? WHERE username=? AND password=?" "333" "user#1" "1234567"))
