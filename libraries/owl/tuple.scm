(define-library (owl tuple)

   (export tuple?
      list->tuple tuple->list
      read-tuple)

   (import
      (scheme core)
      (owl list-extra)
      (owl list) 
      (owl math))

   (begin
      (define (tuple? x) 
         (eq? (type x) type-vector))

      (define (list->tuple lst)
         (vm:make type-vector lst))

      (define (read-tuple tuple pos lst)
         (if (= pos 0)
            lst  
            (read-tuple tuple (- pos 1)
               (cons (ref tuple pos) lst))))

      (define (tuple->list tuple)
         (read-tuple tuple (size tuple) null))))

