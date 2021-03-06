; http://www.rosettacode.org/wiki/Knuth_shuffle
(define *path* (cons "tests/rosettacode" *path*))
(import (otus random!))

(define items [1 2 3 4 5 6 7 8 9])

(print "vector before: " items)
(define (shuffle tp)
   (let ((items (vm:cast tp (type tp))))
      (for-each (lambda (i)
            (let ((a (ref items i))
                  (j (+ 1 (rand! i))))
               (set-ref! items i (ref items j))
               (set-ref! items j a)))
         (reverse (iota (size items) 1)))
      items))
(print "vector after: " (shuffle items))

(define items (list 1 2 3 4 5 6 7 8 9))
(print "list before: " items)
(define (list-shuffle tp)
   (map (lambda (i)
         (list-ref tp i))
      (vector->list
         (shuffle (list->vector (iota (length tp)))))))

(print "list after: " (list-shuffle items))

(define items "Down, down, down. Would the fall NEVER come to an end!")
(print "string before: " items)
(print "string after: " (runes->string (list-shuffle (string->runes items))))
