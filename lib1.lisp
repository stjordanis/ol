(define (string-split string delimiter)
(let ((n (string-length string)))
   (let loop ((i 0) (p 1) (tail '()))
      (if (eq? p n)
         (reverse (cons (substring string i p) tail))
         (if (eq? (string-ref string p) delimiter)
            (loop (+ p 1) (+ p 2) (cons (substring string i p) tail))
            (loop i (+ p 1) tail))))))

(define has-two-dots? (string->regex "m/\\.\\./"))

; syscalls
(define (yield) (syscall 1022 0 #false #false))
(define (time format seconds) (syscall 201 format seconds #f))
(define uname (syscall 63 0 0 0))

(define (starts-with string sub)
   (if (> (string-length sub) (string-length string))
      #false
      (string-eq? (substring string 0 (string-length sub)) sub)))

(define (str-find str char)
   (let loop ((string (str-iter str)) (n 0))
      (if (null? string)
         -1
         (if (char=? (car string) char)
            n
            (loop (force (cdr string)) (+ n 1))))))

;(define (exec filename args fds)
;   (syscall 59 (c-string filename)
;      (map c-string args) fds))
(define (concat . args)
   (foldr str-app "" args))


; URL decoding:

(import (lang sexp))
(define (string->number str base)
   (list->number (string->list str) base))

; подготовим табличку для перевода из hex в dec
(define hex-table
   (list->ff (fold append '() (list
      (zip cons (iota 10 #\0) (iota 10 0))     ; 0-9
      (zip cons (iota  6 #\a) (iota 6 10))     ; a-f
      (zip cons (iota  6 #\A) (iota 6 10)))))) ; A-F
;      (zip cons (lrange 48 1 58)  (lrange  0 1 10))     ; 0-9 todo: change to iota
;      (zip cons (lrange 97 1 103) (lrange 10 1 16))     ; a-f
;      (zip cons (lrange 65 1 71)  (lrange 10 1 16)))))) ; A-F

; valid characters for url is $-_.+!*'(),;
; если в параметре запроса стоит апостроф, то этот параметр обязан быть строковый!
(define (split-url url)
(let ((url (string->bytes url)))
   (let loop ((u url) (t '()) (args '()) (numeric #t))
      (if (null? u)
         (reverse (cons (if numeric
                           (list->number (reverse t) 10)
                           (bytes->string (reverse t)))
                        args))
      (let ((c (car u)))
         (cond
            ((eq? c #\/)
               (loop (cdr u) '() (cons (if numeric
                                          (list->number (reverse t) 10)
                                          (bytes->string (reverse t)))
                                       args) #true))
            ((eq? c #\%)
               (loop (cdddr u)
                  (cons (bor
                           (<< (get hex-table (cadr u) 0) 4)
                               (get hex-table (caddr u) 0)) t)
                  args
                  #false))
            ((eq? c #\')
               (loop (cdr u) t args #false))
            ((<= #\0 c #\9)
               (loop (cdr u) (cons c t) args numeric))
            (else
               (loop (cdr u) (cons c t) args #false))))))))
