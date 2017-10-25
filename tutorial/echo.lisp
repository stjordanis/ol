; echo server

(define (create-socket) (syscall 41 #f #f #f))
(define (bind socket port) (syscall 49 socket port #f))
(define (listen socket) (syscall 50 socket #f #f))
(define (select socket) (syscall 23 socket #f #f))
(define (accept socket) (syscall 43 socket #f #f))
(define (timestamp) (syscall 201 "%c" #f #f))
(define (sendfile in out size) (syscall 40 in out size))


(define (on-accept fd)
(lambda ()
   (let loop ((
   (sendfile stdin stdout 999)))

(define (echo:run port)
(let ((socket (create-socket)))
   ; bind
   (if (not (bind socket port)) ; bind
      (runtime-error "Can't bind to port" port))
   ; listen
   (if (not (listen socket)) ; listen
      (runtime-error "Can't listen socket" #f))

   ; accept
   (let loop ()
      (if (select socket) ; select
         (let ((fd (accept socket))) ; accept
            (print "\n# " (timestamp) ": new request from " (syscall 51 fd #f #f))
            (fork (on-accept fd))))
      (set-ticker-value 0)
      (loop))))

(echo:run 12321)