;; todo: use a failure continuation or make the failure handling otherwise more systematic
;; todo: should (?) be factored to eval, repl and library handling
;; todo: add lib-http and allow including remote resources
;; todo:  ^ would need a way to sign libraries and/or SSL etc

; used only in ol.scm
(define-library (lang eval)
   (export
      repl-file
      repl-port
      eval-string
      repl-trampoline
      repl
      print-repl-error
      bind-toplevel
      library-import                ; env exps fail-cont → env' | (fail-cont <reason>)
      *src-olvm*
      ; 6.5 Eval
      eval eval-repl
      evaluate
      ;scheme-report-environment null-environment
      interaction-environment)

   (import
      (scheme base)
      (scheme cxr)
      (scheme srfi-0)

      (owl list)
      (lang compile)
      (lang closure)
      (lang cps)
      (lang alpha)
      (owl ff)
      (owl sort)
      (src vm)
      (lang fixedpoint)
      (lang ast)
      (lang env)
      (owl interop)
      (owl time) ;; for testing metadata
      (otus blobs)
      (owl io)
      (owl math)
      (owl list-extra)
      (owl render)
      (owl string)
      (lang sexp)
      (owl parse)
      (owl string)
      (scheme misc)
      (owl lazy)
      (lang macro)
      (lang intern)
      (lang primop)
      (only (owl regex) string->regex))

   (begin

      (define (ok? x) (eq? (ref x 1) 'ok))
      (define (ok exp env) ['ok exp env])
      (define (fail reason) ['fail reason])
      (define (isatty? fd) (syscall 16 fd 19))
      (define (interactive? env) (env-get env '*interactive* #false))

      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      (define (debug env . msg)
         (if (env-get env '*debug* #false)
            (apply print msg)))

      (define (verbose-vm-error opcode a b)
         (list "error" opcode "->"
            (case opcode
               (ARITY-ERROR  ;; arity error, could be variable
                              ; this is either a call, in which case it has an implicit continuation,
                              ; or a return from a function which doesn't have it. it's usually a call,
                              ; so -1 to not count continuation. there is no way to differentiate the
                              ; two, since there are no calls and returns, just jumps.
                  `(function ,a did not accept ,(- b 1) arguments))
               (52 ; (car not-a-pair)
                  `(trying to get ,car of a non-pair ,a))
               (53 ; (cdr not-a-pair)
                  `(trying to get ,cdr of a non-pair ,a))

               ((261 258) ; (not-an-executable-object)
                  `(illegal invocation of ,a))
               (259 ; (ff)
                  `(,a is not a procedure))
               (260 ; (ff not-existent-key)
                  `(key ,b not found in ,a))

               ; ------------------------------------------------------------
               ; syscall errors:
               (62000
                  `(too ,(if (> a b) 'many 'few) arguments given to syscall))

               (62001
                  `(syscall argument ,a is not a port))
               (62002
                  `(syscall argument ,a is not a number))
               (62003
                  `(syscall argument ,a is not a reference))
               (62004
                  `(syscall argument ,a is not a binary sequence))
               (62005
                  `(syscall argument ,a is not a string))
               (62006
                  `(syscall argument ,a is not a string or port))
               (62006
                  `(syscall argument ,a is not a positive number))


               ;; (62000 ; syscall number is not a number
               ;;    `(syscall "> " ,a is not a number))
               ;; ;; 0, read
               ;; (62001 ; too few/many arguments given to
               ;;    `(syscall "> " too ,(if (> a b) 'many 'few) arguments given to))
               ;; (62002 ;
               ;;    `(syscall "> " ,a is not a port))
               (else
                  (if (less? opcode 256)
                     `(,(primop-name opcode) reported error ": " ,a " " ,b)
                     `(,opcode " .. " ,a " " ,b))))))
         ;   ;((eq? opcode 52)
         ;   ;   `(trying to get car of a non-pair ,a))
         ;   (else
         ;      `("error: instruction" ,(primop-name opcode) "reported error: " ,a " " ,b)))

      ;; library (just the value of) containing only special forms, primops and define-syntax macro
      (define *src-olvm*
         (fold
            (λ (env thing)
               (env-set env (ref thing 1) (ref thing 5))) ; add primitives to the end of list
            (env-set-macro
               *special-forms* ;; from (owl env), env with only special form tags, no primops
               'define-syntax
               (make-transformer
                  '(define-syntax syntax-rules add quote)
                  '(
                     ((define-syntax keyword
                        (syntax-rules literals (pattern template) ...))
                      ()
                      (quote syntax-operation add #false
                        (keyword literals (pattern ...)
                        (template ...)))) )))
            *primops*))

      (define (execute exp env)
         (values-apply (exp)
            (lambda vals
               (ok
                  (cond
                     ((null? vals) "no vals")
                     ((null? (cdr vals)) (car vals))
                     (else (cons 'values vals)))
                  env))))

      ; (op exp env) -> #(ok exp' env') | #(fail info)
      (define compiler-passes
         (list
                            ;; macres have already been expanded
            apply-env       ;; apply previous definitions
            sexp->ast       ;; safe sane vectored structure
            fix-points      ;; make recursion explicit <3
            alpha-convert   ;; assign separate symbols to all bound values
            cps             ;; convert to continuation passing style
                            ;; partial eval here?
            build-closures  ;; turn lambdas into closures where necessary
            compile         ;; translate and flatten to bytecode
                            ;;   |
                            ;;   '---> split this into separate passes
                            ;;           + ast->rtl      - refer registers and closures (infinite regs here)
                            ;;           + convert-calls - lots of args -> enlist tail, convert lambdas accordingly
                            ;;           + register value analysis? (car&cdr after known type -> _ref, etc)
                            ;;           + allocate-registers - infinite -> fixed register set
            execute))       ;; call the resulting code

      (define (evaluate exp env)
         (call/cc
            (λ (exit)
               (fold
                  (λ (state next)
                     (if (ok? state)
                        (begin
                           (debug env " * " (ref state 2))
                           (next (ref state 2) (ref state 3)))
                        (exit state)))
                  (ok exp env)
                  compiler-passes))))

      ; run the code in its own thread
      (define (evaluate-as exp env task)
         ; run the compiler chain in a new task
         (fork-linked task
            (λ ()
               (evaluate exp env)))
         ; grab the result
         (case (ref (accept-mail (λ (env) (eq? (ref env 1) task))) 2)
            (['finished result not used]
               result) ; <- is already ok/fail
            (['crashed opcode a b]
               (fail (verbose-vm-error opcode a b)))
            (['error cont reason info]
               ; note, these could easily be made resumable by storing cont
               (fail (list reason info)))
            (['breaked]
               (fail (list "breaked")))
            (else is foo
               (fail (list "Funny result for compiler " foo)))))

      ; ----------------------------------
      ; exp streams reader (with prompt)
      ;; todo: fd->exp-stream could easily keep track of file name and line number to show also those in syntax error messages

      ; rchunks fd block? -> rchunks' end?
      ;; bug: maybe-get-input should now use in-process mail queuing using return-mails interop at the end if necessary
      (define (maybe-get-input rchunks fd block?)
         (let ((chunk (try-get-block fd 1024 #false)))
            ;; handle received input
            (cond
               ((not chunk) ;; read error in port
                  (values rchunks #true))
               ((eq? chunk #true) ;; would block
                  (sleep 5) ;; interact with sleeper thread to let cpu sleep
                  (values rchunks #false))
               ((eof? chunk) ;; normal end if input, no need to call me again
                  (values rchunks #true))
               (else
                  (maybe-get-input (cons chunk rchunks) fd #false)))))

      (define (push-chunks data rchunks)
         (if (null? rchunks)
            data
            (append data
               (foldr append null
                  (map blob->list (reverse rchunks))))))

      ; -> lazy list of parser results, possibly ending to ... (fail <pos> <info> <lst>)

      ; interactive parser (with prompt)
      (define (fd->exp-stream fd prompt parse fail)
         (let loop ((old-data #null) (block? #true) (finished? #false) (display-prompt #true)) ; old-data not successfullt parseable (apart from epsilon)
            (let*((rchunks end?
                     (if finished?
                        (values null #true)
                        (maybe-get-input null fd (or (null? old-data) block?))))
                  (data (if (null? rchunks) old-data (push-chunks old-data rchunks))))

               (if (and display-prompt prompt) (display prompt))

               (if (null? data)
                  (if end? null (loop data #true #false #false))
                  (parse data
                     (λ (data-tail backtrack val pos) ; ok
                        (pair val
                           (if (and finished? (null? data-tail))
                              null
                              (loop data-tail (null? data-tail) end? (null? data-tail)))))
                     (λ (pos info) ; fail
                        (cond
                           (end?
                              ; parse failed and out of data -> must be a parse error, like unterminated string
                              (list (fail pos info data)))
                           ((= pos (length data))
                              ; parse error at eof and not all read -> get more data
                              (loop data #true end? #false))
                           (else
                              (list (fail pos info data)))))
                     0)))))

   ; (parser ll ok fail pos)
   ;      -> (ok ll' fail' val pos)
   ;      -> (fail fail-pos fail-msg')

      (define (file->exp-stream path prompt parser fail)
         (let ((fd (open-input-file path)))
            (if fd
               (fd->exp-stream fd #false parser fail))))


      ;; toplevel variable to which loaded libraries are added

      (define (? x) #true)

      (define definition?
         (let ((pat (list 'setq symbol? ?)))
            (λ (exp) (match pat exp))))

      (define multi-definition?
         (let ((pat (list 'setq list? ?)))
            (λ (exp) (match pat exp))))

      ;; toplevel variable which holds currently loaded (r7rs-style) libraries
      (define libraries-var '*libs*)

      (define error-port stderr)

      (define (print-repl-error lst)
         (define (format-error lst ind)
            (cond
               ((and (pair? lst) (null? (cdr lst)) (list? (car lst)))
                  (cons #\newline
                     (let ((ind (+ ind 2)))
                        (append (map (λ (x) 32) (lrange 0 1 ind))
                           (format-error (car lst) ind)))))
               ((pair? lst)
                  (render (car lst)
                     (cons #\space
                        (format-error (cdr lst) ind))))
               ((null? lst) '( #\newline ))
               (else (render lst '( #\newline )))))
         (write-bytes error-port
            (format-error lst 0)))

      ; -> (ok value env), (error reason env)

      (define repl-op?
         (let ((pattern (list 'unquote symbol?)))
            (λ (exp) (match pattern exp))))

      (define (mark-loaded env path)
         (let ((loaded (env-get env '*loaded* null)))
            (if (mem string-eq? loaded path)
               env
               (env-set env '*loaded*
                  (cons path loaded)))))

      ;; values used by the repl to signal they should be printed as such, not rendered as a value
      (define repl-message-tag "foo")
      (define (repl-message foo) (cons repl-message-tag foo))
      (define (repl-message? foo) (and (pair? foo) (eq? repl-message-tag (car foo))))

      (define (maybe-show-metadata env val)
         (lets
            ((meta (env-get env meta-tag empty))
             (info (getf meta val)))
            (if info
               (begin
                  (display ";; ")
                  (if (list? info)
                     (for-each (λ (x) (display x) (display " ")) info)
                     info)
                  (display "\n")))))

      (define (function->name env function)
         (call/cc (lambda (return)
            (let loop ((kvs (ff-iter env)))
               (cond
                  ((null? kvs) (return "#<function>"))
                  ((pair? kvs)
                     (let ((k (caar kvs))
                           (v (cdar kvs)))
                        (let ((v (ref v 2)))
                           (if (function? v)
                              (if (eq? v function)
                                 (return (symbol->string k)))
                              (let ((v (ref v 2)))
                                 (if (function? v)
                                    (if (eq? v function)
                                       (return (symbol->string k))))))))
                     (loop (cdr kvs)))
                  (else (loop (kvs))))))))
      (define (decode-value env l)
         (cond
            ((function? l)
               (fold string-append "#<" (list (function->name env l) ">")))
            ((list? l)
               (map (lambda (r) (if (not (function? r)) r (decode-value env r))) l))
            (else l)))

      ;; render the value if isatty?, and print as such (or not at all) if it is a repl-message
      ;; if interactive mode and output fails, the error is fatal
      (define (prompt env val)
         (if (interactive? env)
            (if (repl-message? val)
               (begin
                  (if (cdr val)
                     (print (decode-value env (cdr val)))))
                  ;(display "> "))
               (begin
                  (maybe-show-metadata env val)
                  ((writer-to (env-get env name-tag empty))
                     stdout (decode-value env val))
                  (display "\n"))))) ;>


      (define syntax-error-mark (list 'syntax-error))

      ;; fixme: the input data stream is iirc raw bytes, as is parser error position, but that one is unicode-aware

      ; lst -> n, being the number of things before next 10 or end of list
      (define (next-newline-distance lst)
         (let loop ((lst lst) (pos 0))
            (cond
               ((null? lst) (values pos lst))
               ((eq? (car lst) 10) (values (+ pos 1) (cdr lst)))
               (else (loop (cdr lst) (+ pos 1))))))

      (define (find-line data error-pos)
         ;(print " - find-line")
         (let loop ((data data) (pos 0))
            ;(print "data " data " pos " pos  " error-pos " error-pos)
            (lets ((next datap (next-newline-distance data)))
               (cond
                  ((<= error-pos next)
                     (runes->string (take data (- next 1)))) ; take this line
                  ((null? data)
                     "(end of input)")
                  (else
                     (loop datap next))))))

      (define (syntax-fail pos info lst)
         (list syntax-error-mark info
            (list ">>> " (find-line lst pos) " <<<")))

      (define (syntax-error? x) (and (pair? x) (eq? syntax-error-mark (car x))))

      (define (repl-ok env value) ['ok value env])
      (define (repl-fail env reason) ['error reason env])

      ;; just be quiet
      (define repl-load-prompt
         (λ (val result?) null))

      (define (repl-evaluate exp env)
         (evaluate-as exp env 'repl-eval))

      ;; load and save path to *loaded*

      ;; todo: should keep a list of documents *loading* and use that to detect circular loads (and to indent the load msgs)
      (define (repl-load repl path in env)
         (let*((paths (map (lambda (dir) (fold string-append dir `("/" ,path))) (env-get env '*path* '("."))))
               (exps ;; find the file to read
                  (let loop ((paths paths))
                     (unless (null? paths)
                        (or (file->exp-stream (car paths) "" sexp-parser syntax-fail)
                            (loop (cdr paths)))))))
            (if exps
               (let*((interactive (env-get env '*interactive* #false))
                     (load-env    (env-set env '*interactive* #false))
                     (outcome (repl load-env exps)))
                  (case outcome
                     (['ok val env]
                        (repl (mark-loaded (env-set env '*interactive* interactive) path) in))
                     (['error reason partial-env]
                        ; fixme, check that the fd is closed!
                        (repl-fail env (list "Could not load" path "because" reason)))))
               (repl-fail env
                  (list "Could not find any of" paths "for loading.")))))

      ;; regex-fn | string | symbol → regex-fn | #false
      (define (thing->rex thing)
         (cond
            ((function? thing) thing)
            ((string? thing)
               (string->regex
                  (foldr string-append "" (list "m/" thing "/"))))
            ((symbol? thing)
               (thing->rex (symbol->string thing)))
            (else #false)))

      (define repl-ops-help
"Commands:
   ,help             - show this
   ,words            - list all current definitions
   ,expand <expr>    - expand macros in the expression
   ,find [regex|sym] - list all defined words matching regex or m/<sym>/
   ,libraries        - show all currently loaded libraries
   ,l                - || -
   ,quit             - exit owl")

      (define (repl-op repl op in env)
         (case op
            ((help)
               (prompt env repl-ops-help)
               (repl env in))
            ((load)
               (let* ((op in (uncons in #false)))
                  (cond
                     ((symbol? op)
                        (repl-load repl (symbol->string op) in env))
                     ((string? op)
                        (repl-load repl op in env))
                     (else
                        (repl-fail env (list "Not loadable: " op))))))
            ((forget-all-but)
               (lets ((op in (uncons in #false)))
                  (if (and (list? op) (all symbol? op))
                     (let ((nan ['defined ['value 'undefined]]))
                        (repl
                           (env-keep env
                              (λ (name)
                                 (if (or (primop-of name) (has? op name))
                                    name
                                    #false)))
                           ;(ff-fold
                           ;   (λ (env name val)
                           ;      (tuple-case val
                           ;         ((defined x)
                           ;            (cond
                           ;               ((or (primop-of (ref x 2))
                           ;                  (has? op name))
                           ;                  ;(print " + keeping " name)
                           ;                  env)
                           ;               (else
                           ;                  ;(print " - forgetting " name)
                           ;                  (env-del env name))))
                           ;         ;((macro x)
                           ;         ;   (if (has? op name)
                           ;         ;      env
                           ;         ;      (env-del env name)))
                           ;         (else env)))
                           ;   env env)
                           in))
                     (repl-fail env (list "bad word list: " op)))))
            ((words w)
               (prompt env
                  (repl-message
                     (bytes->string
                        (foldr
                           (λ (x tl) (render x (cons #\space tl)))
                           null
                           (cons "Words: "
                              (sort string<?
                                 (map symbol->string
                                    (env-keys env))))))))
               (repl env in))
            ((find)
               (prompt env
                  (let*((thing in (uncons in #false))
                        (rex (thing->rex thing)))
                     (cond
                        ((function? rex)
                           (keep (λ (sym) (rex (symbol->string sym))) (env-keys env)))
                        (else
                           "I would have preferred a regex or a symbol."))))
               (repl env in))
            ((libraries libs l)
               (prompt env
                  (map car (env-get env '*libraries* null)))
               (repl env in))
            ((expand)
               (let*((exp in (uncons in #false)))
                  (case (macro-expand exp env)
                     (['ok exp env]
                        (write exp))
                     (['fail reason]
                        (print "Macro expansion failed: " reason)))
                  (repl env in)))
            ((quit)
               ; this goes to repl-trampoline
               ['ok 'quitter env])
            (else
               (print "unknown repl op: " op)
               ;(prompt env (repl-message #f))
               (repl env in))))

      ;; → (name ...) | #false
      (define (exported-names env lib-name)
         (let ((libp (assoc lib-name (env-get env '*libraries* null))))
            (if libp
               (env-fold (λ (out name value) (cons name out)) null (cdr libp))
               #false)))

      ;; todo: this uses direct environment access - move to lib-env or handle here?
      ;; <export spec> = <identifier>
      ;;               | (rename <identifier_1> <identifier_2>)
      ;;               | (exports <lib)
      ;; TODO - use env-keep and check bindings from result instead to avoid absraction violation
      (define (build-export names env fail)
         (let loop ((names names) (unbound null) (module empty-env))
            (cond
               ((null? names)
                  (cond
                     ((null? unbound) module)
                     ((null? (cdr unbound))
                        (fail (list "Undefined exported value: " (car unbound))))
                     (else
                        (fail (list "Undefined exports: " unbound)))))
               ((env-get-raw env (car names) #false) =>
                  (λ (value)
                     (loop (cdr names) unbound (env-put-raw module (car names) value))))
               ((and ;; swap name for (rename <local> <exported>)
                   (match `(rename ,symbol? ,symbol?) (car names))
                   (env-get-raw env (cadar names) #false)) =>
                  (λ (value)
                     (loop (cdr names) unbound (env-put-raw module (caddar names) value))))
               ((match `(exports ,list?) (car names))
                  (let ((exported (exported-names env (cadr (car names)))))
                     (if exported
                        (loop (append exported (cdr names)) unbound module)
                        (fail (list "Didn't find " (cadar names) " for exporting.")))))
               (else
                  (loop (cdr names) (cons (car names) unbound) module)))))

      ; fixme, use pattern matching...

      (define (symbol-list? l) (and (list? l) (all symbol? l)))

      (define export?
         (let ((pat `(export . ,symbol-list?)))
            (λ (exp) (match pat exp))))

      (define (_ x) #true)

      (define import?  ; toplevel import using the new library system
         (let ((patternp `(import . ,(λ (x) #true))))
            (lambda (exp) (match patternp exp))))

      (define (library-definition? x)
         (and (pair? x) (list? x) (eq? (car x) '_define-library)))

      ;; a simple eval

      ; todo: implement R5RS "6.5 Eval"
      (define (eval exp env)
         (case (macro-expand exp env)
            (['ok exp env]
               (case (evaluate-as exp env (list 'evaluating))
                  (['ok value env]
                     value)
                  (['fail reason]
                     #false)))
            (['fail reason]
               #false)))
;      (define-syntax eval
;         (syntax-rules (*toplevel*)
;            ((eval (exp env))
;               (tuple-case (macro-expand exp env)
;                  ((ok exp env)
;                     (tuple-case (evaluate-as exp env (list 'evaluating))
;                        ((ok value env)
;                           value)
;                        ((fail reason)
;                           #false)))
;                  ((fail reason)
;                     #false)))
;            ((eval (exp))
;               (eval exp *toplevel*))))

      (define-syntax interaction-environment
         (syntax-rules (*toplevel*)
            ((interaction-environment)
               *toplevel*)))

      (define (bind-toplevel env)
         (env-set env '*toplevel*
            (env-del env '*toplevel)))

      ;; list starting with val?
      (define (headed? val exp)
         (and (pair? exp) (eq? val (car exp)) (list? exp)))

      ;; (import <import set> ...)
      ;; <import set> = <library name>
      ;;              | (only <import set> <identifier> ...)
      ;;              | (except <import set> <identifier> ...)
      ;;              | (prefix <import set> <identifier>)
      ;;              | (rename <import set_1> (<identifier_a> <identifier_b>) ..)

      ;; (a ...)
      (define (symbols? exp)
         (and (list? exp) (all symbol? exp)))

      ;; ((a b) ...)
      (define (pairs? exp)
         (and (list? exp)
            (all (λ (x) (and (list? x) (eq? (length x) 2))) exp)))
      (define alist? pairs?)

      ;; → 'ok env | 'needed name | 'circular name, non-ok exists via fail
      (define (import-set->library iset libs fail)
         (cond
            ((assoc iset libs) =>
               (λ (pair)
                  (if (eq? (cdr pair) 'loading) ;; trying to reload something
                     (fail 'circular iset)
                     (values 'ok (cdr pair)))))
            ((match `(only ,? . ,symbols?) iset)
               (lets ((ok lib (import-set->library (cadr iset) libs fail)))
                  (values 'ok
                     (env-keep lib (λ (var) (if (has? (cddr iset) var) var #false))))))
            ((match `(except ,? . ,symbols?) iset)
               (lets ((ok is (import-set->library (cadr iset) libs fail)))
                  (values 'ok
                     (env-keep is (λ (var) (if (has? (cddr iset) var) #false var))))))
            ((match `(rename ,? . ,pairs?) iset)
               (lets ((ok lib (import-set->library (cadr iset) libs fail)))
                  (values 'ok
                     (env-keep lib
                        (λ (var)
                           (let ((val (assq var (cddr iset))))
                              (if val (cdr val) #false)))))))
            ((match `(prefix ,? ,symbol?) iset)
               (lets
                  ((ok lib (import-set->library (cadr iset) libs fail))
                   (prefix (symbol->string (caddr iset))))
                  (values 'ok
                     (env-keep lib
                        (λ (var)
                           (string->symbol
                              (string-append prefix (symbol->string var))))))))
            (else
               (fail 'needed iset))))

      ;; (foo bar baz) → "/foo/bar/baz.scm"
      (define (library-name->path iset)
         (runes->string
            (foldr
               (λ (thing tl)
                  (append
                     (string->list (cond
                                    ((symbol? thing) (symbol->string thing))
                                    ((string? thing) thing)
                                    ((integer? thing) (list->string (render-number thing #null 10)))
                                    (else
                                       (runtime-error "Invalid library name part" thing))))
                     (if (null? tl)
                        (string->list ".scm")
                        (cons #\/ tl))))
            null iset)))


      ;;
      (define (library-file->list env file)
         ;(print-to stderr "library-file->list " file)
         (let ((hook (env-get env 'hook:import #f)))
            (if hook (or
                        (let ((content (hook (c-string file))))
                           (if (string? content) (string->list content) #f))
                        (file->list file))
               (file->list file))))

      ;; try to find and parse contents of <path> and wrap to (begin ...) or call fail
      (define (repl-include env path fail)
         (let*((paths (map
                        (λ (dir) (list->string (append (string->list dir) (cons #\/ (string->list path)))))
                        (env-get env '*path* null)))
;             (_ (print "paths: " paths))
               (data (let loop ((paths paths))
                        (unless (null? paths)
                           (or (library-file->list env (car paths))
                               (loop (cdr paths)))))))

            ;;  (datas (lmap (lambda (file) (library-file->list env file)) paths))
            ;;  (data (first (λ (x) x) datas #false)))
            (if data
               (let ((exps (list->sexps data "library fail" path)))
                  (if exps ;; all of the file parsed to a list of sexps
                     (cons 'begin exps)
                     (fail (list "Failed to parse contents of " path))))
               (fail (list "Couldn't find " path "from any of" (env-get env '*path* null))))))

      ;; nonempty list of symbols or integers
      (define (valid-library-name? x)
         (and (list? x) (pair? x) (all (λ (x) (or (integer? x) (symbol? x))) x)))

      ;; try to load a library based on it's name and current include prefixes if
      ;; it is required by something being loaded and we don't have it yet
      ;; → 'ok x env | 'error x reason | 'not-found x _
      (define (try-autoload env repl iset)
         (if (valid-library-name? iset) ;; (foo bar baz) → try to load "./foo/bar/baz.scm"
            (let
               ((exps
                  (call/cc
                     (λ (ret)
                        (repl-include env
                           (library-name->path iset) (λ (why) (ret #false)))))))
               (if exps
                  (case (repl env (cdr exps)) ; drop begin,
                     (['ok value env]
                        ;; we now have the library if it was defined in the file
                        (values 'ok env))
                     (['error reason env]
                        ;; no way to distinquish errors in the library from missing library atm
                        (values 'error reason)))
                  (values 'not-found (library-name->path iset))))
            (values 'error (list "Bad library name:" iset))))

      (define (any->string obj)
         (list->string (render obj null)))

      (define (library-import env exps fail repl)
         (fold
            (λ (env iset)
               (lets ((status lib (call/cc (λ (ret) (import-set->library iset (env-get env '*libraries* null) ret)))))
                  (cond
                     ((eq? status 'needed)
                        (lets ((status env (try-autoload env repl lib)))
                           (cond
                              ((eq? status 'ok)
                                 (library-import env exps fail repl))
                              ((eq? status 'error)
                                 (fail (list "Failed to load" lib "because" env)))
                              (else
                                 (fail (list "I didn't have or find library" (any->string lib)))))))
                     ((eq? status 'ok)
                        (env-fold env-put-raw env lib)) ;; <- TODO env op, should be in (owl env)
                     ((eq? status 'circular)
                        (fail (list "Circular dependency causing reload of" (bytes->string (render lib null)))))
                     (else
                        (fail (list "BUG: bad library load status: " status))))))
            env exps))

      ;; temporary toplevel import doing what library-import does within libraries
      (define (toplevel-library-import env exps repl)
         (let*/cc ret
            ((fail (λ (x) (ret (cons "Import failed because" x)))))
            (library-import env exps fail repl)))

      (define (match-feature req feats libs fail)
         (cond
            ((memv req feats) #true) ;; a supported implementation feature
            ((symbol? req) #false)
            ((assv req libs) #true) ;; an available (loaded) library
            ((and (headed? 'not req) (eq? (length req) 2))
               (not (match-feature (cadr req) feats libs fail)))
            ((headed? 'and req)
               (all (λ (req) (match-feature req feats libs fail)) (cdr req)))
            ((headed? 'or req)
               (some (λ (req) (match-feature req feats libs fail)) (cdr req)))
            (else
               (fail "Weird feature requirement: " req))))

      (define (choose-branch bs env fail)
         (cond
            ((null? bs) null) ;; nothing matches, no else
            ((match `(else . ,list?) (car bs)) (cdar bs))
            ((pair? (car bs))
               (if (match-feature
                        (caar bs)
                        (env-get env '*features* null) ; list of implementation feature symbols
                        (env-get env '*libraries* null); list of loaded libraries
                        fail)
                  (cdar bs)
                  (choose-branch (cdr bs) env fail)))
            (else
               (fail (list "Funny cond-expand node: " bs)))))


      (define (repl-library exp env repl fail)
         (cond
            ((null? exp) (fail "no export?"))
            ((headed? 'import (car exp))
               (repl-library (cdr exp)
                  (library-import env (cdar exp) fail repl)
                  repl fail))
            ((headed? 'begin (car exp))
               ;; run basic repl on it
               (case (repl env (cdar exp))
                  (['ok value env]
                     ;; continue on to other defines or export
                     (repl-library (cdr exp) env repl fail))
                  (['error reason env]
                     (fail reason))))
            ((headed? 'export (car exp))
               ;; build the export out of current env
               (ok (build-export (cdar exp) env fail) env))
            ((headed? 'include (car exp))
               (repl-library
                  (foldr
                     (λ (path exp) (cons (repl-include env path fail) exp))
                     (cdr exp) (cdar exp))
                  env repl fail))
            ((headed? 'cond-expand (car exp))
               (repl-library
                  (append (choose-branch (cdar exp) env fail) (cdr exp))
                  env repl fail))
            (else
               (fail (list "unknown library term: " (car exp))))))

      ;; variables which are added to *src-olvm* when evaluating libraries
      (define library-exports
         (list
            '*libraries*   ;; loaded libraries
            '*path*        ;; where to try to load includes/libraries from
            '*features*))  ;; implementation features

      ;; update *owl-names* (used by renderer of repl prompt) if the defined value is a function
      (define (maybe-name-function env name value)
         (if (function? value)
            (env-set env name-tag
               (put (env-get env name-tag empty) value name))
            env))

      ;; update *owl-meta* to have some data about this
      (define (maybe-save-metadata env name value)
         (env-set env meta-tag
            (put (env-get env meta-tag empty) value
               `(defined in ,(env-get env current-library-key 'repl)))))

      (define (eval-repl exp env repl evaluator)
         (debug env "Evaling " exp)
         (case (macro-expand exp env)
            (['ok exp env]
               (debug env " * expanded to " exp)
               (cond
                  ((import? exp) ;; <- new library import, temporary version
                     (lets
                        ((envp (toplevel-library-import env (cdr exp) repl)))
                        (if (pair? envp) ;; the error message
                           (fail envp)
                           (ok
                              (repl-message
                                 (list->string
                                    (foldr render null
                                       (cons ";; Imported " (cdr exp)))))
                              envp))))
                  ((definition? exp)
                     (case (evaluator (caddr exp) env)
                        (['ok value env2]
                           (lets
                              ((env (env-set env (cadr exp) value))
                               (env (maybe-name-function env (cadr exp) value))
                               ;(env (maybe-save-metadata env (cadr exp) value))
                               )
                              (ok
                                 (repl-message
                                    (bytes->string (render ";; Defined " (render (cadr exp) null))))
                                 (bind-toplevel env))))
                        (['fail reason]
                           (fail
                              (list "Definition of" (cadr exp) "failed because" reason)))))
                  ((multi-definition? exp)
                     (case (evaluator (caddr exp) env)
                        (['ok value env2]
                           (let ((names (cadr exp)))
                              (if (and (list? value)
                                    (eq? (length value) (length names)))
                                 (ok (repl-message ";; All defined")
                                    (fold
                                       (λ (env pair)
                                          (env-set env (car pair) (cdr pair)))
                                       env
                                       (zip cons names value)))
                                 (fail
                                    (list "Didn't get expected values for definition of " names)))))
                        (['fail reason]
                           (fail
                              (list "Definition of" (cadr exp) "failed because" reason)))))
                  ((export? exp)
                     (lets ((module (build-export (cdr exp) env (λ (x) x)))) ; <- to be removed soon, dummy fail cont
                        (ok module env)))
                  ((library-definition? exp)
                     ;; evaluate libraries in a blank *src-olvm* env (only primops, specials and define-syntax)
                     ;; include just loaded *libraries* and *include-paths* from the current one to share them
                     (let*/cc ret
                        ((exps (map cadr (cdr exp))) ;; drop the quotes
                         (name exps (uncons exps #false))
                         (libs (env-get env '*libraries* null))
                         ;; mark the current library as being loaded for circular dependency detection
                         (env (env-set env '*libraries* (cons (cons name 'loading) libs)))
                         (fail
                           (λ (reason)
                              (ret (fail (list "Library" name "failed:" reason)))))
                         (lib-env
                           (fold
                              (λ (lib-env key) (env-set lib-env key (env-get env key null)))
                              *src-olvm* library-exports))
                         (lib-env (env-set lib-env current-library-key name)))
                        (case (repl-library exps lib-env repl fail) ;; anything else must be incuded explicitly
                           (['ok library lib-env]
                              ;; get new function names and metadata from lib-env (later to be handled differently)
                              (lets
                                 ((names (env-get lib-env name-tag empty))
                                  (env (env-set env name-tag (ff-union (env-get env name-tag empty) names (λ (old new) new))))
                                  (meta (env-get lib-env meta-tag empty))
                                  (env (env-set env meta-tag (ff-union (env-get env meta-tag empty) meta (λ (old new) new)))))
                                 (ok
                                    (repl-message
                                       (list->string
                                          (foldr render null
                                             (list ";; Library " name " added" ))))
                                    (env-set env '*libraries*
                                       (cons (cons name library)
                                          (keep  ;; drop the loading tag for this library
                                             (λ (x) (not (equal? (car x) name)))
                                             (env-get lib-env '*libraries* null))))))) ; <- lib-env may also have just loaded dependency libs
                           (['error reason not-env]
                              (fail
                                 (list "Library" name "failed to load because" reason))))))
                  (else
                     (evaluator exp env))))
            (['fail reason]
               ['fail
                  (list "Macro expansion failed: " reason)])))


      ; !
      ; in this repl changed from (repl env in) to (repl env in evaluator)
      ; (repl env in) -> #(ok value env) | #(error reason env)
      (define (repl env in evaluator)
         (define repl__ (lambda (env in)
                           (repl env in evaluator)))
         (let loop ((env env) (in in) (last #false)) ; last - последний результат
            (cond
               ((null? in)
                  (repl-ok env last))
               ((pair? in)
                  (lets ((this in (uncons in #false)))
                     (cond
                        ((eof? this)
                           (repl-ok env last))
                        ((syntax-error? this)
                           (repl-fail env (cons "This makes no sense: " (cdr this))))
                        ((repl-op? this)
                           (repl-op repl__ (cadr this) in env)) ; todo: add last
                        (else
                           (case (eval-repl this env repl__ evaluator)
                              (['ok result env]
                                 (prompt env result)
                                 (loop env in result))
                              (['fail reason]
                                 (repl-fail env reason)))))))
               (else
                  (loop env (in) last)))))

      (define (repl-port env fd)
         (repl env
            (fd->exp-stream fd (if (and (eq? fd stdin) (interactive? env)) "> ") sexp-parser syntax-fail)
            repl-evaluate))

      (define (repl-file env path)
         (let ((fd (open-input-file path)))
            (if fd
               (repl-port env fd)
               ['error "cannot open file" env])))

      (define (eval-string env str)
         (let ((exps (parse (get-kleene+ sexp-parser) (str-iter str) #false syntax-fail #false)))
            ;; list of sexps
            (if exps
               (repl env exps evaluate)
               ['error "not parseable" env])))

      ;; run the repl on a fresh input stream, report errors and catch exit
      (define (repl-trampoline env in)
         (let boing ((env env))
            (let ((env (bind-toplevel env)))
               (case (repl-port env in)
                  (['ok val env]
                     ;; bye-bye
                     (let ((atexit (env-get env '*atexit* #false)))
                        (if (function? atexit)
                           (atexit)))
                     (if (interactive? env)
                        (print "bye-bye :/"))
                     (halt 0))
                  (['error reason env]
                     (let ((hook:fail (env-get env 'hook:fail #f)))
                        (if hook:fail (hook:fail reason (syscall 1002))))

                     (if (list? reason)
                        (print-repl-error (decode-value env reason)))

                     ; better luck next time
                     (boing env))

                  ; notify hooker about strange error
                  ; is this cannot be reached?
                  (else is foo
                     (print "Repl is rambling: " foo) ; what is this?
                     (boing env))))))
))
