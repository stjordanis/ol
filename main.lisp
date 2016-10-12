#!/usr/bin/ol
;(define *interactive* #t)
;(define *debug* #t)

(import (lib http))
;(import (lib html))

,load "db.lisp"
,load "lib1.lisp"
,load "html.lisp"

(define (ne? . args)
   (not (apply eq? args)))

(define unurl (string->regex "s/+/ /g"))

; ==============================================================
; набор общей игровой математики
(define (get-advantage-points-left prt lrts)
   (+ prt lrts))

; пара мелкий упрощений синтаксиса
(define-syntax li-
   (syntax-rules ()
      ((li- rest...)
         (list
            "<li style='clear:both'>"
              rest...
            "</li>"))))



; todo: сделать такой string-split
;       который, если в строке есть сумволы кроме чисел
;       возвращает url-decoded строки, в ином случае - числа через (string->number)
; а вообще, просто добавить сигнатуру вызова и автоматическую его парсилку


(define-syntax actions
   (syntax-rules (else action account simple al string-split string-split-with-auth close)
      ((actions) #false)
      ((actions (else exp . rest))
         ((lambda () exp . rest)))        ; (begin ...)
      ((actions (simple url (args...) .body) .rest)
         (if (starts-with al url)
            (apply (lambda (args...)
                     .body)
               (cddr (split-url al)))
            (actions .rest)))
      ((actions (action url (args...) .body) .rest)
         (if (starts-with al url)
            (apply (lambda (account args...)
                     .body)
               (string-split-with-auth al))
            (actions .rest)))
      ((actions (clause exp . rest-exps) .rest)
         (if clause
            ((lambda () exp . rest-exps)) ; (begin ...)
            (actions . rest)))))


; todo: please,  return sendfile

; todo: 401 Unauthorized
; todo: 403 Forbidden
;(define (send-404 fd)
;(let*((send (lambda args
;         (for-each (lambda (arg)
;            (display-to fd arg)) args))))
;   (print "Sending error")
;   (print "Sending 404 Not Found")
;
;   (send "HTTP/1.0 404 Not Found\n")
;   (send "Connection: close\n"
;         "Content-Type: text/html\n"
;         "Server: " (car *version*) "/" (cdr *version*) "\n\n")
;   (send "<HTML><BODY>"
;         "<h1>404 Not Found OK</h1>")))

;(define (sendfile fd content-type filename)
;   (print "Sending as '" content-type "' " filename)
;(let*((path (if (string? filename) (str-app "." (c-string filename)) "?"))
;      (send (lambda args
;         (for-each (lambda (arg)
;            (display-to fd arg)) args)))
;      (stat (syscall 4 path #f #f)))
;   (if stat (begin
;      (print "Sending 200 OK, file size is " (ref stat 8) ", name is " path)
;      (send "HTTP/1.0 200 OK\n"
;            ;"Connection: close\n"
;            "Content-Type: " content-type "\n"
;            "Content-Length: " (ref stat 8) "\n"
;            "Server: " (car *version*) "/" (cdr *version*) "\n\n")
;      (write-vector (file->vector path) fd)
;      (print "File sent."))
;   ;else
;   (begin
;      (print "Sending 404 Not Found")
;      (send "HTTP/1.0 404 Not Found\n"
;            ;"Connection: close\n"
;            "Content-Type: text/html\n"
;            "Server: " (car *version*) "/" (cdr *version*) "\n\n")
;      (send "<HTML><BODY>"
;            "<h1>404 Not Found OK</h1>"
;            "<h4>url: " filename "</h4>")))))

; ===================================================
(http:run 8080 (lambda (fd request headers send close)
   (print "Request: " request)
;  (print "Headers: " headers)

   (define (respond status-code)
      (print "Sending " status-code)
      (send "HTTP/1.0 " status-code "\n")
      (send "Connection: close\n"
            "Content-Type: text/html\n"
            "Server: " (car *version*) "/" (cdr *version*) "\n"
            "\n") #true)
   (define (send-200) (respond "200 OK"))
   (define (send-204) (respond "204 No Content"))
   (define (send-400) (respond "400 Bad Request"))
   (define (send-401) (respond "401 Unauthorized"))
   (define (send-404) (respond "404 Not Found"))
   (define (send-405) (respond "405 Method Not Allowed"))

   ; эта функция обходит дерево рекурсивно и отправляет его в выходной поток
   ; null и #false в выходной поток НЕ отправляются!
   (define (html . args)
      (let html ((args args))
         (for-each (lambda (arg)
            (if arg
               (if (list? arg)
                  (html arg)
                  (if (not (null? arg))
                     (display-to fd arg)))))
            args)))

   ; todo: refactor
;  (define (old-send-200 title . extras)
;     (send "HTTP/1.0 200 OK\n"
;           "Connection: close\n"
;           "Content-Type: text/html\n"
;           "Server: " (car *version*) "/" (cdr *version*) "\n"
;           "\n")
;     (send "<!DOCTYPE html>"
;           "<html>"
;           "<head>"
;           "   <title>" title "</title>"
;           "   <link rel='stylesheet' href='/stylesheets/normalize.css'>"
;           "   <script src='/javascripts/jquery-2.1.1.min.js'> </script>")
;     (for-each send extras)
;     (send "</head><body>")
;     #true)
;  (define (send-end)
;     (send "</body></html>"))
   (define (sendfile content-type filename)
      (print "Sending as '" content-type "' " filename)
      (if (has-two-dots? filename)
         (send-204) ; нельзя выходить за границу папки
         ; else
         (let*((path (str-app "." (c-string filename)))
               (stat (syscall 4 path #f #f)))
            (if stat (begin
               (print "Sending 200 OK, file size is " (ref stat 8) ", name is " path)
               (send "HTTP/1.0 200 OK\n"
                     "Connection: close\n"
                     "Content-Type: " content-type "\n"
                     "Content-Length: " (ref stat 8) "\n"
                     "Server: " (car *version*) "/" (cdr *version*) "\n"
                     "\n")
               (write-vector (file->vector path) fd)
               (print "File sent."))
            ;else
            (begin
               (print "Sending 404 Not Found, file name is " path)
               (send "HTTP/1.0 404 Not Found\n"
                     "Connection: close\n"
                     "Content-Type: text/html\n"
                     "Server: " (car *version*) "/" (cdr *version*) "\n"
                     "\n")
               (send "<html><body>"
                     "<h1>404 Not Found</h1>"
                     "</body></html>"))))))
   ; %->%25, etc.

; парсит, проверяя аутентификацию пользователя
   (define (string-split-with-auth url)
      (let ((args (cdr (split-url url))))
      (let ((account (db:value "SELECT id FROM accounts WHERE session = ? AND remote_address = ?"
                        (list-ref args 1) ;session
                        (car (syscall 51 fd #f #f))))) ;remote peer address
         (print "account: " account ", from: " (car (syscall 51 fd #f #f)) ", args: " args)
         (if (not account)
            (close (send-401))) ; todo: return only args, change apply to case-lambda

         (cons account (cdr args)))))

   ; обработчик запросов
   (cond
      ((string-eq? (ref request 1) "GET")
         (let*((al (ref request 2))) ; al - address line
            (actions
               ; static web content:
               ((starts-with al "/javascripts/")
                  (sendfile "application/javascript" al))
               ((starts-with al "/stylesheets/")
                  (sendfile "text/css" al))

               ; -----------------------------------
               ; обработка логина пользователя:
               ((or  (string-eq? al "/")
                     (string-eq? al "/index.html")
                     (string-eq? al "/login.html"))
                  (send-200)
                  (head "SIMPLE login")
                  (body
                     (section (class "loginform cf")
                        ; http://www.scriptol.com/html5/forms.php
                        (form (id "login")
                              (action "/login")
                           (ul
                              (li (label "Email")
                                  (input (id "usermail") (type "usermail")
                                         (placeholder "yourname@mail.com")
                                         (required)))
                              (li (label "Password")
                                  (input (id "password") (type "password")
                                         (placeholder "strong passphrase")
                                         (required)))
                              (li-
                                  (input (type "submit") (value "Login"))))))
                     (script
                        "$('#login').submit(function(event) {
                           goto( login.action,
                                 login.elements.usermail.value,
                                 login.elements.password.value);
                           event.preventDefault();
                        });")))

               ; временное решение вопроса логина
               (simple "/login/" (username password)
                  (send-200)

                  (head "Logging in...")
                  (body
                     (let ((remote_address (car (syscall 51 fd #f #f)))
                           (session (db:value "SELECT lower(hex(randomblob(16)))"))) ; сеансовый ключ
                     (if (db:value "UPDATE accounts SET session=?, remote_address=? WHERE name=? AND password=?"
                                    session remote_address username password)
                        ; ok
                        (script "goto('/home/"session"')")
                        ; error
                        (section (class "loginform cf")
                           (form (id "sorry")
                                 (action "/")
                                 (onsubmit "window.location = '/'; return false")
                              (ul
                                 (li (label "Sorry, name or password is invalid."))
                                 (li (a (href "/") "Please, try again") "."))))))))


               ; ===============================================================
               ; домашний экран пользователя
               (action "/home/" (session)
                  (send-200)
                  ; ok, все проверки закончены
                  (head "Home"
                     (stylesheet "/stylesheets/table.css"))
                  (body

                  "<br>"
                  "Hello " (db:value "SELECT name FROM accounts WHERE id = ?" account)
                  "<br><hr><br>"

                  ; в эту табличку мы поместим все расы игрока и играемые ними игры:
                  (table
                     (tr (th "Race") (th "Game"))

                     (db:map (db:query
                        "SELECT  id,name  FROM races WHERE account=?" account)
                        (lambda (id name) (tr
                           (td
                              (a (href "/race/"session"/"id) name))
                           (td
                              (table
                                 (db:map (db:query
                                    "SELECT  id,name,state  FROM games WHERE id IN (
                                       SELECT DISTINCT game FROM game_players WHERE race=?
                                     )" id)
                                    (lambda (id name state)
                                       (tr
                                          (td id) (td name)
                                          (td (a (href "/game/"session"/"id)
                                                (if (eq? state 0)
                                                   "(edit)"
                                                   "(view)")))
                                          (td
                                             (case state ; todo: make new query to check the current race state, not common
                                                (0 "New one")
                                                (1 "Waiting for players")
                                                (2 "Wait for turns")
                                                (3 "Thinking...")
                                                (4 "Closed")
                                                (else "Unknown"))))))
                                 (tr
                                    (td (colspan 4) (style "text-align:center")
                                       (a (href "/create-new-game/"session"/"id) "(create new game)")))
                                 )))))
                     (tr (td (colspan 2) (style "text-align:center")
                        (a (href "/create-new-race/"session) "(create new race)")))
                  )))

               ; =============================================
               ; race
               ;
               (action "/create-new-race/" (session)
                  (send-200)

                  (head "create-new-race")
                  (body
                     (a (href "/home/"session)
                        "Home.") (br)

                     "Creating new race... "
                     (let ((race (db:value "INSERT INTO races (name, account) VALUES (?,?)" "new race" account)))
                        (if race
                           (list "Ok. You can " (a (id "redirect")
                                                   (href "/race/"session"/"race)
                                                   "customize it")
                                 "." (br)
                                 (script "redirect()"))
                           (list "Something wrong :/")))))

               ; update
               (action "/update-the-race/" (session race name prt lrts)
                  (send-200)

                  (head "update-the-race")
                  (body
                     (a (href "/home/"session)
                        "Home.") (br)

                     "Updating the race... "

                     (if (db:value "UPDATE races SET name=?,prt=?,lrts=? WHERE id=? AND account=?"
                                    name prt lrts  race account)
                        (list "Ok. You can " (a (id "redirect")
                                                (href "/race/"session"/"race)
                                                "customize it")
                              "." (br)
                              (script "redirect()"))
                        (list "Something wrong :/"))))

               ; ---------------------------------------
               (action "/race/" (session race)
                  (send-200)

                  (head "Race"
                     (javascript "/javascripts/race.js"))

                  (body
                     (a (href "/home/"session)
                        "Home.") (br)

                     (section (class "loginform cf")
                        (form (id "race")
                              (action "/update-the-race/"session"/"race)
                           (ul

                     ; а теперь просто вывести весь список текущих параметров:
                     ; http://wiki.starsautohost.org/wiki/Race_Design
                     (db:apply (db:value
                        "SELECT  name,prt,lrts FROM races WHERE id=?" race)
                        (lambda (name prt lrts) (span
                           (fieldset ""
                              (li (label "Name"))
                              (li-
                                 (input (id "name") (required)
                                    (value name)))
                              (li
                                 "Adv. Points Left" " : "
                                 (span (id "apl")
                                       (get-advantage-points-left prt lrts))))

                           ; Первичные особенности расы
                           (fieldset "Primary Racial Trait" (ul
                              (map (lambda (name)
                                       (li (input (type "radio") (name "prt") name)))
                                 (list "Jack Of All Trades"     ; JoAT 0 мастер на все руки: хорошо сбалансированная раса
                                       "Hyper Expansion"        ; HE   1 гиперэкспансия: дешевые быстрые колонисты, плодовитость, уменьшение максимума населения;
                                       "Space Demolition"       ; SD   2 минирование: дешевые и совершенные минные поля
                                       "Super Stealth"          ; SS   3 скрытность: корабли труднее обнаружить, можно воровать ресурсы у соперников
                                       "Packet Physics"         ; PP   4 пакеты минералов обретают множество полезных свойств (todo: наверное, стоит исключить из игры ради баланса?)
                                       "War Monger"             ; WM   5 нападение: мощное оружие, недоступны минные поля и планетарная защита
                                       "Interstellar Traveller" ; IT   6 продвинутые звездные врата
                                       "Claim Adjuster"         ; CA   7 приспособление: автоматический терраформинг при колонизации, среда спонтанно улучшается
                                       "Alternate Reality"      ; AR   8 раса живёт на орбите внутри планетарной базы, производство работает по-другому, при уничтожении базы население гибнет
                                       "Inner Strength"))))     ; IS   9 защита: совершенные средства защиты, оружие дороже и некоторые виды недоступны, корабли быстрее ремонтируются, жители планет успешнее противостоят десанту
                           (script "prt("prt")")

                           ; Вторичные особенности расы
                           (fieldset "Lesser Racial Traits" (ul
                              (map (lambda (name)
                                       (li (input (type "checkbox") (name "lrts") name)))
                                 (list "Improved Fuel Efficiency" ;+ IFE     1
                                       "No Ramscoop Engines"      ;- NRSE    2
                                       "Total Terraforming"       ;+ TT      4
                                       "Cheap Engines"            ;- CE      8
                                       "Advanced Remote Mining"   ;+ ARM    16
                                       "Only Basic Remote Mining" ;- OBRM   32
                                       "Improved Starbases"       ;+ ISB    64
                                       "No Advanced Scanners"     ;- NAS   128
                                       "Generalized Research"     ;+ GR    256
                                       "Low Starting Population"  ;- LSP   512
                                       "Ultimate Recycling"       ;+ UR   1024
                                       "Bleeding Edge Technology" ;- BET  2048
                                       "Mineral Alchemy"          ;+ MA   4096
                                       "Regenerating Shields")))) ;- RS   8192
                           (script "lrts("lrts")")
                     )))

                     ; todo: на submit показывать попап окошко, и в нем делать операцию
                     (li-
                        (input (type "submit") (value "Submit changes")))
                     (li
                        (a (href "/home/"session) "Cancel changes"))

                     (script
                        "$('#race').submit(function(event) {
                           goto( race.action,
                                 race.elements.name.value,
                                 prt(), lrts());
                           event.preventDefault();
                        });"
                        ; обновление очков "advantage points left"
                        "$('#race input, #race select').change(function(event) {
                           $.ajax({ url: '/get-advantage-points-left/' + prt() + '/' + lrts(), success: function(result) {
                              $('#apl').html(result);
                           }});
                        });")))

                  )))

               ; =============================================
               ; game
               ;
               (action "/create-new-game/" (session race)
                  (send-200)

                  (head "create-new-game")
                  (body
                     (a (href "/home/"session)
                        "Home.") (br)

                     "Creating new game... "
                     (begin
                        (db:value "BEGIN")
                        (let ((game
                           (db:value "INSERT INTO games (name, account) VALUES (?,?)" "new game" account)))
                           (db:value "INSERT INTO game_players (game, race) VALUES (?,?)" game race)

                           ; пакетное внесение изменений в БД:
                           (if (db:value "COMMIT")
                                 (list "Ok. You can " (a (id "redirect")
                                                         (href "/game/"session"/"game)
                                                         "customize it")
                                       "." (br)
                                       (script "redirect()"))
                                 (list "Something wrong :/"))))))
               ; -------------------------------------
               (action "/game/" (session game)
                  (send-200)

                  (head "Game"
                     (javascript "/javascripts/game.js"))
                  (body
                     (a (href "/home/"session)
                        "Home.") (br)

                     (section (class "loginform cf")
                        (form (id "game")
                              (action "/update-the-game/"session"/"game)
                           (ul

                     ; todo: если state не 0, то запретить редактирование размера мира
                     ; todo: если игра уже опубликована, то вывести список игроков, принимающих участие в ней

                     ; http://www.elite-games.ru/stars/game_types.shtml
                     ; игра создается в несколько этапов. пока, для упрощения логики, будет так
                     ; 1. выбрать размер доски и название игры, выбрать правила
                     ; 2. опубликовать игру (после чего правила уже нельзя менять)
                     ;    но можно игру "распубликовать" и поменять правила?
                     ; 3. ждать игроков
                     ; 4. играть
                     (db:apply (db:value
                        "SELECT  account,name,password,state,board_size,worlds_density,players_adjacency,options FROM games WHERE id=?" game)
                        (lambda (game_ac name password state board-size worlds-density players-adjacency options)
                        (list
                           (fieldset "Game"
                              (li (label "Name")
                                 (input (id "name") (required)
                                    (value name)))
                              (li (label "Password")
                                 (input (id "password")
                                    (value ""))))

                           ; Размер доски
                           ; www.elite-games.ru/stars/doc/galaxy.shtml
                           (fieldset "Board Size"
                              (map (lambda (arg)
                                       (li (input (type "radio") (name "board_size") arg)))
                                 '("Tiny"    "Small"
                                   "Medium"  "Large"
                                   "Huge")))
                           (script "board_size("board-size")")

                           ; плотность размещения планет
                           (fieldset "Density"
                              (map (lambda (arg)
                                       (li (input (type "radio") (name "worlds_density") arg)))
                                 '("Sparse"  "Normal"
                                   "Dense"   "Packed")))
                           (script "worlds_density("worlds-density")")

                           ; как близко размещены игроки
                           (fieldset "Player Adjacency"
                              (map (lambda (arg)
                                       (li (input (type "radio") (name "players_adjacency") arg)))
                                 (list "Close"    "Moderate"
                                       "Farther"  "Distant")))
                           (script "players_adjacency("players-adjacency")")

                           ; игровые флаги
                           (fieldset "Game Options"
                              (map (lambda (arg)
                                       (li (input (type "checkbox") (name "options") arg)))
                                 (list "Beginner: Maximum Minerals"      ;
                                       "Slower Tech Advances"            ;
                                       "Accelerated BBS Play"            ;
                                       "No Random Events"                ;
                                       "Computer Players Form Alliances" ;
                                       "Public Player Scores"            ;
                                       "Galaxy Clumping")))              ;
                           (script "options("options")")

                           (case state
                           (0 ; создается, можно редактировать все
                              (list
                                 (li (style "width:100%")
                                    (input (type "checkbox") (name "publish")
                                       "I'm ready. Please, publish this board for other players."))
                                 (li
                                    (input (type "submit")
                                       (value "Submit changes")
                                       (script "$('#game').submit(function(event) {
                                                   goto( '/update-the-game/"session"/"game"',
                                                         game.elements.name.value,
                                                         game.elements.publish.checked ? 1 : 0,
                                                         board_size(),
                                                         worlds_density(),
                                                         players_adjacency(),
                                                         options()
                                                         );
                                                   event.preventDefault();
                                                });")))))
                           (1 ; создана, опубликована, собираем игроков
                              (fieldset "Players"
                                 (db:map (db:query
                                          "SELECT  name FROM races WHERE
                                             id IN (SELECT race FROM game_players WHERE game = ?)" game)
                                    (lambda (name)
                                       (li "<data>* "name"</data>")))
                                 (if (eq? game_ac account)
                                    (li
                                       (input (type "submit")
                                          (value "Start the game!")
                                          (script "$('#game').submit(function(event) {
                                                      goto( '/start-the-game/"session"/"game"'
                                                            );
                                                      event.preventDefault();
                                                   });")))
                                 (let ((races (db:query
                                          "SELECT  id,name FROM races WHERE account=?
                                             AND id NOT IN (SELECT race FROM game_players WHERE game=?)" account game)))
                                    (if races (list
                                          (li
                                             (select (name "myrace")
                                                (db:map races
                                                   (lambda (id name)
                                                      (option (value id) name)))))
                                          (li
                                             (input (type "submit")
                                                (value "Join the game")
                                                (script "$('#game').submit(function(event) {
                                                            goto( '/join-to-the-game/"session"/"game"',
                                                                  myrace()
                                                                  );
                                                            event.preventDefault();
                                                         });"))))))))))
                     ))))))))
                     ; todo: add the chat here, from game_creation_chat table!
                     ; todo: добавить игровую статистику сюда, если игра уже закончена

               ; ------------------------------------------------
               (action "/update-the-game/" (session game name publish board-size worlds-density players-adjacency options)
                  ; изменять можно только свою игру и только в 0 состоянии
               (let ((state (db:value "SELECT state FROM games WHERE id=?" game)))
                  (if (ne? state 0)
                     (close (send-401)))
                  (send-200)

                  (head "update-the-race")
                  (body
                     (a (href "/home/"session)
                        "Home.") (br)

                     "Updating the race... "

                     (if (db:value "UPDATE games SET name=?,state=?,board_size=?,worlds_density=?,players_adjacency=?,options=? WHERE id=? AND account=?"
                                    name
                                    ; если публикуем игру, то меняем ее состояние
                                    (if (and (eq? state 0) (eq? publish 1))
                                       1
                                       state)
                                    board-size worlds-density players-adjacency options
                                    game account)
                        (list "Ok. You can " (a (id "redirect")
                                                (href "/game/"session"/"game)
                                                "customize it")
                              "." (br)
                              (script "redirect()"))
                        (list "Something wrong :/")))))

               ; присоединиться к игре
               (action "/join-to-the-game/" (session game race)
                  (send-200)

                  (head "Joining race to the game...")
                  (body
                     (if (db:value "INSERT INTO game_players (game,race) VALUES (?,?)" game race)
                        (list "Ok. You can " (a (id "redirect")
                                                (href "/game/"session"/"game)
                                                "check it")
                              "." (br)
                              (script "redirect2()"))
                        (list "Something wrong :/"))))

;               (action "/get-game-players-list/" (account session game)
;                  (send-200)
;                  (html
;                     (db:map (db:query
;                        "SELECT  name FROM races WHERE id IN
;                           (SELECT race FROM game_players WHERE id = ?)" game)
;                        (lambda (name)
;                           (li "<data>"name"</data>")))))


               ; ---------------------------------------


;;
;;                ; ----------------------------------------------
;;                ;
;;                ((starts-with url "/create-new-game/") ; session
;;                (let*((account session args (parse url 2)))
;;
;;                   ; ok, все проверки закончены
;;                   (old-send-200 "Create new game")
;;                   (send-end)))
;;
;;                ((starts-with url "/play/") ; session/game
;;                   (let ((args (string-split url #\/)))
;;
;;                   (if (ne? (length args) 3)
;;                      (close (send-400)))
;;
;;                   (let ((session (list-ref args 1))
;;                         (game    (list-ref args 2)))
;;                   (let ((account (db:value "SELECT id FROM accounts WHERE session = ? AND address = ?" session (car (syscall 51 fd #f #f)))))
;;                      (if (not account)
;;                         (close (send-401)))
;;                      ; todo: добавить проверку на то, что игра действительно принадлежит игроку
;;                      ;       и вообще,  все проверки стоит впихнуть в один большой запрос
;;
;;                      ; ok
;;                      (old-send-200 "One"
;;                            "   <link href='/stylesheets/main.css' type='text/css' rel='stylesheet' />")
;;                      (send "   <header>"
;;                            (db:value "SELECT name FROM games WHERE id=?" game)
;;                            "   </header>"
;;                            "   <div id='main'>"
;;                            "      <view>"
;;                            "         view"
;;                            "      </view>"
;;                            "      <info>"
;;                            "         <scrolls>")
;;                      (send "<scroll>" "title1" "</scroll>")
;;                      (send "<scroll>" "title2" "</scroll>")
;;                      (send "<scroll>" "title3" "</scroll>")
;;                      (send "         </scrolls>"
;;                            "         <mailbox>Mailbox</mailbox>"
;;                            "         <mailbox>Summary</mailbox>"
;;                            "      </info>"
;;                            "   </div>")
;;                      (send-end)))))
;;
;;
               ; ===============================================================
               ; всякая серверная математика
               (simple "/get-advantage-points-left/" (prt lrts)
                  (send-200)

                  (send (+ prt lrts)))

               ;else
               (else
                  (send-404)
                  (send "<html><body>"
                        "<h1>404 Not Found</h1>"
                        "</body></html>")))))
      (else
         (send-405)
         (send "<html><body>"
               "<h1>405 Method Not Allowed</h1>"
               "</body></html>")))
   (close #t)))
