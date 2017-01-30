#!/usr/bin/ol

,load "db.lisp"

; account - обязательный атрибут для проверки ownership объектов, с которыми мы работаем

; account идентифицирует пользователя. один пользователь - один аккаунт
(db:query "DROP TABLE IF EXISTS accounts")
(db:query "CREATE TABLE accounts (
   id INTEGER PRIMARY KEY
-- todo: store creation time of account, in 'dob' field or something like this
,  name TEXT
,  password TEXT
,  creation_time DATETIME DEFAULT CURRENT_TIMESTAMP

,  usermail TEXT -- временно не принимаем

,  session TEXT -- сеансовый ключ
,  remote_address TEXT -- адрес, с которого зашли в аккаунт (todo: завести отдельную табличку, и позволить множественные заходы с разных адресов?)
,  logged_at DATETIME DEFAULT CURRENT_TIMESTAMP -- время последнего логина
)")
(db:query "DROP TABLE IF EXISTS account_logged_from")
(db:query "CREATE TABLE account_logged_from (
   id INTEGER PRIMARY KEY
,  account REFERENCES accounts(id)
,  remote_address TEXT -- адрес, с которого зашли в аккаунт
)")

; пользователь "никто" имеет номер 0
(for-each (lambda (values)
   (apply db:query (cons "INSERT INTO accounts (id, name, password) VALUES (?,?,?)" values))) '(
      (0 "noname" "racerx")
      (1 "user@1" "123456")
      (2 "user@2" "223456")
      (3 "user@3" "323456")
      (4 "user@4" "423456")
      (5 "user@5" "523456")
      (6 "user@6" "623456")
      (7 "user@7" "723456")
      (8 "user@8" "823456")
      (9 "user@9" "923456")
))


; в играх участвуют расы
; каждый пользователь может завести себе столько рас, сколько захочет
(db:query "DROP TABLE IF EXISTS races")
(db:query "CREATE TABLE races (
   id INTEGER PRIMARY KEY
,  name TEXT -- название расы
,  account REFERENCES accounts(id)
,  creation_time DATETIME DEFAULT CURRENT_TIMESTAMP

,  prt  INTEGER DEFAULT 0 -- Primary Racial Trait (0..9)
,  lrts INTEGER DEFAULT 0 -- Lesser Racial Traits (1,2,4,8,..,8192)
)")
;(db:query "INSERT INTO races (id, name, account) VALUES (0, 0, 0)")
; есть несколько "предустановленных" рас, owner для них должен быть "0"
;(for-each (lambda (values)
;   (apply db:query (cons
;"INSERT INTO races (id, name, account) VALUES (?,?,?)" values))) '(
;      (1  "Humanoids"   0)
;      (2  "Rabbitoids"  0)
;      (3  "Insectoids"  0)
;      (4  "Nucleotids"  1)
;      (5  "Silicanoids" 1)
;      (6  "Antetherals" 1)
;))


; список всех игр - законченных и текущих
(db:query "DROP TABLE IF EXISTS games")
(db:query "CREATE TABLE games (
   id INTEGER PRIMARY KEY
,  name TEXT  -- название игры
,  password   -- пароль к игре
,  account REFERENCES accounts(id)
,  creation_time DATETIME DEFAULT CURRENT_TIMESTAMP

--
,  state         INTEGER DEFAULT 0 -- текущее состояние игры (0..x)
                                   -- 0: создается, можно редактировать все
                                   -- 1:
,  options       INTEGER DEFAULT   0 -- опции создания игры (7 опций)

-- параметры генератора доски
,  board_size         INTEGER DEFAULT 2 -- размер доски, l.y. (1: 400m tiny, 2: 800 small, 3: 1200 medium, 4: 1600 large, 5: 2000 huge)
,  worlds_density     INTEGER DEFAULT 2 -- населенность мира (1-4: sparse, normal, dense, packed)
,  players_adjacency  INTEGER DEFAULT 2 -- соседство игроков (1-4: close, moderate, farther, distant)

-- http://www.elite-games.ru/stars/doc/game_rules.shtml
,  victory_rules         INTEGER DEFAULT  11 -- флаги провозглашения победы
,  owns_of_all_planets   INTEGER DEFAULT  60 -- если захватичено N% планет
,  attains_tech_level    INTEGER DEFAULT  22
,  attains_tech_fields   INTEGER DEFAULT   4
,  exceeds_score         INTEGER DEFAULT 11000
,  exceeds_second_score  INTEGER DEFAULT 100
,  production_capacity   INTEGER DEFAULT 100
,  owns_capital_ships    INTEGER DEFAULT 100
,  has_score_after_year  INTEGER DEFAULT 100
,  must_meet_criterias   INTEGER DEFAULT   1
,  must_years_pass       INTEGER DEFAULT  50

,  difficulty   INTEGER -- сложность (1 - easy, 2 - standard, 3 - harder, 4 - expert), на будущее так как относится к AI
)")
(db:query "DROP TABLE IF EXISTS game_players")
(db:query "CREATE TABLE game_players (
   id INTEGER PRIMARY KEY -- не нужен
,  game REFERENCES games(id)
,  race REFERENCES races(id)
,  ready BOOLEAN DEFAULT 0
)")
(db:query "DROP TABLE IF EXISTS game_creation_chat")
(db:query "CREATE TABLE game_creation_chat (
   id INTEGER PRIMARY KEY
,  game REFERENCES games(id)
,  race REFERENCES races(id)
,  text TEXT
,  time DATETIME
)")

; сразу создам парочку игр
;(for-each (lambda (values)
;   (apply db:query (cons
;"INSERT INTO games (id, name, state) VALUES (?,?,?)" values))) '(
;      (1  "A Walk in the Park" 0)
;      (2  "A Barefoot JayWalk" 0)
;      (3  "Roller Ball"        0)
;      (4  "Blade Runner"       0)))

;(for-each (lambda (values)
;   (apply db:query (cons
;"INSERT INTO game_players (id, game, race) VALUES (?,?,?)" values))) '(
;      (1  1 1)
;      (2  1 3)
;      (3  2 2)
;      (4  2 4)
;      (5  3 5)
;      (6  3 6)
;      (7  4 1)
;      (8  4 3)
;      (9  4 4)
;))


;(db:query "CREATE TABLE users (
;   id INTEGER PRIMARY KEY -- comment
;)")
;
;(db:query "CREATE TABLE scrolls (
;   id INTEGER PRIMARY KEY
;,  title TEXT
;)")
