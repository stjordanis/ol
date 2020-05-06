---
layout: page
title:  Принципы функционирования REPL 
date: 2017-02-23 12:14:24 EET
categories: en
---

REPL, или раскрывая аббревиатуру Read-Eval-Print-Loop, это цикл взаимодействия среды Lisp с окружающим миром (включая пользователя, если он есть). Разберем все этапы по очереди.

### Read

Под "read" подразумевается чтение исходного кода программы и приведение его в удобоваримый вид. Этой работой занимается функция fd->exp-stream, возвращающая ленивый итератор.
