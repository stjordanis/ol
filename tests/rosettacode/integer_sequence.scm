; http://www.rosettacode.org/wiki/Integer_sequence

(let loop ((n 2))
   (print n)
   (unless (> n 100000000000000000000000000000000)
      (loop (* n n))))
