(with-input-from-string (*standard-input* "123") (repl))

(with-input-from-string (*standard-input* "(define a 10) a (set! a 20) a") (repl))

(with-input-from-string (*standard-input* "(define f (lambda (x) x)) f") (repl))

(with-input-from-string (*standard-input* "(+ 1 2)") (repl))

(with-input-from-string (*standard-input* "
(define (fibonacci n)
    (if (<= n 1)
	n
	(+ (fibonacci (- n 1)) n)))
(fibonacci 5)
") (repl))

(with-input-from-string (*standard-input* "
(define (num-to-symbol n)
    (cond
      ((= n 0) 'zero)
      ((= n 1) 'one)
      ((= n 2) 'two)))
(num-to-symbol 0)
(num-to-symbol 2)
") (repl))
