#lang typed/racket

(lambda ([x : (U #f Number)])
  (if (number? x) (add1 x) 0))
