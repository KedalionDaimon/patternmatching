; DEMONSTRATION - how to determine whether two images are similar,
; judging by the sets of their overlapping sub-patterns.

; arbirtrary limitation of the upper subsquare size:
; - shall prevent all too large square sets later on:
(define uppersize 4)
; the idea is that analyzing too long chains is pointless
; (albeit possible), as they are too rare.

; same with the lower size:
(define lowersize 2)

(define (proto-takefirst howmany ofwhatlist resultlist)
  (if (or (zero? howmany) (null? ofwhatlist)) (reverse resultlist)
    (proto-takefirst (- howmany 1)
      (cdr ofwhatlist) (cons (car ofwhatlist) resultlist))))

(define (takefirst howmany ofwhatlist)
  (proto-takefirst howmany ofwhatlist '()))
; sample calls:
; (takefirst 4 '(a b c d e f)) --> (a b c d)
; (takefirst 4 '(a b c)) --> (a b c)

; take the upper left corner of a rectangle:
(define (takesquare whatsize whatrectangle whatheight whatlength)
  ; if it is pointless to take a sub-element, just return the rectangle:
  (if (or (> whatsize whatheight) (> whatsize whatlength)) whatrectangle
    (map (lambda (x) (takefirst whatsize x))
      (takefirst whatsize whatrectangle))))
; sample calls:
; (takesquare 3
;   '((a b c d e f)
;     (g h i j k l)
;     (m n o p q r)
;     (s t u v w x))
;   4 6)
; -->
; '((a b c)
;   (g h i)
;   (m n o))
; (takesquare 3 '(()) 1 0) --> (())

(define (proto-allverticalsquares
          whatsize whatrectangle whatheight whatlength resultlist)
  (if (> whatsize whatheight)
    (reverse resultlist)
    (proto-allverticalsquares
      whatsize (cdr whatrectangle) (- whatheight 1) whatlength
      (cons (takesquare whatsize whatrectangle whatheight whatlength)
        resultlist))))
; sample call:
; (proto-allverticalsquares 3
;   '((a b c d e f)
;     (g h i j k l)
;     (m n o p q r)
;     (s t u v w x))
;   4 6 '())
; -->
; (((a b c)
;   (g h i)
;   (m n o))
;
;  ((g h i)
;   (m n o)
;   (s t u)))

(define
  (proto-allsquares whatsize whatrectangle whatheight whatlength resultlist)
  (if (> whatsize whatlength) (apply append (reverse resultlist))
    (proto-allsquares
      whatsize (map cdr whatrectangle) whatheight (- whatlength 1)
      (cons (proto-allverticalsquares
             whatsize whatrectangle whatheight whatlength '()) resultlist))))

(define
  (allsquares whatsize whatrectangle whatheight whatlength)
  (proto-allsquares whatsize whatrectangle whatheight whatlength '()))
; sample call:
; (allsquares 3
;   '((a b c d e f)
;     (g h i j k l)
;     (m n o p q r)
;     (s t u v w x))
;   4 6)
; -->
; (((a b c)
;   (g h i)
;   (m n o))
;
;  ((g h i)
;   (m n o)
;   (s t u))
; 
;  ((b c d)
;   (h i j)
;   (n o p))
;
;  ((h i j)
;   (n o p)
;   (t u v))
;
;  ((c d e)
;   (i j k)
;   (o p q))
;
;  ((i j k)
;   (o p q)
;   (u v w))
;
;  ((d e f)
;   (j k l)
;   (p q r))
;
;  ((j k l)
;   (p q r)
;   (v w x)))


(define (proto-patternset
          whatsize lowers whatrectangle whatheight whatlength resultlist)
  (if (< whatsize lowers) (apply append (reverse resultlist))
    (proto-patternset
      (- whatsize 1) lowers whatrectangle whatheight whatlength
      (cons (allsquares whatsize whatrectangle whatheight whatlength)
        resultlist))))

; THUNK - uses uppersize and lowersize defined earlier;
; THIS FUNCTION TRANSFORMS A RECTANGLE INTO A SET OF SQUARE PATTERNS:
(define (patternset whatrectangle)
  (let ((h (length whatrectangle))
        (w (length (car whatrectangle))))
  (if (or (> lowersize h) (> lowersize w))
    whatrectangle ; then refrain from further analysis - else:
    (proto-patternset (min h w uppersize) lowersize whatrectangle h w '()))))
; sample call:
; (patternset
;   '((a b c d e f)
;     (g h i j k l)
;     (m n o p q r)
;     (s t u v w x)))
; --> (each line represents sub-square:)
; (((a b c d) (g h i j) (m n o p) (s t u v))
; ((b c d e) (h i j k) (n o p q) (t u v w))
; ((c d e f) (i j k l) (o p q r) (u v w x))
; ((a b c) (g h i) (m n o))
; ((g h i) (m n o) (s t u))
; ((b c d) (h i j) (n o p))
; ((h i j) (n o p) (t u v))
; ((c d e) (i j k) (o p q))
; ((i j k) (o p q) (u v w))
; ((d e f) (j k l) (p q r))
; ((j k l) (p q r) (v w x))
; ((a b) (g h))
; ((g h) (m n))
; ((m n) (s t))
; ((b c) (h i))
; ((h i) (n o))
; ((n o) (t u))
; ((c d) (i j))
; ((i j) (o p))
; ((o p) (u v))
; ((d e) (j k))
; ((j k) (p q))
; ((p q) (v w))
; ((e f) (k l))
; ((k l) (q r))
; ((q r) (w x)))

; Now, for the sake of simplicity, let us assume all images
; to be compared shall have the same size.

(define (proto-comparepics firstpic secondpic result)
  (if (null? firstpic) result
    (if (member (car firstpic) secondpic)
      (proto-comparepics (cdr firstpic) secondpic (+ 1 result))
      (proto-comparepics (cdr firstpic) secondpic result))))

(define (comparepics firstpic secondpic)
  (proto-comparepics (patternset firstpic) (patternset secondpic) 0))
; sample call, to see how similar 8 and 0 look:
; (comparepics
;   '((_ _ _ _ _ x x _ _ _ _ _)
;     (_ _ _ _ x _ _ x _ _ _ _)
;     (_ _ _ x _ _ _ _ x _ _ _)
;     (_ _ _ _ _ x x x _ _ _ _)
;     (_ _ _ _ x x x _ _ _ _ _)
;     (_ _ _ x _ _ _ _ x _ _ _)
;     (_ _ _ _ x _ _ x _ _ _ _)
;     (_ _ _ _ _ x x _ _ _ _ _))
;
;   '((_ _ _ _ _ x x _ _ _ _ _)
;     (_ _ _ _ x _ _ x _ _ _ _)
;     (_ _ _ x _ _ _ _ x _ _ _)
;     (_ _ _ x _ _ _ _ x _ _ _)
;     (_ _ _ x _ _ _ _ x _ _ _)
;     (_ _ _ x _ _ _ _ x _ _ _)
;     (_ _ _ _ x _ _ x _ _ _ _)
;     (_ _ _ _ _ x x _ _ _ _ _)))
;
; --> 108
; - the higher this is, the more similar the images are.

; main function - adjust with images of your liking:
(begin
  (newline)
  (display "TESTING IMAGE COMPARISON:") (newline)
  (display "THE VALUE OF SIMILARITY BETWEEN") (newline)
  (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ X X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ X _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ X _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (newline)
  (display "AND") (newline)
  (newline)
  (display '((_ _ _ _ _ _ _ _ X X _ _))) (newline)
  (display '((_ _ _ _ _ _ _ X _ X _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ X _ _))) (newline)
  (display '((_ _ _ _ _ _ _ _ _ X _ _))) (newline)
  (display '((_ _ _ _ _ _ _ _ _ X _ _))) (newline)
  (display '((_ _ _ _ _ _ _ _ _ X _ _))) (newline)
  (display '((_ _ _ _ _ _ _ _ _ X _ _))) (newline)
  (display '((_ _ _ _ _ _ _ _ _ X _ _))) (newline)
  (newline)
  (display "IS: ")
  (display (comparepics
  '((_ _ _ _ _ _ X _ _ _ _ _)
    (_ _ _ _ _ X X _ _ _ _ _)
    (_ _ _ _ X _ X _ _ _ _ _)
    (_ _ _ X _ _ X _ _ _ _ _)
    (_ _ _ _ _ _ X _ _ _ _ _)
    (_ _ _ _ _ _ X _ _ _ _ _)
    (_ _ _ _ _ _ X _ _ _ _ _)
    (_ _ _ _ _ _ X _ _ _ _ _))

  '((_ _ _ _ _ _ _ _ X X _ _)
    (_ _ _ _ _ _ _ X _ X _ _)
    (_ _ _ _ _ _ X _ _ X _ _)
    (_ _ _ _ _ _ _ _ _ X _ _)
    (_ _ _ _ _ _ _ _ _ X _ _)
    (_ _ _ _ _ _ _ _ _ X _ _)
    (_ _ _ _ _ _ _ _ _ X _ _)
    (_ _ _ _ _ _ _ _ _ X _ _))))
    ; --> 168
  (newline)
  (newline)
  (display "AND THE VALUE OF SIMILARITY BETWEEN") (newline)
  (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ X X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ X _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ X _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (newline)
  (display "AND") (newline)
  (newline)
  (display '((_ _ _ _ X X X X X _ _ _))) (newline)
  (display '((_ _ _ _ _ _ _ _ X _ _ _))) (newline)
  (display '((_ _ _ _ _ _ _ _ X _ _ _))) (newline)
  (display '((_ _ _ _ _ _ _ X _ _ _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (display '((_ _ _ _ _ _ X _ _ _ _ _))) (newline)
  (newline)
  (display "IS: ")
  (display (comparepics
  '((_ _ _ _ _ _ X _ _ _ _ _)
    (_ _ _ _ _ X X _ _ _ _ _)
    (_ _ _ _ X _ X _ _ _ _ _)
    (_ _ _ X _ _ X _ _ _ _ _)
    (_ _ _ _ _ _ X _ _ _ _ _)
    (_ _ _ _ _ _ X _ _ _ _ _)
    (_ _ _ _ _ _ X _ _ _ _ _)
    (_ _ _ _ _ _ X _ _ _ _ _))

  '((_ _ _ _ X X X X X _ _ _)
    (_ _ _ _ _ _ _ _ X _ _ _)
    (_ _ _ _ _ _ _ _ X _ _ _)
    (_ _ _ _ _ _ _ X _ _ _ _)
    (_ _ _ _ _ _ X _ _ _ _ _)
    (_ _ _ _ _ _ X _ _ _ _ _)
    (_ _ _ _ _ _ X _ _ _ _ _)
    (_ _ _ _ _ _ X _ _ _ _ _))))
    ; --> 156
  (newline))

