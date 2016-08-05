; Sample word checking mechanism based on structural decomposition.
; usage: enter (tryword "someword") at the REPL.

; this function adds each time the current first element of a list to the result list,
; producing a new list, and terminates when either the source list is empty or the
; number of required elements has been reached.
(define (proto-takefirst howmany ofwhatlist resultlist)
  (if (or (zero? howmany) (null? ofwhatlist))
    (reverse resultlist)
    (proto-takefirst (- howmany 1) (cdr ofwhatlist) (cons (car ofwhatlist) resultlist))))

(define (takefirst howmany ofwhatlist) (proto-takefirst howmany ofwhatlist '()))

; this function determines all possible fragments of length "whatlength" in a given
; ordered source list.
(define (proto-subchains whatlength ofwhatlist listlength resultlist)
  (if (> whatlength listlength)
    (reverse resultlist)
    (proto-subchains whatlength (cdr ofwhatlist) (- listlength 1)
	  (cons (takefirst whatlength ofwhatlist) resultlist))))
	  
(define (subchains whatlength ofwhatlist)
  (proto-subchains whatlength ofwhatlist (length ofwhatlist) '()))

; this function determines all possible sub-fragments of a list, starting
; from a given length
(define (proto-allsubchains ofwhatlist startinglength resultlist)
  (if (zero? startinglength)
    (reverse resultlist)
    (proto-allsubchains ofwhatlist (- startinglength 1)
	  (cons (subchains startinglength ofwhatlist) resultlist))))
	  
(define (allsubchains ofwhatlist) (apply append (proto-allsubchains ofwhatlist (length ofwhatlist) '())))

; primordial matching of the sets against each other:
(define (proto-matchsets firstset secondset matchcounter)
  (if (null? firstset)
    matchcounter
    (if (member (car firstset) secondset)
      (proto-matchsets (cdr firstset) secondset (+ 1 matchcounter))
	  (proto-matchsets (cdr firstset) secondset matchcounter))))

; this is going to give a number on how well two sets were matching:
(define (matchsets firstset secondset)
  (* 1.00
    (/ (proto-matchsets secondset firstset 0)
       (+ 1 (abs (- (length firstset) (length secondset)))))))

; extend this dictionary as you will - it turns each word into a
; sub-chain set.
(define dictionary (map allsubchains  (map string->list
'("apple" "appeal" "apocryphal" "awful" "awsome" "acryl"))))

; find the best matching word in the dictionary;
; could be easily adapted to return a list of best matches rather than "the" best match.
(define (proto-checkwords target mydict candidate vigor)
  (if (null? mydict)
    (car candidate) ; we can return just the first chain, as that is the matched word in chars.
    (let ((matchstrength (matchsets target (car mydict))))
      (if (< vigor matchstrength)
	    (proto-checkwords target (cdr mydict) (car mydict) matchstrength)
	    (proto-checkwords target (cdr mydict) candidate vigor)))))
	  
; this function is a thunk using the dictionary above
(define (checkwords target)
  (proto-checkwords target dictionary (list (string->list "NO SUGGESTION FOUND")) 0))

(define (tryword misspelled)
  (let ((subchains (allsubchains (string->list misspelled))))
    (if (member subchains dictionary)
	  (begin (display "THIS IS A KNOWN WORD.") (newline))
	  (begin (display (string-append "SUGGESTION: " (list->string (checkwords subchains)))) (newline)))))

(begin
(display 'KNOWN_WORDS) (newline) (display '("apple" "appeal" "apocryphal" "awful" "awesome" "acryl")) (newline) (newline)
(display '(tryword #\" apfel #\")) (newline)
(tryword "apfel") (newline) (newline)
; result:
; SUGGESTION: apple
(display '(tryword #\" akril #\")) (newline)
(tryword "akril") (newline) (newline)
; result:
; SUGGESTION: acryl
(display '(tryword #\" xxx #\")) (newline)
(tryword "xxx") (newline) (newline))
; result:
; SUGGESTION: NO SUGGESTION FOUND

