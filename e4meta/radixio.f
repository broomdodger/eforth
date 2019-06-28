CR .( ++++ Radix Based Number I/O ++++ )
\ ==============================================================
\ todo
\ 0 SET-ORDER \ gforth find bug?
\             \ swiftforth win32forth vfxforth ok
\ ==============================================================
\ 20071007 bee add save order to allow D# -1 when -1 is constant
\ 20070729 bee use abort" not throw
\ 20070720 bee separate
\ ==============================================================

DECIMAL

WORDLIST CONSTANT RNI-WORDLIST \ an empty wordlist

: RNI ( n 'ccc' -- n | d ) \ radix number input
  BASE @ >R  BASE !
  GET-ORDER DUP BEGIN ?DUP WHILE 1- ROT >R REPEAT >R  \ save
  RNI-WORDLIST 1 SET-ORDER      \ only knows numbers
  PARSE-WORD ['] EVALUATE CATCH \ convert number, unknown stack
  R> DUP BEGIN ?DUP WHILE 1- R> -ROT REPEAT SET-ORDER \ restore
  R> BASE !  ABORT" number?" ;  \ restore radix before error

: B# ( 'ccc' -- )  2 rni ; IMMEDIATE
: O# ( 'ccc' -- )  8 rni ; IMMEDIATE
: D# ( 'ccc' -- ) 10 rni ; IMMEDIATE
: H# ( 'ccc' -- ) 16 rni ; IMMEDIATE

: RNO ( n n -- ) \ radix number output, ONLY decimal signed
  BASE @ >R  DUP BASE ! 10 XOR IF U. ELSE . THEN  R> BASE ! ;

: .B ( n -- )  2 rno ;
: .O ( n -- )  8 rno ;
: .D ( n -- ) 10 rno ;
: .H ( n -- ) 16 rno ;

