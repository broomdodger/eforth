CR .( ++++ eForth c18 assembler/disassembler ++++ )
\ ==============================================================
\ todo  initialize assembler variables
\       integrate with Charley flash runner
\ ==============================================================
\ 20090105 bee add 's check
\ 20090104 bee add "0 call? !" to fix ; tail recursion
\ 20090103 bee disassemble address, optimize j2 message
\ 20090102 bee cleanup, gforth ' .error-string is doerror
\ 20081227 bee add search order, many changes, disassembler
\ 20081227 bee keep line length < 64, will be in blocks
\ 20070707 bee c18 asm from colorforth
\ ==============================================================
ONLY FORTH DEFINITIONS  HEX
[DEFINED] EMPTY [IF] EMPTY [THEN] \ swiftforth
[DEFINED] dobacktrace [DEFINED] maxdepth-.s and [IF] \ gforth
  ' noop is dobacktrace \ limit error messages ( *** )
: .s ( -- ) depth if >r  RECURSE  r> dup . then ;
[THEN]
[DEFINED] +ORDER 0= [IF] \ search order extensions
: _-ORDER ( w wid*n n -- wid*n w n ) ( remove )
  DUP IF 1- SWAP >R  RECURSE  ( wid*n w n )
    OVER R@ XOR IF 1+ R> -ROT EXIT  THEN R> DROP  THEN ;
: -ORDER ( wid -- ) GET-ORDER  _-ORDER NIP  SET-ORDER ;
: +ORDER ( wid -- )
  DUP >R -ORDER GET-ORDER R> SWAP 1+ SET-ORDER ;
[THEN]
\ ==============================================================
WORDLIST CONSTANT ASSEMBLER-WORDLIST ( assembler USER words )
: ]a ( -- ) ASSEMBLER-WORDLIST +order ;
: [m ( -- ) ( start macro ) ]a ; immediate
GET-CURRENT ( wid )
WORDLIST CONSTANT ASM-WORDLIST ( assembler INTERNAL words )
ASM-WORDLIST SET-CURRENT ( add to assembler INTERNAL words )
: asm-err ( a n -- ) cr type 1 abort"  asm?" ;
: [a] ( "name" -- ) ( compile word from ASSEMBLER-WORDLIST )
  parse-word ASSEMBLER-WORDLIST search-wordlist 0= abort" [a]?"
  compile, ; immediate
: a: ( "name" -- ) ( compile word into ASSEMBLER-WORDLIST )
  get-current >r  ASSEMBLER-WORDLIST set-current  :
  r> set-current ;
ASM-WORDLIST +ORDER ( assembler INTERNAL words )
a: a[ ( -- ) ASSEMBLER-WORDLIST -order ;
a: m] ( -- ) ( end macro ) [a] a[ ; immediate
\ ==============================================================
( memory management )
create mem ( -- a ) 080 ( c18 ram=64 rom=64 ) cells allot
: memory ( n -- a ) 07F and  cells mem + ;
: pxa ( p -- a ) 0155 xor ; ( pattern x address )
: pxn ( p -- n ) 015555 xor ; ( pattern x number )
( disassembler )
5 constant #n ( -- n ) ( opcode name max size )
create 'n ( -- a ) ( opcode names table )
  'n  020 ( 32 ) #n * dup chars allot  bl fill ( clear table )
  char ; 'n c! ( special case name  ; )
: n: ( op "name" -- ) dup #n * chars 'n + ( op dst ) >r
  >in @  parse-word ( op n src u ) r> swap chars move ( name )
  >in !  >r  a:  r> ( op ) [compile] literal ; ( definition )

: tab ( -- ) space space ;
: _ ( n -- ) s" ________" drop swap type tab ;
: .iw ( a -- ) pxn 0 <# # # # # # # #> type ;
: .i ( u n -- i ) rshift  01F and  dup 0 <# # # #> type ;
: .n ( i -- ) #n * chars 'n +  #n  -trailing  type space ;
: j? ( n -- n f ) .i ." ." dup 2 8 within over 4 xor and ;
: .op ( a a -- a ) dup pxn  swap 2* 2* ( 0 fill slot 3 )
  dup 0F j? ( slot 0 jump 10 bits, 9 address, 1 math )
  if 8 _             .n drop 03FF and u. exit  then >r
  dup 0A j? ( slot 1 jump 8 bits )
  if 5 _ r>       .n .n drop  0FF and u. exit  then >r
  dup  5 j? ( slot 2 jump 3 bits )
  if 2 _ r> r> .n .n .n drop    7 and
    over 7 and 7 = over 0= and 8 and + ( page fix )
    over 7 invert and +               u. exit  then >r
  0 .i  tab  r> r> r> .n .n .n .n drop ;

: dm ( a n -- ) ( disassemble memory ) bounds
  begin 2dup xor \ bbb ??? wrap
  while cr dup 0 <# # # # #> type tab ( address )
    dup memory @  dup .iw tab  .op  1 +
  repeat 2drop ;

\ ==============================================================
( assembler )
create h       0BA , ( 186 ??? )
create ip      0B9 , ( 185 ??? )
create slot      4 ,
create call?  0300 , ( 768 )
create 's?      -1 , ( 0= active, 0<> inactive )

: ix ( op a -- ) swap cells + @ execute ;
\ : p, ( n -- ) h  @ memory  !  h @ 1 +  040 invert and  h ! ;
: p, ( n -- ) \ 20081229 OkadWork.cf  bbb  080 chunks  ???
  h @ memory !  h @  1 over +  over xor  07F and  xor  h ! ;
: +i ( op -- ) ip @ memory +! ;
: s4 ( op -- ) 0D ( 13 ) lshift
               dup call? !  h @ ip !  p,  1 slot  ! ;
: sn ( op -- ) dup call? !            +i  1 slot +! ;
: s0 ( op -- ) 0D ( 13 ) lshift sn ;
: s1 ( op -- ) 8 lshift sn ;
: s2 ( op -- ) 3 lshift sn ;
: s3 ( op -- ) dup 3 and if 7 sn s4 exit  then 2/ 2/ sn ;
create 'i ( -- a ) ' s0 , ' s1 , ' s2 , ' s3 , ' s4 ,
: i, ( op -- ) slot @ 'i ix ;

  01C n: . ( -- ) i, ; ( nop ( slot 3 ok )

( compile jump )
: break ( -- ) 4 slot ! ; ( force new iw, no pad )
: 9same ( a op a -- a op f ) >r over r> invert xor 0200 and ;
: j0 ( a op -- ) i,  +i  break ;
: j3 ( a op -- ) [a] .  j0 ;
: j1 ( a op -- ) h @ 9same ( not port ? )
  if over 0100 and if >r  0FF and  r> j0 exit  then
  then cr ." port j1 to " over pxa u. ." at " h ?
  [a] .  [a] .  j3 ;
: j2 ( a op -- ) 's? @ ( 's inactive ? )
  if over pxa  h @ xor  03F8 and ( j2 fit ? )
    if cr ." j2 to j0 at " ip ?  [a] .  j3 exit  then
  then 's? @ 0= \ bbb
  if cr ." 's active at " ip ? ." to " over pxa .  dup 's? !
  then >r  7 and  r> j0 ;
create 'j ( -- a ) ' j0 , ' j1 , ' j2 , ' j3 , ' j0 ,
: -adr ( a op -- ) >r  pxa  r> slot @ 'j ix ;

( resolve forward jump )
: @h ( n -- a ) 8 /mod + ;
: f1 ( a op -- ) 2/ 2/ 2/ memory +! ;
: f3 ( a op -- ) over pxa  over  @h xor  02F8 and
  if s" f3 page" asm-err  then >r  7 and  r> f1 ;
: f2 ( a op -- ) dup @h 9same
  if over 0100 and if >r  0FF and  r> f1 exit
  then then s" f2 page" asm-err ;
create 'f ( -- a ) ' f1 , ' f1 , ' f2 , ' f3 ,

( mark forward jump )
: adr ( op -- n ) slot @ 3 = if [a] .  then  i,
  ip @ 2* 2* 2* ( encode ip h slot in one cell, iii.hhh.sss )
  h @ ip @ negate + + 2* 2* 2*  slot @ +  break ;

\ ==============================================================
a: ; ( -- ) call? @ dup 06000 xor
   if dup 0300 xor  if dup 018 xor
     if 0 and i, break exit ( return ( slot 3 ok )
   then then then  0 call? ! \ bbb ???
   dup 2/ and negate  +i ; ( tail recursion )

  001 n: ;:    (   -- ) i, break ; ( coroutine )
  002 n: jump  ( a -- ) -adr ;    a: ahead  ( -- a ) 02  adr ;
  003 n: call  ( a -- ) -adr ;    a: leap   ( -- a ) 03  adr ;
  004 n: unext ( a -- ) i, drop ; ( slot 3 ok )
  005 n:  next ( a -- ) -adr ;    a: zif    ( -- a ) 05  adr ;
  006 n:  if   ( -- a )  adr ;    a:  until ( a -- ) 06 -adr ;
  007 n: -if   ( -- a )  adr ;    a: -until ( a -- ) 07 -adr ;

  008 n: @p+  ( -- ) i, ; ( fetch via p inc p ( slot 3 ok )
  009 n: @a+  ( -- ) i, ; ( fetch via a inc a )
  00A n: @b   ( -- ) i, ; ( fetch via b )
  00B n: @a   ( -- ) i, ; ( fetch via a )
  00C n: !p+  ( -- ) i, ; ( store via p inc p ( slot 3 ok )
  00D n: !a+  ( -- ) i, ; ( store via a inc a )
  00E n: !b   ( -- ) i, ; ( store via b )
  00F n: !a   ( -- ) i, ; ( store via a )

  010 n: +*   ( -- ) i, ; ( slot 3 ok )
  011 n: 2*   ( -- ) i, ;
  012 n: 2/   ( -- ) i, ; ( signed )
  013 n: not  ( -- ) i, ; ( invert )
  014 n: +    ( -- ) i, ; ( slot 3 ok )
  015 n: and  ( -- ) i, ;
  016 n: xor  ( -- ) i, ; ( exclusive-or )
  017 n: drop ( -- ) i, ;

  018 n: dup  ( -- ) i, ; ( slot 3 ok )
  019 n: pop  ( -- ) i, ; ( return stack )
  01A n: over ( -- ) i, ;
  01B n: a@   ( -- ) i, ; ( fetch register a )
\ 01C n: .    ( -- ) i, ; ( nop ( slot 3 ok )
  01D n: push ( -- ) i, ; ( return stack )
  01E n: b!   ( -- ) i, ; ( store register b )
  01F n: a!   ( -- ) i, ; ( store register a )

a: | ( -- ) ( force new iw, pad with nop )
\  begin slot @ 4 xor while [a] .  repeat  0 call? ! ;
   slot @ 4 xor if [a] .  RECURSE ( exit ) then  0 call? ! ;
a: ?| ( -- ) 1 slot @ < if [a] |  then ; ( pad slots 2 3 )
a: , ( n -- ) pxn  p, ;
a: # ( n -- ) [a] @p+  [a] ,  ;

a: here ( -- a ) [a] |  h @ 03FF and ;
a: begin ( -- a ) [a] here  ;
a: for ( -- a ) [a] push  [a] begin  ;
a: aft ( a -- a a ) drop  [a] ahead  [a] begin  swap ;
a: then ( a -- ) [a] begin  pxa  swap 8 /mod swap 'f ix ;
a: else ( a -- A ) [a] ahead  swap  [a] then  ;
a: while ( a -- A a ) [a] if  swap ;
a: -while ( a -- A a ) [a] -if  swap ;
a: again ( a -- ) [a] jump ;
a: repeat ( a a -- ) [a] again  [a] then  ;

c0de constant mn@ ( -- n ) ( magic number )
a: end-asm ( n a -- ) drop  [a] |  [a] a[  mn@ over xor
   if s" stack" asm-err  then drop ;
\ a: end-code ( n a -- ) [a] end-asm  ;

\ ==============================================================
a: 's ( n "name" -- ) 0 's? !  ( ... ) ; \ bbb ??? test

\ ==============================================================
( wid ) SET-CURRENT ( restore )
: dm ( a n -- ) dm ;
:  ORG ( n -- ) dup h !  ip !  break ; 000 ORG
: +ORG ( n -- ) [a] here  + ORG ;
: ASM ( -- n a ) 0 ORG  ]a  mn@  [a] begin  ;
\ : CODE ( "name" -- n a ) ( [a] code ) ASM ; \ bbb ???

\ ==============================================================
( tools and test )

: tt ( -- ) 020 0 do cr i .n loop ; ( table test )
: hh ( -- ) ." h=" h ?  ." ip=" ip ?
  ." slot=" slot ?  ." call?=" call? ?  ." 's?=" 's? ? ;

: eras0 ( a n -- ) swap org  0 do [m 0 , m] loop ;
: eras ( a n -- ) \ bbb save restore h ip ???
  swap org  0 do [m 0A9 call ( dean ) m] loop ;

ASM-WORDLIST -ORDER ( restore )

: ii ( -- ) ( instruction test ) 0 org  [m
  ;     ;:    0 jump  0 call
  begin unext  begin next  begin until  begin -until
  @a    @b    @a+   @p+     !a    !b    !a+   !p+
  not   2/    2*    +*      drop  xor   and   +
  a@    over  pop   dup     a!    b!    push  .
  .     .     .     .       .     .     .     .
  m]  0 010 dm ;

: do! ( -- ) s" 0 org  010  dup 0 do" evaluate ; immediate
: loop! ( -- ) s" loop 0 swap dm" evaluate ; immediate
: j0 do! [m begin       again m] loop! ;
: j1 do! [m begin     . again m] loop! ;
: j2 do! [m begin   . . again m] loop! ;
: j3 do! [m begin . . . again m] loop! ;

: f0 do! [m       if   then m] loop! ;
: f1 do! [m     . if   then m] loop! ;
: f2 do! [m   . . if   then m] loop! ;
: f3 do! [m . . . if   then m] loop! ;

: p0 do! [m       if . then m] loop! ;
: p1 do! [m     . if . then m] loop! ;
: p2 do! [m   . . if . then m] loop! ;
: p3 do! [m . . . if . then m] loop! ;
\ p2 will cause  f2 page?  error

: q0 do! [m       if . | . then m] loop! ;
: q1 do! [m     . if . | . then m] loop! ;
: q2 do! [m   . . if . | . then m] loop! ;
: q3 do! [m . . . if . | . then m] loop! ;

: n0 do! [m for       next m] loop! ;
: n1 do! [m for     . next m] loop! ;
: n2 do! [m for   . . next m] loop! ;
: n3 do! [m for . . . next m] loop! ;

: u0 do! [m for       unext m] loop! ;
: u1 do! [m for     . unext m] loop! ;
: u2 do! [m for   . . unext m] loop! ;
: u3 do! [m for . . . unext m] loop! ;

: tail ( -- ) 0 org
  [m    5 call ;        . 5 call ;  . . 5 call ;
  . . . 5 call ;  . . . . 5 call ;
  ; \ bbb ??? fix bug
  + + + +   + + + +   + + + +   m]  0 010 dm ;

: ss ( n -- ) 010  2dup eras  over org [m
  @p+ !a 's 010 call
  @p+ !a 's 013 call
  @p+ !a 's 015 call
  @p+ !a      0 call
  @p+ !a      3 call
  @p+ !a      5 call
  @p+ !a 's 020 jump
  @p+ !a 's 023 jump
  @p+ !a 's 025 jump m]  dm ;

]a ( start assembler )

