CR .( ++++ platform load -- gforth ++++ )
\ ==============================================================
\ todo     CREATE-FILE does not use the same path as INCLUDE !!!
\ notify   when is DOES> problem?
\          fails oddly in later search order
\ notify   bugs -- include   0 set-order   ctrl
\ ==============================================================
\ 20110115 bee add RIVE >CHAR . .S
\ 20090726 bee add CELL- S+ (gforth S+ weird error in SAVEHEX)
\ 20071209 charley add include" tags.fs" for vim
\ 20071127 bee add c+! append place for .ERROR-FRAME2
\ 20070825 bee move included include to load.gf
\ 20070715 bee add +ORDER -ORDER
\ 20070709 bee add ' ['] for "Cannot tick compile-only word"
\ 20070419 bee try to tame gforth error messages
\ 20070419 bee separate from eforth load file
\ 20070417 bee grrr gforth  Cannot tick compile-only word
\ 20070417 bee gforth try again grrr
\ 20061210 bee start  ctrl bug  logout on include error bug
\ ==============================================================

DECIMAL

\ ==============================================================

marker m.empty

: empty ( -- ) only forth also definitions  m.empty ;

: warning ( -- a ) warnings ;

warning on

\ ==============================================================

: ' ( "name" -- xt ) COMP' DROP ; \ need for compile-only words
: ['] ( "name" -- ) ' [COMPILE] LITERAL ; IMMEDIATE

: CTRL ( 'ccc' -- c ) CHAR $01F AND ; \ grrr gforth069
: [CTRL] ( 'ccc' -- ) CTRL [COMPILE] LITERAL ; IMMEDIATE

: NOT ( f -- f ) 0= ;

: C+! ( n a -- ) DUP >R C@ + R> C! ;
: APPEND ( src +n dst -- ) 2DUP 2>R COUNT + SWAP MOVE 2R> C+! ;
: PLACE ( src +n dst -- ) 0 OVER C!  APPEND ;
: +PLACE ( src +n dst-- ) APPEND ;

: CELL- ( a -- a ) [ 1 CELLS ] LITERAL - ;

: S+ ( ca1 u1  ca2 u2 -- ca1 u1+u2 ) \ concatenate strings
  >R >R                 \ save second string
  2DUP CHARS +          \ end of first string
  R> SWAP R@ CHARS MOVE \ append second string
  R> + ;                \ adjust count

: RIVE ( hl -- h l ) DUP 8 RSHIFT  SWAP $0FF AND ;
: >CHAR ( c -- c ) DUP 127 BL WITHIN IF DROP BL THEN ;

\ ==============================================================
\ Print Working Directory
\ [defined] windows [if] \ Win gforth069
\ : pwd ( -- ) ; \ 'system' BROKEN
\
\ [else] \ Mac gforth069
\ : pwd ( -- ) s" pwd" system ; \ bbb -n ???
\
\ [then]

\ ==============================================================
\ little endian fetch
BASE C@ [if] \ Intel -- little endian

: le! ( n a -- ) ! ;
: le@ ( a -- n ) @ ;

[else] \ Mac PPC -- big endian

: le! ( n a -- )
  [ 1 CELLS ] literal bounds do dup i c! 8 rshift loop drop ;

: le@ ( a -- n )
  0 swap [ 1 CELLS 1- ] literal over +
  do 8 lshift i c@ or -1 +loop ;

[then]

\ ==============================================================
\ : DEFERS ( compilation "name" -- ; run-time ... -- ... )
\   \G Compiles the present contents of deferred word @i{name}
\   \G into the current definition.  I.e., this produces static
\   \G binding as if @i{name} was not deferred.
\   ' >BODY @ COMPILE, ; IMMEDIATE
\ ==============================================================

\ column output control
CREATE C#  0 , 54 , \ current column, max column
: _EMIT ( c -- )      1 C# +!   DEFERS EMIT ;   ' _EMIT IS EMIT
: _TYPE ( ca u -- ) DUP C# +!   DEFERS TYPE ;   ' _TYPE IS TYPE
: CR ( -- ) CR   0 C# ! ;
: ?LF ( u -- ) C# 2@ U< IF CR THEN ;

\ search order extensions
: _-ORDER ( w wid*n n -- wid*n w n ) \ remove from search order
  DUP IF 1- SWAP >R   RECURSE   ( wid*n w n )
    OVER R@ XOR IF 1+ R> -ROT EXIT THEN R> DROP
  THEN ;

: -ORDER ( wid -- ) GET-ORDER  _-ORDER NIP  SET-ORDER ;
: +ORDER ( wid -- )
  DUP >R -ORDER  GET-ORDER R> SWAP 1+ SET-ORDER ;

\ ==============================================================
\ gforth error control, editor hooks

create fixer  0 , 256 chars allot

[defined] MacOSX [if] \ for bee
: editor ( -- a u ) s" gvim +" ;
: fix ( -- ) \ open editor to file:line
  fixer @ 0= abort" error: not a file"
  editor pad place  fixer count pad append  pad count system ;
[then]

[defined] vim [if] \ for Charley
INCLUDE" tags.fs"
: editor ( -- a u ) s" vim -g +" ; \ Linux vim gui
: fflush ( -- ) tags-file-id flush-file throw ; \ for vim
: fix ( -- ) \ open editor to file:line of error
  fixer @ 0= abort" error: not a file"  fflush
  editor pad place  fixer count pad append  pad count system ;
[then]

: .errline ( a1 u1  a2 u2 -- ) \ underlined only error word
  >r  >r over >r  cr type \ display error line
  r> r> swap -  cr spaces r> 0 ?do [char] ^ emit loop ;

: .ERROR-FRAME2 ( throw#  a1 u1  a2 u2  n3  [a4 u4] -- throw# )
  \ a4 u4: filename of included file - optional
  \ n3:    line number
  \ a2 u2: parsed word
  \ a1 u1: input line
  error-stack @
  if dup \ over \ bbb ??? ( filename count? )
    if cr ( filename ) type ." :" ( error line ) 0 dec.r
    else 2drop drop
    then 2drop 2drop  exit
  then
  dup ( filename count? )
  if ( n3  [a4 u4] ) 2>R
    dup 0 <# bl hold #s #> ( error line ) fixer place
    2R@ ( filename ) fixer append
    2R> ( filename ) cr type ." :" ( error line ) 0 dec.r
  else ( 0 ) fixer !  2drop
  then  .errline space
  ( throwcode ) dup .error-string  dup ."  (" 0 .r ." )" ;

: _DOERROR ( throw-code -- )
  >stderr
  input-error-data .ERROR-FRAME2
  error-stack @ 0 ?do  error> .ERROR-FRAME2  loop
  normal-dp dpp !  clearstacks  forth-wordlist +order ;

' _DOERROR IS DOERROR

\ ==============================================================

