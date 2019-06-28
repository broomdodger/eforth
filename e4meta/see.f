CR .( ++++ eForth see -- target decompiler ++++ )
\ ==============================================================
\ todo
\ ==============================================================
\ 20110210 bee update dump for macros
\ 20110115 bee move word% ?char >adr to cpu.e4
\ 20110115 bee fix addressing  name collisions
\ 20110114 bee cr at next word
\ 20110112 bee see
\ ==============================================================

decimal

get-current ( wid )
forth-wordlist set-current

wordlist constant e4-see
: esee ( -- ) \ set search order
  e4-wordlist +order  e4-see +order ;

esee

: ' ( "name" -- a ) [O] ' ;

e4-see set-current
: esee ( -- ) \ un-set search order
  e4-wordlist -order  e4-see -order ;

\ host words
: h! ( a -- n ) ! ;
: h@ ( a -- n ) @ ;
: hsee ( "name" -- ) see ;

\ target words
: c@ ( a -- n ) ta>ha c@ ;
: @ ( a -- n ) ta>ha '@[] h@ execute databit& and ;
: here ( -- a ) [O] here ;

: chars ( n -- n ) [O] chars ;
: char- ( a -- a ) 1 chars - ;
: char+ ( a -- a ) 1 chars + ;

: cells ( n -- n ) [O] cells ;
: cell- ( a -- a ) 1 cells - ;
: cell+ ( a -- a ) 1 cells + ;

: name> ( a -- xt ) cell- cell- @ ;
: count ( a -- a c ) dup char+ swap c@ ;
: type ( a n -- )
  chars bounds begin 2dup xor while count emit repeat 2drop ;
: .id ( a -- ) count [O] attribute& and type ;

\ name structure \xt\link\attribute+count\name_string\
\ link > attribute+count
: named? ( a -- na|0 la ) \ gforth!!!
  @ ( xt) [O] =last       \ start la
  begin dup >r            \ save la
    ( la) @ ( na|0) dup   \ named ?
  while 2dup name> xor    \ match xt ?
    while ( na) cell-     \ next la
      r> drop             \ cleanup
  repeat then nip r> ;    \ na|0 la

: dump ( a n -- )( 15.6.1.1280 ) \ address units
  base h@ >r hex  cells bounds
  begin 2dup xor                    \ end ?
  while cr dup >r named?            \ na|0 la
    @ cell- cell- r@ = if cr then   \ head ?
    r@       [m_cell_m]             \ address
    r@ @ dup [m_cell_m] [m_ascii_m] \ data
    ?dup if space .id then          \ xt name
    r> cell+  nuf?
  until then 2drop  r> base h! cr ;

: >adr ( xt -- a ) [m_>adr_m] ;

: see ( "name" -- )
  ' dup >adr
  begin cell- 2dup @ = until nip \ start of head
  here over - 1 cells / dump ;   \ end of list max

forth-wordlist set-current

: cc ( a n -- ) dump ;
: c ( "name" -- ) see ;

: all ( -- ) e4base here over - 1 cells / dump ;

( wid ) set-current  esee

