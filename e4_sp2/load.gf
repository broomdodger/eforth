CR .( ++++ eForth load file for gforth ++++ )
\ ==============================================================
\ todo
\ ==============================================================
\ 20090729 bee add INCLUDE"
\ 20090726 bee update
\ ==============================================================

ONLY  FORTH  DEFINITIONS

DECIMAL

\ ==============================================================
\ a quieter gforth

' noop is dobacktrace \ limit error messages ( *** )
' noop is bootmessage \ no signon message
' noop is .status     \ info after the 'ok' ( noop is default )

\ fix CRITICAL gforth bug
: INCLUDED ( ca u -- ) \ prevent error logout, fix gforth bug
  ['] INCLUDED CATCH ?DUP IF DOERROR QUIT THEN ;

: INCLUDE ( 'ccc' -- ) PARSE-WORD INCLUDED ; \ fix gforth bug

: INCLUDE" ( 'ccc'" -- ) [CHAR] " PARSE INCLUDED ;

fpath+ ~+/

\ ==============================================================

INCLUDE" ../e4meta/platform.gf"
INCLUDE" ../e4meta/radixio.f"
INCLUDE" ../e4meta/savefile.f"
INCLUDE" ../e4meta/savehex.f"
INCLUDE" ../e4meta/nuf.f"
INCLUDE" eforth.f"
