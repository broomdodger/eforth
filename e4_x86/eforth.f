CR .( ++++ eForth btc x86 load file ++++ )
\ ==============================================================
\ todo
\ ==============================================================
\ 20110125 bee add io.e4 and division macro, change filenames
\ 20110115 bee remove E4- added to >IMAGE
\ 20110114 bee add see.f  init 'LAST
\ 20090729 bee add INCLUDE"
\ 20090727 bee add E4- to TC! TC@
\ 20070917 bee move BITS/CELL  change to SAVEBIN
\ 20070916 bee add include code.lib.e4
\ 20070901 bee add +include"
\ 20070822 bee folder navigation
\ 20070720 bee separate
\ 20070518 bee for updated t18  add ALL-RAM  other files changed
\ 20070518 bee move ALL.NODES.HEX  SAVE.E4
\ 20070420 bee compile both c18 and x86 always
\ 20070419 bee gforth try again grrr and tame
\ 20070411 bee changes to move to nodes 0-3
\ 20070330 bee move compiler switch here
\ 20070326 bee cleanup
\ 20061210 bee start
\ ==============================================================

DECIMAL

GET-CURRENT ( wid )

WORDLIST CONSTANT E4-WORDLIST       ( eForth internal words )
E4-WORDLIST +ORDER
E4-WORDLIST SET-CURRENT

INCLUDE" cpu.e4"                    ( cpu definition )
INCLUDE" ../../../e4meta/meta.e4"   ( meta compiler )

D# 6                                ( warn if over... )
D# 1024 * D# 4 * !IMAGE             ( MAX size of binary image )

\ assembler memory access
: TC! ( n a -- ) >IMAGE C! ;
: TC@ ( a -- n ) >IMAGE C@ ;
: TC,   ( n -- ) HERE[]  1 ALLOT[]  TC! ;
: THERE ( -- a ) HERE[] ;

INCLUDE" ../../assembler.e4"        ( assembler )
INCLUDE" ../../metacpu.e4"          ( meta extensions )

\ floored or symmetric division
\ :I [m_fm/sm_m] ( -- ) S" SM/REM" EVALUATE ;
  :I [m_fm/sm_m] ( -- ) S" FM/MOD" EVALUATE ;

CR .( //// Metacompile Begin \\\\ )
META[   INTEL

  INCLUDE" code.e4"                 ( code source )
  INCLUDE" code_io.e4"              ( io code source )
\ INCLUDE" usercode.e4"             ( user extension code )
  INCLUDE" ../../../e4meta/list.e4" ( list source )
  INCLUDE" ../../list_io.e4"        ( io list source )
  INCLUDE" ../../userlist.e4"       ( user extension list )

  HERE    =DP   ! ( init dictionary pointer )
  LATEST  =LAST ! ( init search order and see.f )

]META
CR .( \\\\ Metacompile End //// )

( wid ) SET-CURRENT

DECIMAL

S" eforth.com" SAVEBIN

INCLUDE" ../../../e4meta/see.f"     ( target decompiler )

CR .( eForth saved )
CR .( Project Complete )

