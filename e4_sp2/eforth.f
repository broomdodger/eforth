CR .( ++++ eForth btc sp2 load file ++++ )
\ ==============================================================
\ todo
\ ==============================================================
\ 20110125 bee add io.e4 and division macro, change filenames
\ 20090729 bee add INCLUDE"
\ 20090726 bee move !IMAGE from code.e4
\ 20070824 bee remove common loads  savefile.f dothex.f
\ 20070723 bee combine metacpu.e4 and code.e4
\ 20070707 bee sp2 c18
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

WORDLIST CONSTANT E4-WORDLIST ( eForth internal words )
E4-WORDLIST +ORDER
E4-WORDLIST SET-CURRENT

INCLUDE" cpu.e4"              ( cpu definition )
INCLUDE" ../e4meta/meta.e4"   ( meta compiler )

D# 8                          ( warn if over... )
D# 1024 * D# 4 * !IMAGE       ( max target image size in bytes )

\ assembler memory access
: @[a] ( a -- n ) AU* @[]  DATABIT& AND ;
: ![a] ( n a -- ) >R       DATABIT& AND  R> AU* ![] ;

INCLUDE" assembler.e4"        ( assembler )
INCLUDE" metacpu.e4"          ( meta extensions )

\ floored or symmetric division
\ :I [m_fm/sm_m] ( -- ) S" SM/REM" EVALUATE ;
  :I [m_fm/sm_m] ( -- ) S" FM/MOD" EVALUATE ;

CR .( //// Metacompile Begin \\\\ )
META[   INTEL                 ( low-byte at low/even address )

  INCLUDE" code.e4"           ( code source )
  INCLUDE" code_io.e4"        ( io code source )
\ INCLUDE" usercode.e4"       ( user optimization code source )
  INCLUDE" ../e4meta/list.e4" ( list source )
  INCLUDE" list_io.e4"        ( io init list source )
  INCLUDE" userlist.e4"       ( user extension list source )

  HERE    =DP   !             ( init dictionary pointer )
  LATEST  =LAST !             ( init search order and see.f )

]META
CR .( \\\\ Metacompile End //// )

( wid ) SET-CURRENT

DECIMAL

0  0  5 1024 *  S" eforth.hex" SAVEHEX

INCLUDE" ../e4meta/see.f"     ( target decompiler )

CR .( eForth saved )
CR .( Project Complete )

