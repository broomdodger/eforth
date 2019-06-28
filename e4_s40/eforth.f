CR .( ++++ eForth btc s40 load file ++++ )
\ ==============================================================
\ todo
\ ==============================================================
\ 20110125 bee add io.e4 and division macro, change filenames
\ 20090729 bee add INCLUDE", LE@ fix
\ 20090726 bee remove EMPTY, move csp ?lf to platform files
\ 20081101 jwr comment style change, write two eforth.bin files
\ 20081101 jwr change file paths for changed folder location
\ 20081101 jwr code to restore Bill's automatic XT assignment
\ 20080817 bee change SAVEFILE setup pointers
\ 20080713 jwr consistant line-lengths and eForth spelling
\ 20080711 bee move init MAX binary size from code.e4
\ 20080711 bee move init 'COLD from list.e4 to meta load
\ 20080711 bee add user.e4 to meta load list
\ 20080706 jwr add PWD
\ 20080705 bee CSP check both SP@ and tos
\ 20080704 jwr save eforth.bin to okad ( e4load ) folder
\ 20080702 bee start
\ ==============================================================

ONLY  FORTH  DEFINITIONS

DECIMAL

WARNING ON

MARKER M.EFORTH

0 [IF] ---------------------------------------------------------
 Once the following settings match your board, chip, and usage
 they won't need to be changed.

 Make sure that the nodes chosen for TERM, BITSY, STACK, and
 SDRAM are right
--------------------------------------------------------- [THEN]

10 constant TERM  \ must have two I/O pins
11 constant BITSY
12 constant STACK
13 constant SDRAM \ determined by s40's parallel ports
                  \ and controller code

\ ==============================================================
\ eForth meta compiler

GET-CURRENT ( wid )

WORDLIST CONSTANT E4-WORDLIST ( eForth internal words )
E4-WORDLIST +ORDER
E4-WORDLIST SET-CURRENT

INCLUDE" cpu.e4"              ( cpu definition )
INCLUDE" ../e4meta/meta.e4"   ( meta compiler )
INCLUDE" metacpu.e4"          ( meta extensions )

D# 5                          ( warn if over... )
D# 1024 * CELLS !IMAGE        ( MAX size of binary image )

\ floored or symmetric division
\ :I [m_fm/sm_m] ( -- ) S" SM/REM" EVALUATE ;
  :I [m_fm/sm_m] ( -- ) S" FM/MOD" EVALUATE ;

CR .( //// Metacompile Begin \\\\ )
META[   INTEL                 ( low-byte at low/even address )

  INCLUDE" code.e4"           ( code source )
  INCLUDE" code_io.e4"        ( io code source )
\ INCLUDE" usercode.e4"       ( user extension code source )
  INCLUDE" ../e4meta/list.e4" ( list source )
  INCLUDE" list_io.e4"        ( io list source )
  INCLUDE" userlist.e4"       ( user extension list source )

  HERE    =DP   !             ( init dictionary pointer )
  LATEST  =LAST !             ( init search order and see.f )

]META
CR .( \\\\ Metacompile End //// )

( wid ) SET-CURRENT

DECIMAL

IADDR @  FSIZE @  S" eforth.bin" SAVEFILE

INCLUDE" ../e4meta/see.f"     ( target decompiler )

CR .( eForth saved )
CR .( Project Complete )

