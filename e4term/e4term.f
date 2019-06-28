CR .( Terminal and Upload Source Files )
\ ==============================================================
\ todo
\ ==============================================================
\ 20110101 jwr swiftforth problem ONLY ALSO FORTH DEFINITIONS
\ 20080828 bee add com17-com32
\ 20080822 bee shift-F2 clear screen AND scrollback
\ 20080719 jwr file-load display more compact (81 cols, no CRs)
\ 20080719 jwr added delay for FILE word to handle echo
\ 20080709 bee add com9 com10 com11 com12 user default template
\ 20080706 bee usa (usb serial adaptor) autobps startup trouble
\ 20080706 bee change HELP type demo to run "demo_name"
\ 20080706 bee change default to 115kbps as per Skip Inskeep
\ 20080706 bee add baudrates determined by autobps keyspan 19w
\ 20080706 bee 1-2 character delay needed upload some adaptors
\ 20080706 bee ?crash guc232a and i.connect after running random
\ 20070609 bee swiftforth cursor disappears
\ 20070609 bee change to XON for pace char
\ 20070609 bee ansi escape sequences do not work in sf console
\ 20070609 bee add COM+ COM- to change available ports
\ 20070525 bee add ~DTR ~RTS ~BREAK  add status  add to history
\ 20070507 bee add ANSI sequences  BPS-UP/DOWN
\ 20070402 bee more
\ 20070316 bee ok now swiftforth
\ ==============================================================

ONLY FORTH ALSO DEFINITIONS
[DEFINED] M.HOST [IF] M.HOST [THEN] MARKER M.HOST
DECIMAL

INCLUDE C:\Programs\ForthInc\SwiftForth\lib\options\win32\sio.f

\ ==============================================================
$70 ( index Windows function keys )
\ =================================
  enum VK_F1
  enum VK_F2
  enum VK_F3
  enum VK_F4
\ =================================
  enum VK_F5
  enum VK_F6
  enum VK_F7
  enum VK_F8
\ =================================
  enum VK_F9
  enum VK_F10
  enum VK_F11
  enum VK_F12
\ =================================
drop

\ ==============================================================

0 CONSTANT [NMI] IMMEDIATE \ conditional compile NewMicros

\ ==============================================================
\ compatibility + extensions

: =IF ( -- sys ) ( n -- n ( n -- ) \ same as CASE's OF
  ['] OVER COMPILE,  ['] = COMPILE,
  [COMPILE] IF  ['] DROP COMPILE, ; IMMEDIATE

[DEFINED] CTRL 0= [IF]
: CTRL ( 'ccc' -- ) CHAR $01F AND ;
[THEN]
[DEFINED] [CTRL] 0= [IF]
: [CTRL] ( 'ccc' -- ) CTRL [COMPILE] LITERAL ; IMMEDIATE
[THEN]

\ adjust as needed

128 2 + [NMI] [IF] DROP 80 [THEN]
  CONSTANT TIBSIZE \ Input Buffer Size

VARIABLE FBUFN
256 CONSTANT FBUFSIZE
: FBUF[] ( -- ca ) PAD ;
256 CONSTANT FSPECSIZE
: FSPEC[] ( -- ca ) FBUF[] FBUFSIZE + ;

CREATE C#  0 , 81 , \ column, max \ jwr
: ?LF ( -- ) C# CELL+ @ GET-XY DROP U< IF CR THEN ;

\ ==============================================================
: K1 ( -- 1 ) 1 ; \ force a call to waste time
VARIABLE SDA \ Short Delay Adjust
: SD ( u -- ) \ Short Delay
  BEGIN ?DUP WHILE  PAUSE  SDA @
    BEGIN  K1 EXPIRED DROP  ?DUP WHILE 1- REPEAT 1-
  REPEAT ;

: CALIBRATE ( -- ) \ time 100ms adjust for ~100us
  COUNTER 100 +  0 ( ms 0 )
  BEGIN  1+  OVER EXPIRED
  UNTIL ( ms u ) 1000 / SDA !  DROP ;

CALIBRATE \ bbb

: J1 ( u -- ) >R COUNTER R> SD TIMER ; \ 10000 J1 display ms
: J2 ( u -- ) >R COUNTER R> MS TIMER ; \ 1000 J2 display ms
: JJ ( -- ) 10000 J1 ;

\ ==============================================================
\ character definitions

CTRL [ CONSTANT ESC-CHAR  ( $01B ) \ !!! esc[ == CTRL [ CHAR [
CTRL E CONSTANT CMD-CHAR  ( $005 )
CTRL H CONSTANT PACE-CHAR ( $008 ( bs )
CTRL Q CONSTANT XON       ( $011 )
CTRL S CONSTANT XOFF      ( $013 )
CTRL M CONSTANT \r        ( $00D ( cr )
CTRL J CONSTANT \n        ( $00A ( lf )

\ ==============================================================
\ Show/Hide Caret \ 20070527 Rick VanNorman
\ Function: ShowCaret ( a -- x ) \ dllfunctions.f
: +CURSOR ( -- ) 29 0 DO PHANDLE ShowCaret DROP LOOP ;
: -CURSOR ( -- ) PHANDLE HideCaret DROP ;
' -CURSOR  ' +CURSOR  2CONSTANT +-CURSOR
CREATE 'CURSOR  +-CURSOR , ,
: ~CURSOR ( -- ) \ toggle cursor
  'CURSOR 2@ OVER 'CURSOR 2! EXECUTE ;

Function: ShowCursor ( f -- x ) \ mouse pointer
: -MOUSE ( -- ) FALSE ShowCursor DROP ;
: +MOUSE ( -- ) TRUE ShowCursor DROP ;

\ ==============================================================

VARIABLE CHARDELAY \ delay after sending a character
VARIABLE LINEDELAY \ delay after sending a line of characters

0 LINEDELAY !  0 CHARDELAY !

CREATE LSTAT  16 CHARS ALLOT
: LZ ( -- ) LSTAT COUNT 2 SF-STATUS PANE-TYPE ;
: .DEL ( a ca u -- )
  LSTAT APPEND  @ 0 <# BL HOLD # # # #> LSTAT APPEND ;
: .DELAYS ( -- )
  0 LSTAT C! \ clear count
  CHARDELAY S" C=" .DEL  LINEDELAY S" L=" .DEL  LZ ;

: CHARDELAY+ ( -- )
  CHARDELAY @ 1 + 999 MIN CHARDELAY !  .DELAYS ;
: CHARDELAY- ( -- )
  CHARDELAY @ 1 -   0 MAX CHARDELAY !  .DELAYS ;
: LINEDELAY+ ( -- )
  LINEDELAY @ 1 + 999 MIN LINEDELAY !  .DELAYS ;
: LINEDELAY- ( -- )
  LINEDELAY @ 1 -   0 MAX LINEDELAY !  .DELAYS ;

\ ==============================================================
\ serial communication -- swiftforth -- sio.f

: SIOKEY? ( -- f ) (COM-KEY?) ;
: SIOKEY ( -- c ) (COM-KEY) ;
: SIOFLUSH ( -- ) BEGIN SIOKEY? WHILE SIOKEY DROP REPEAT ;
: SIOECHO ( -- ) BEGIN SIOKEY? WHILE SIOKEY EMIT REPEAT ;

: SIOEMIT ( c -- ) CHARDELAY @ SD (COM-EMIT) ;
: SIOTYPE ( ca u -- ) BOUNDS ?DO I C@ SIOEMIT LOOP ;
: SIOCR ( -- ) \r SIOEMIT ;
: SIOSEND ( ca u -- ) SIOECHO  SIOTYPE SIOCR ; \ bbb SIOFLUSH

\ ==============================================================
\ toggle DTR RTS BREAK -- swiftforth -- sio.f

CREATE BSTAT  4 CHARS ALLOT
: -BZ ( -- ) BSTAT 4 BL FILL ;

\ ==============================================================

-BZ \ clear status buffer

\ ==============================================================

: BZ ( -- ) \ display control line status
  BSTAT 3 4 SF-STATUS PANE-TYPE ;
: BZ! ( c u -- ) CHARS BSTAT + C! BZ ; \ set/clear status char

\ RTS Request To Send
: --RTS ( -- ) -RTS [CHAR] R 0 BZ! ;
: ++RTS ( -- ) +RTS BL 0 BZ! ;
' --RTS  ' ++RTS  2CONSTANT +-RTS
CREATE 'RTS  +-RTS , ,
: INIT-RTS ( -- ) +-RTS 'RTS 2! ;
: ~RTS ( -- ) 'RTS 2@ OVER 'RTS 2! EXECUTE ; \ toggle rts

\ DTR Data Terminal Ready
: --DTR ( -- ) -DTR [CHAR] D 1 BZ! ;
: ++DTR ( -- ) +DTR BL 1 BZ! ;
' --DTR  ' ++DTR  2CONSTANT +-DTR
CREATE 'DTR  +-DTR , ,
: INIT-DTR ( -- ) +-DTR 'DTR 2! ;
: ~DTR ( -- ) 'DTR 2@ OVER 'DTR 2! EXECUTE ; \ toggle dtr

\ BREAK \ bbb useful ???
: --BREAK ( -- ) -BREAK BL 2 BZ! ;
: ++BREAK ( -- ) +BREAK [CHAR] B 2 BZ! ;
' ++BREAK  ' --BREAK  2CONSTANT +-BREAK
CREATE 'BREAK  +-BREAK , ,
: INIT-BREAK ( -- ) +-BREAK 'BREAK 2! ;
: ~BREAK ( -- ) \ toggle break
  'BREAK 2@ OVER 'BREAK 2! EXECUTE ;

\ ==============================================================
\ set baud rate -- swiftforth -- sio.f

CREATE BAUD[]  0 , \ baud rate index
\ 110 , 150 , 300 , 600 , 1200 , 2400 , 4800 , \ bbb not useable
    9600 ,  19200 ,  38400 , 57600 ,
  115200 , 230400 , 460800 ,
\ 921600 , \ bbb not useable
HERE BAUD[] - 1 CELLS / 1 - CONSTANT #BAUDS
: !BAUD ( n -- ) \ index to bps
  DUP BAUD[] ! CELLS BAUD[] + @ BAUD ;

: BPS+ ( -- ) BAUD[] @ 1+ #BAUDS MIN !BAUD ;
: BPS- ( -- ) BAUD[] @ 1- 1 MAX !BAUD ;

\ ==============================================================
\ set com port -- swiftforth -- sio.f

SERIALPORT +ORDER
  COMNAME: COM9    COMNAME: COM10
  COMNAME: COM11   COMNAME: COM12
  COMNAME: COM13   COMNAME: COM14
  COMNAME: COM15   COMNAME: COM16
  COMNAME: COM17   COMNAME: COM18
  COMNAME: COM19   COMNAME: COM20
  COMNAME: COM21   COMNAME: COM22
  COMNAME: COM23   COMNAME: COM24
  COMNAME: COM25   COMNAME: COM26
  COMNAME: COM27   COMNAME: COM28
  COMNAME: COM29   COMNAME: COM30
  COMNAME: COM31   COMNAME: COM32
SERIALPORT -ORDER

CREATE COM[]  0 , 0 ,
  0 , ' COM1  ,  0 , ' COM2  ,  0 , ' COM3  ,  0 , ' COM4  ,
  0 , ' COM5  ,  0 , ' COM6  ,  0 , ' COM7  ,  0 , ' COM8  ,
  0 , ' COM9  ,  0 , ' COM10 ,  0 , ' COM11 ,  0 , ' COM12 ,
  0 , ' COM13 ,  0 , ' COM14 ,  0 , ' COM15 ,  0 , ' COM16 ,
  0 , ' COM17 ,  0 , ' COM18 ,  0 , ' COM19 ,  0 , ' COM20 ,
  0 , ' COM21 ,  0 , ' COM22 ,  0 , ' COM23 ,  0 , ' COM24 ,
  0 , ' COM25 ,  0 , ' COM26 ,  0 , ' COM27 ,  0 , ' COM28 ,
  0 , ' COM29 ,  0 , ' COM30 ,  0 , ' COM31 ,  0 , ' COM32 ,
HERE COM[] - 2 CELLS / 1 - CONSTANT #COMS
: >COM ( n -- a ) 2* CELLS COM[] + ;
: !COM ( n -- )
  DUP >COM 2@ IF EXECUTE ELSE DROP THEN COM[] ! ;

: COM? ( n -- f ) DUP [ #COMS 1 + ] LITERAL 1 WITHIN ;

: COM+ ( -- ) \ next available com port
  COM[] @ \ current com port
  BEGIN 1+ COM? IF DROP 1 THEN DUP >COM @
  UNTIL !COM +COM ;

: COM- ( -- ) \ previous available com port
  COM[] @ \ current com port
  BEGIN 1- COM? IF DROP #COMS THEN DUP >COM @
  UNTIL !COM +COM ;

: !!BAUD ( -- ) 5 !BAUD ; \ 5=115kbps
: CCOM ( -- ) \ find available com ports
  ." Available COM ports: " N,8,1  0
  BEGIN DUP #COMS U<
  WHILE 1+ DUP >COM CELL+ @ EXECUTE ['] !!BAUD CATCH
    IF 0 ELSE DUP . DUP COM[] ! DUP THEN OVER >COM !
  REPEAT DROP   -COM   COM[] @ !COM ;

\ ==============================================================

CCOM \ find available com ports

\ ==============================================================
\ file upload

VARIABLE FID \ File ID

: .ERR ( -- ) ."  Error " ABORT ;
: F.ERR ( -- ) ." -File" .ERR ;

: ?OPEN-FILE ( ca u -- )
  R/O OPEN-FILE IF ."  Open" F.ERR THEN FID ! ;

: ?CLOSE-FILE ( -- )
  FID @ ?DUP
  IF CLOSE-FILE IF CR ." Close" F.ERR THEN  0 FID ! THEN ;

: READ-LINE? ( -- u f )
  FBUF[] TIBSIZE FID @ READ-LINE
\ drop -1 \ bbb testing force read-line error
  IF CR ." Read-Line" .ERR THEN ;

CREATE LINE#  0 , 28 , \ current line number, mod lines for cr
: ?LINE ( u -- u )
  1 LINE# +!  TIBSIZE OVER U<
  IF CR LINE# ? ." Line Too Long" .ERR THEN ;

: PROGRESS ( -- ) [CHAR] . EMIT   ?LF  ;
: CR.OK ( -- ) CR ." Upload Finished " ;
: ?AT-0Y ( -- ) \ no CR at start of line
  GET-XY DROP IF CR THEN ;

\ ==============================================================
\ choose file dialog -- swiftforth -- ofn.f

OFN-DIALOGS +ORDER

OFN-DIALOG SUBCLASS UPLOAD-FILE-DIALOG

: CUSTOM ( -- title filter flags )
  Z" Upload File"  ALL-FILES  DEFAULT-OPEN-FLAGS ;

END-CLASS

: CHOOSE-FILE ( -- ca u )
  [OBJECTS
    UPLOAD-FILE-DIALOG MAKES UFD
  OBJECTS]
  UFD CHOOSE
  IF  UFD  FILENAME ZCOUNT EXIT
  THEN FSPEC[] 0 ;

OFN-DIALOGS -ORDER

\ ==============================================================
\ add a string to or clear history -- swiftforth -- editline.f

ACCEPTOR +ORDER

\ add a line to the front of the history buffer
: +HISTORY ( ca u -- ) PUSHLINE ;

\ clear the history buffer
: CLEAR-HISTORY ( -- ) LBUF |LBUF| ERASE ;

ACCEPTOR -ORDER

\ ==============================================================
\ paced upload

: ?USER ( -- )
  EKEY? \ user key press?
  IF EKEY ESC-CHAR =
    IF CR ." User Abort  " ABORT THEN
  THEN ;

: ?PACE ( -- )
  BEGIN
    BEGIN  ?USER  SIOKEY?  \ target response?
    UNTIL   SIOKEY
    DUP XON XOR   \ target ready for more input
  WHILE DUP 0=    \ target sends 0 (null) on error
    IF SIOECHO
      CR FBUF[] FBUFN @ TYPE
      CR ." Error at Line " LINE# @ U. SPACE ABORT
    THEN
    DUP XOFF XOR IF DUP EMIT THEN DROP \ target output echo
  REPEAT DROP ;

: _PUPLOAD ( -- )
  0 LINE# !
  BEGIN ?PACE  READ-LINE?
  WHILE  DUP FBUFN ! ?LINE  FBUF[] SWAP SIOSEND  PROGRESS
  REPEAT DROP CR.OK ;

: SIOSENDKEYS ( ca u -- ) \ must allow for echo
  20 CHARDELAY DUP @ >R +! SIOSEND R> CHARDELAY ! ;

: PUPLOAD ( ca u -- ) \ paced upload
  DUP IF  2DUP ?OPEN-FILE  +HISTORY \ bbb
    S"  FILE" SIOSENDKEYS ['] _PUPLOAD CATCH DROP
    LINEDELAY @ MS S"  [ HAND " SIOSEND
    ?CLOSE-FILE  EXIT
  THEN 2DROP SIOCR ;

: PUPCAT ( ca u -- ) ['] PUPLOAD CATCH IF 2DROP THEN ;
: CUPLOAD ( -- ) ( ?AT-0Y) CHOOSE-FILE PUPCAT ; \ jwr
: >UPLOAD ( -- )
  ?AT-0Y CD ." > " FSPEC[] DUP FSPECSIZE ACCEPT PUPCAT ;

\ ==============================================================
\ timed upload

: _TUPLOAD ( ca u -- )
  PAGE  ?OPEN-FILE   0 LINE# !
  BEGIN ?USER  READ-LINE?
  WHILE  ?LINE  SIOECHO  LINEDELAY @ MS  FBUF[] SWAP SIOSEND
    LINE# 2@ SWAP MOD 0= IF PAGE THEN
  REPEAT DROP  SIOECHO  CR.OK ;

: TUPLOAD ( ca u -- )
  ['] _TUPLOAD CATCH IF 2DROP THEN ?CLOSE-FILE ;

\ ==============================================================
\ binary upload

128 CONSTANT #IBUF \ remote buffer size

: XPAD ( -- ) PAD  #IBUF 4 * 0 DO I  OVER I + C! LOOP DROP ;

: XB ( a u -- )
  DUP SIOEMIT  BOUNDS DO I C@ SIOEMIT SIOECHO ?LF LOOP ;

0 [IF]
bbb 2 byte count 65535 * 128 = 8,388,480 bytes
bbb 1 byte count 255 * 256 = 65280 bytes
bbb 1 byte count 255 * 128 = 32640 bytes
c18 (16 * 1024 * 18) / 8 = 36864 bytes packed
c18 (16 * 1024 * 3) = 49152 bytes
72/18=4 72/8=9 every 9 bytes it is even 36864 / 9 = 4096 evens
14 * 9 = 126 / 18 = 7 words per 128 buffer
9bits * 8 = 72

\ 4 18bit words packed in 9bytes(72bits)
\ : j6 ( n1 n2 n3 n4 -- )
\   base @ >r hex   >r >r >r \ assume 18bits
\   dup $03 and &18 lshift r> or
\     swap &2 rshift $100 /mod xout xout
\   dup $0f and &18 lshift r> or
\     swap &4 rshift $100 /mod xout xout
\   dup $3f and &18 lshift r> or
\     swap &6 rshift $100 /mod xout xout
\   dup $ff and
\     swap &8 rshift $100 /mod xout xout
\   xout r> base ! ;

\ lcd == least common denominator for 18bit streams
: xout ( n -- ) 0 <# # # #> type space ;

\ 4 18bit words packed in 9bytes(72bits)
: j7 ( n1 n2 n3 n4 -- )
  base @ >r hex   >r >r >r \ assume 18bits
  dup &2 rshift $100 /mod xout xout  $03 and &18 lshift r> or
  dup &4 rshift $100 /mod xout xout  $0f and &18 lshift r> or
  dup &6 rshift $100 /mod xout xout  $3f and &18 lshift r> or
  dup &8 rshift $100 /mod xout xout  $ff and xout   r> base ! ;

[THEN]

: XBUF ( -- ) \ test
  XPAD
  S"  BINX " SIOSEND \ send command
  PAD #IBUF
  96  DUP SIOEMIT  0 \ 255 * 128 = 32640 bytes max
  ?DO 2DUP XB  1 /STRING \ one less each loop
  LOOP 2DROP
  SIOCR S"  IBUF[] 200 DUMP " SIOSEND ;

0 [IF]

\ 11.6.1.1522   FILE-SIZE   FILE ( fileid -- ud ior )
\ 11.6.1.2080   READ-FILE   FILE ( c-addr u1 fileid -- u2 ior )

: BUPLOAD ( ca u -- )
  DUP IF  2DUP ?OPEN-FILE  +HISTORY \ bbb
    FID @ FILE-SIZE ( ud ior ) IF S" FILE-SIZE error" .ERR THEN
      DUP IF S" FILE-SIZE no way that big" .ERR THEN
      ( ud ) #IBUF UM/MOD SWAP
      IF 1+ THEN DUP SIOEMIT 0 \ number of buffers
      ?DO FBUF[] DUP #IBUF READ-FILE DUP SIOEMIT SIOTYPE
      LOOP  ?CLOSE-FILE  EXIT
  THEN 2DROP ;

: BUPCAT ( ca u -- ) ['] BUPLOAD CATCH IF 2DROP THEN ;
: BUPLOAD ( -- ) ?AT-0Y CHOOSE-FILE BUPCAT ;
: >BUPLOAD ( -- )
  ?AT-0Y CD ." > " FSPEC[] DUP FSPECSIZE ACCEPT BUPCAT ;

[THEN]

\ ==============================================================
\ user interface

: CHAR-MASK ( u -- c ) $0FF AND ;
$10000 CONSTANT KEY_SCAN    $20000 CONSTANT KEY_CTRL
$40000 CONSTANT KEY_SHIFT   $80000 CONSTANT KEY_ALT

\ menu no modifier
: KF1 ( -- ) ." | F1    F2    F3   F4  |" ;
: TF1 ( -- ) ." | HELP  PAGE  ..   ..  |" ;
: KF5 ( -- ) ." | F5     F6     F7    F8   |" ;
: TF5 ( -- ) ." | ~RTS   ~DTR   ..    ..   |" ;
: KF9 ( -- ) ." | F9      F10  F11  F12  |" ;
: TF9 ( -- ) ." | UPLOAD  ..   ..   EXIT |" ;
: HELP-MENU ( -- ) ?AT-0Y KF1 KF5 KF9 CR TF1 TF5 TF9 CR ;

: _KEY ( u -- ) \ function key
  CASE CHAR-MASK
    VK_F1  OF HELP-MENU               ENDOF
    VK_F2  OF PAGE                    ENDOF
    VK_F3  OF                         ENDOF
    VK_F4  OF                         ENDOF
\ =========================================
    VK_F5  OF ~RTS                    ENDOF
    VK_F6  OF ~DTR                    ENDOF
    VK_F7  OF                         ENDOF
    VK_F8  OF                         ENDOF
\ =========================================
    VK_F9  OF CUPLOAD                 ENDOF
    VK_F10 OF                         ENDOF
    VK_F11 OF                         ENDOF
    VK_F12 OF ( swiftforth ) 0=       ENDOF
  ENDCASE ;

\ menu shift modifier
: $KF1 ( -- ) ." | $F1   $F2   $F3  $F4 |" ;
: $TF1 ( -- ) ." | HELP  PAGE  ..   ..  |" ;
: $KF5 ( -- ) ." | $F5    $F6    $F7   $F8  |" ;
: $TF5 ( -- ) ." | -CHAR  -LINE  -COM  -BPS |" ;
: $KF9 ( -- ) ." | $F9  $F10  $F11  $F12 |" ;
: $TF9 ( -- ) ." | ..   ..    ..    ..   |" ;
: $HELP-MENU ( -- ) ?AT-0Y $KF1 $KF5 $KF9 CR $TF1 $TF5 $TF9 CR ;

: _KEY_SHIFT ( u -- ) \ function and shift key
  CASE CHAR-MASK
    VK_F1  OF $HELP-MENU              ENDOF
    VK_F2  OF WIPE                    ENDOF
    VK_F3  OF                         ENDOF
    VK_F4  OF                         ENDOF
\ =========================================
    VK_F5  OF CHARDELAY-              ENDOF
    VK_F6  OF LINEDELAY-              ENDOF
    VK_F7  OF COM-                    ENDOF
    VK_F8  OF BPS-                    ENDOF
\ =========================================
    VK_F9  OF                         ENDOF
    VK_F10 OF                         ENDOF
    VK_F11 OF                         ENDOF
    VK_F12 OF                         ENDOF
  ENDCASE ;

\ menu control modifier
: ^KF1 ( -- ) ." | ^F1   ^F2   ^F3  ^F4 |" ;
: ^TF1 ( -- ) ." | HELP  PAGE  ..   ..  |" ;
: ^KF5 ( -- ) ." | ^F5    ^F6    ^F7   ^F8  |" ;
: ^TF5 ( -- ) ." | +CHAR  +LINE  +COM  +BPS |" ;
: ^KF9 ( -- ) ." | ^F9  ^F10  ^F11  ^F12 |" ;
: ^TF9 ( -- ) ." | ..   ..    ..    ..   |" ;
: ^HELP-MENU ( -- ) ?AT-0Y ^KF1 ^KF5 ^KF9 CR ^TF1 ^TF5 ^TF9 CR ;

: _KEY_CTRL ( u -- ) \ function and control key
  CASE CHAR-MASK
    VK_F1  OF ^HELP-MENU              ENDOF
    VK_F2  OF PAGE                    ENDOF
    VK_F3  OF                         ENDOF
    VK_F4  OF                         ENDOF \ S"  WORDS" SIOSEND
\ =========================================
    VK_F5  OF CHARDELAY+              ENDOF
    VK_F6  OF LINEDELAY+              ENDOF
    VK_F7  OF COM+                    ENDOF
    VK_F8  OF BPS+                    ENDOF
\ =========================================
    VK_F9  OF                         ENDOF
    VK_F10 OF                         ENDOF
    VK_F11 OF                         ENDOF
    VK_F12 OF                         ENDOF
  ENDCASE ;

: ?HOST ( 0 c -- f )
  DUP [ $07F INVERT ] LITERAL AND \ function key ?
  IF DUP KEY_SHIFT AND IF _KEY_SHIFT EXIT THEN
     DUP KEY_CTRL  AND IF _KEY_CTRL  EXIT THEN
     _KEY EXIT
  THEN  SIOEMIT ;

\ ==============================================================
\ ansi escape sequences -- swiftforth

: DIGIT? ( c base -- u f ) \ try to convert a char to a number
  >R [CHAR] 0 - 9 OVER <
  IF -7 + DUP 10 < OR THEN DUP R> U< ;

: MORE-DIGITS ( u -- u' f ) \ collect digits and form a number
  BEGIN SIOKEY  DUP 10 DIGIT? \ valid decimal digit ?
  WHILE NIP  SWAP 10 * +  REPEAT DROP ;

: ESC[M ( u*i -- ) \ display attributes
   0 =IF NORMAL  EXIT  THEN \ t18 reset all, black on white
   1 =IF BOLD    EXIT  THEN \ t18 blue on white
   7 =IF INVERSE EXIT  THEN \ t18 white on black
  37 =IF 41 =IF BRIGHT  EXIT THEN DROP 1
     THEN                   \ t18 white on red or White
  30 =IF 0 ( sf translation )       THEN \ Black
  31 =IF 2 ( sf translation )       THEN \ Red
  32 =IF 7 ( sf translation )       THEN \ Green
  33 =IF 6 ( sf translation )       THEN \ Yellow
  34 =IF 3 ( sf translation )       THEN \ Blue
  35 =IF 5 ( sf translation )       THEN \ Magenta
  36 =IF 4 ( sf translation )       THEN \ Cyan
  DUP 0 16 WITHIN IF ATTRIBUTE EXIT THEN \ swiftforth specific
  THROW ; \ unknown value, unknown stack depth

: ANSI? ( u*i c -- u*j f )
  [CHAR] H =IF SWAP AT-XY        -1 EXIT THEN \ Home
  [CHAR] ; =IF 0                  0 EXIT THEN \ more attributes
  [CHAR] J =IF 2 = IF PAGE THEN  -1 EXIT THEN \ clear
  [CHAR] m =IF ESC[M             -1 EXIT THEN \ set attributes
  THROW ; \ unknown char, unknown stack depth

: _ANSI ( -- ) 0 BEGIN MORE-DIGITS ANSI? UNTIL ;

: ?LISTEN ( f c -- f )  \ listen for commands
  ESC-CHAR OVER =       \ start ansi escape ?
  IF SWAP IF DUP EMIT THEN  EXIT
  THEN   OVER           \ previous escape ?
  IF [CHAR] [ OVER =    \ really ansi escape ?
    IF 2DROP  ['] _ANSI CATCH DROP  0 EXIT
    THEN >R  EMIT  0  R>
  THEN DUP EMIT  BL = IF ?LF THEN ;

\ ==============================================================
\ start terminal

: TALK-OFF ( -- ) -COM +CURSOR NORMAL ;
: TALK-ON ( -- )
  TALK-OFF  INIT-DTR  INIT-RTS  INIT-BREAK  -BZ BZ
  .DELAYS  +COM ;

: TT ( -- ) \ start terminal
  BASE @ DECIMAL  ." terminal "  TALK-ON  0 ( ansi escape flag )
  BEGIN
    BEGIN  SIOKEY? IF SIOKEY ?LISTEN THEN  EKEY?
    UNTIL  0 EKEY ?HOST
  UNTIL  DROP  TALK-OFF  CR ." SwiftForth "  BASE ! ;

: HELP ( -- ) CR ." Type  TT  to start terminal " ;

\ ==============================================================

CR HELP CR \ display help

\ ==============================================================
\ user default baud rate and comport

\ 115200 com= com5
\ 0 CHARDELAY !

\ ==============================================================

