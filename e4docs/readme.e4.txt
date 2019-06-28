Owner:  Bill Muench <forth@calcentral.com>
Second: John Rible <e4th@sandpipers.com>

\ ==============================================================
  Bill's notes are marked with *bbb*
    usb serial adaptor problems
\ ==============================================================

eForth and eForth Terminal Application -- Engineering Version

WARNING: This software is an engineering version.
         It has deficiencies, some of which are due to the
         VentureForth console window.

         No claims are made.
         Use at your own risk.

WARNING: Currently it is not possible to program
         the IntellaSys s40 nodes directly from eForth.

The interface is command-line, there is no GUI.

\ ==============================================================

For extensive information about Forth, refer to the
ANS Programming Language -- Forth (dpans94.pdf)

\ ==============================================================

Installation
------------

1) Expand   e4distYYYYMMDD.zip
   After expanding, you should have a folder named   e4dist
   with the file/folder structure listed below.

2) Connect the s40 board(s) to your PC with
   a) USB to mini-USB cable for normal S40 interactions
   b) USB to serial cable for eForth terminal operations.

3) Double-click   ...\???   to run the
   eForth terminal emulator.
   SwiftForth will start and compile the terminal application.
   At the prompt type:  TT
   See notes below on usage.

4) Double-click  ...\e4cf\Okad2.exe   to start the
   eForth loader in colorForth:

WARNING: Close any previous Okad2.exe and its companion file.
If more than one instance is running the upload will not work.

5) Compile the node source code and load the eforth.bin file by
   typing  116 load <enter>  into the large, colorful, window.
   There is no cursor, but you will see the text you type. This
   should take several seconds, during which time the three
   yellow LEDs on the s40 board will blink. The operation is
   complete when the Okad2.exe display changes, now showing she
   IDE panel.

6) Switch back to the terminal emulator window, press the space
   bar, and the eForth version is displayed:

     eForth btc:20080713 c18:20080925
     ok

7) Enter eForth source in the terminal window and/or run the IDE
   in the ColorForth window. Enjoy!

Distribution software
---------------------
e4distYYYYMMDD.zip -- eForth distribution package

Included in the eForth distribution package:

e4dist:             -- eForth distribution package folder
  e4cf:               -- ColorForth loader and IDB
    e4cf.log            -- record of development changes
    cForth2.ico         -- colorForth icon
    eforth.bin          -- external ram image
    Okad2.exe           -- The colorForth runtime
    OkadWork.cf         -- code words, s40 drivers, IDB code
  e4docs:             -- documents, reference materials
    dpans94.pdf         -- ANS Programming Language -- Forth
    e4.readme.txt       -- This readme file
    lib.e4              -- reference of common extensions
    s40poster.A1.pdf    -- Poster overview of the s40 chip
  e4term:             -- terminal distribution folder
    bps-test.e4         -- test: baud-rate testing code
    bps-test.log        -- test: baud-rate data from tests
    e4term.f            -- terminal load file
    e4term.log          -- record of development changes
    filldump.e4         -- test: memory read and write words
    hanoi.e4            -- demo: Move disks between three poles
    lib.e4              -- DO-LOOP and color for demos
    random.e4           -- demo: Random position char and color
    tetris.e4           -- demo: Tetris the game
  eforth:             -- high-level source and meta-compiler
    code.e4             -- cpu specific code and equates
    cpu.e4              -- cpu specific hardware declarations
    eforth.f            -- main load file, platform extensions
    eforth.log          -- record of development changes
    list.e4             -- high level source
                           For more Forth information,
                           see e4docs/dpans94.pdf
    meta.e4             -- eForth meta compiler
    metacpu.e4          -- meta compiler cpu extensions
    user.e4             -- source for user metacompilation

Terminal emulator *bbb*
-----------------
WARNING: Most USB Serial Adaptors work properly, although they
may need help with file downloads (see below). If yours does
not, see the comments in the e4term/bps-test.e4 and
eforth/user.e4 files. The Keyspan products seem to work well.
The cables with drivers by   Prolific   seem to give problems.

On startup, the terminal emulator checks for available com ports
and by default selects the highest numbered available com port.
It also defaults to 115kbps to match the compiled eForth image.

There are three levels of help, F1, Control-F1 and Shift-F1.
  F1          -- help to: UPLOAD
  Control-F1  -- help to: increase line parameters
  Shift-F1    -- help to: decrease line parameters
  Escape      -- key will abort an upload

\ ==============================================================
1) Function keys with NO modifier

    F1  HELP    -- two lines of minimal help
    F2  PAGE    -- clear screen
    F3          -- not used by s40
    F4          -- not used by s40
    F5  ~RTS    -- toggle rts, currently not used by s40
    F6  ~DTR    -- toggle dtr, currently not used by s40
    F7          -- not used by s40
    F8          -- not used by s40
    F9  UPLOAD  -- open send file dialog
    F10         -- not used by s40
    F11         -- not used by s40
    F12 EXIT    -- exit terminal, return to SwiftForth

\ ==============================================================
2) Function keys with CONTROL key modifier

   ^F1  HELP    -- two lines of minimal help
   ^F2  PAGE    -- clear screen
   ^F3          -- not used by s40
   ^F4          -- not used by s40
   ^F5  +CHAR   -- increase character transmission delay
   ^F6  +LINE   -- increase line transmission delay
   ^F7  +COM    -- select the next higher available com port
   ^F8  +BPS    -- select the next higher standard baud rate
   ^F9          -- not used by s40
   ^F10         -- not used by s40
   ^F11         -- not used by s40
   ^F12         -- not used by s40

\ ==============================================================
3) Function keys with SHIFT key modifier

   $F1  HELP    -- two lines of minimal help
   $F2  PAGE    -- clear screen
   $F3          -- not used by s40
   $F4          -- not used by s40
   $F5  -CHAR   -- decrease character transmission delay
   $F6  -LINE   -- decrease line transmission delay
   $F7  -COM    -- select the next lower available com port
   $F8  -BPS    -- select the next lower standard baud rate
   $F9          -- not used by s40
   $F10         -- not used by s40
   $F11         -- not used by s40
   $F12         -- not used by s40

\ ==============================================================
NOTE: Character and line delays are "short delays"
      only roughly approximating a millisecond.
      They should normally remain set to 0.
\ ==============================================================

Change terminal settings
------------------------
The bottom of the VentureForth window has a status bar.

Before typing "TT" it will look something like:
--------------------------------------------------------------
| <tos         |Dec  |               | Inactive    |   | ovr |
--------------------------------------------------------------

After typing "TT" it will look something like:
--------------------------------------------------------------
| <tos         |Dec  | C=000 L=000   | COM3:115200 |   | ovr |
--------------------------------------------------------------

The new entries in the status bar are:
C=000   -- current character delay (may need change, see below)
L=000   -- current line delay (should not be changed)
COM3    -- current com port (your port number may be different)
115200  -- current bps

If the com port listed in the status bar is not correct:
  use Control-F7 to increase the port number, or
  use Shift-F7   to decrease the port number

Prior to typing "TT" type "COM5 TT" to select com port 5 and
enter terminal.

The VentureForth NAMED com ports are:
  COM1   COM2   COM3   COM4   COM5   COM6
  COM7   COM8   COM9   COM10  COM11  COM12

Trouble shooting serial adaptors *bbb*
--------------------------------
If the   UPLOAD   of a demo file fails, resulting in only one or
more of the characters from the word "FILE" displayed, press the
Escape key to abort the upload and use Control-F5 to increase
the character delay to 1 or 2 and try the upload again.

\ ==============================================================

Running the demo applications
-----------------------------
lib.e4      -- Adds the DO LOOP structure,
               needed for the following demos

hanoi.e4    -- Move disks between three poles
random.e4   -- Random positioning of character and color
tetris.e4   -- Tetris the game

Description
-----------
eForth is a minimal Forth system. There are about 30 primitive
words coded in VentureForth on three IntellaSys s40 nodes. In
addition, the external memory interface uses five IntellaSys s40
nodes. The remainder of eForth is made of high level calls to
those primitives.

For program control, the eForth kernel has ONLY the following
words:

  IF ELSE THEN AHEAD  BEGIN UNTIL WHILE REPEAT AGAIN

Some of the demo applications need counted loops, such as:

  DO LOOP LEAVE UNLOOP

The file   lib.e4   provides the needed words.
lib.e4   MUST be loaded once prior to loading any demo.

To run a demo
-------------
While in the terminal application:

1) Press the function key "F9" to open a file select dialog.
   Select:   lib.e4
   As a file loads you will see dots as a progress indicator.
   When the load is complete, an "ok" prompt will be displayed.

2) After the   lib.e4   is loaded:
   Press F9 again and select a demo such as   hanoi.e4
   As before you will see dots to show progress,
   and "ok" when done.
   After the demo is loaded, type "DEMO" to run.

\ ==============================================================

eForth
------
eForth was developed as a tool to port Forth to a new processor,
some processors with very limited resources. The newest
processor is the IntellaSys s40.

This version of eForth is 18-bit cell addressed, with a one
character occupying an 18-bit cell. Although eForth can address
the full 18-bit range of external memory, 256k 18-bit words,
eForth's program space is limited to 17-bits, 128k words. With
approximately 5k words used by the eForth kernel, the remaining
123k words may be used for data. Through the use of the words
PAGE@ and PAGE! eForth can access the entire 32MW (16MW in early
boards) sdram space in 128K pages.

eForth occupies two IntellaSys s40 nodes:
  node 11 -- interpretative engine
  node 12 -- ALU interface and stack controllers

eForth requires a serial terminal to run;
one additional node is used:
  node 10 -- asynchronous serial interface controller

eForth requires external memory to run;
five additional nodes are used to
control the external ddr sdram:
  node  3 -- data read and write
  node  4 -- control lines and clock
  node  5 -- address and command
  node 13 -- translate user page and address to bank,row,column
  node 14 -- command polling and refresh counter

eForth is a SUBSET of the ANS Programming Language -- Forth.
Words defined in eForth adhere to the usage described by ANS
Programming Language -- Forth. The following is a listing of the
eForth kernel using "WORDS". The first line shows the current
WORDLIST   being displayed as its word list identifier or "wid".

  ok words
  wid={2600}
  COLD SEE SSEE ?ID NAMED? WORDS WIDWORDS VOCS ORDER .ORDER .WID
  .ID .{#} DUMP _DUMP NDUMP ADUMP .ADR >CHAR .S ?CSP !CSP AT-XY
  PAGE REPEAT AGAIN UNTIL WHILE ELSE AHEAD IF MARK RESOLVE THEN
  BEGIN MARKER _MARKER DEFINITIONS PREVIOUS ALSO ONLY +ORDER
  -ORDER _-ORDER -ROT SET-ORDER GET-ORDER ORDER@ WORDLIST HAT
  USER CONSTANT VARIABLE CREATE DOES> _DOES> ; : :NONAME
  POSTPONE RECURSE REVEAL COMPILE-ONLY IMMEDIATE LEX! HEAD, >XT
  ?UNIQUE SET-CURRENT GET-CURRENT LAST ] _] ABORT" ." S, S"
  SLITERAL S, STRING, _S, IF\ .\ \ .( ( PARSE [DEFINED]
  [COMPILE] ['] ' [CTRL] CTRL [CHAR] CHAR LITERAL COMPILE, , C,
  AU, ALLOT ALIGN QUIT ?THROW HAND FILE PACER 'CR 'OK .OK
  EVALUATE PARSE-WORD SOURCE [ _[ ?STACK SFIND CURRENT CONTEXT
  FORTH-WORDLIST WID? NAME> _PARSE _DELIMIT SAME? ACCEPT 'PACER
  ECHO 'ECHO ? . U. D. .R U.R D.R S.R _ABORT" _." _S" _" ?CR CR
  TYPE SPACES EMITS SPACE EMIT C# 'EMIT NUF?  NUF MS MSA KEY
  KEY? IBUF 'KEY? SIGN #> #S # HOLD DIGIT <# UNUSED PAD HERE
  NUMBER? >NUMBER DIGIT? ABORT DEPTH THROW CATCH RELEASE GET
  BUILD ACTIVATE STOP SLEEP AWAKE 'S PASS WAKE _WAKE PAUSE U2 U1
  TF TID TOS STATUS FOLLOWER _USR UP UPPER >BODY >ADR -TRAILING
  FILL ERASE MOVE 2@ 2! ALIGNED /STRING BOUNDS COUNT +! CELL/
  CHAR/ / MOD /MOD FM/MOD SM/REM UM/MOD RIVE RSHIFT * UM* LSHIFT
  WITHIN MAX MIN < U< = DABS DNEGATE D+ S>D ABS - ?DUP 2DUP
  2DROP ROT NIP DECIMAL HEX \n \r #TIB BL SUP DP STATE CSP #IN
  >IN HLD DPL BASE RX? TX! !IO @IO 1- 1+ 2* 2/ + 0= INVERT
  NEGATE OR XOR AND UM+ 0< SWAP DUP OVER DROP SP! SP@ _IF _ELSE
  _LIT @ ! C@ C! AU@ AU! CELLS CELL+ CELL- CHARS CHAR+ CHAR- R>
  >R R@ RP! RP@ EXECUTE EXIT _VAR _CON NOOP

The file   lib.e4   defines many words not in the eForth kernel.
Please add only words that are needed for your application.

eForth dictionary structure
---------------------------
In this version of eForth a CELL and a CHAR are both 18-bit
values, one ASCII character per CHAR.

The dictionary is composed of inline definition headers and
execution tokens.

The word   '   (tic) returns an execution token, NOT an address.
An execution token may be changed to an address
with   >adr ( xt -- a )

A definition header is composed as follows:

Name structure: \xt\link\attribute+count\name_string\
  'xt' execution token -- cell
  'link' pointer to previous 'name string' -- cell
  'attribute+count' string length and attribute 'icn+' -- char
    n+ -- string length, 16 bits
    i  -- immediate, 1 bit
    c  -- compile-only, 1 bit
  'name string' variable length ascii 'ccc' -- chars
    compiler does not set bits in the 'name string'
  0 < link name < ... < link name < last name compiled

eForth external memory map
--------------------------

0x00000                0x20000                        0x3ffff
  |---paged data space---|---h>-pad>-----<u\r>--<s\tib>-|

0x00000 -- start of memory
0x20000 -- Start of eForth program space
HERE    -- Start of user dictionary space
PAD     -- Transient buffer, floating 80 characters above HERE
0x3feb0 -- User task area (builds down)
0x3feb0 -- Return stack (builds up)
0x3ff70 -- Data stack (builds down)
0x3ff70 -- Terminal input buffer, 128 characters
0x3ffff -- End of eForth program space
0x3ffff -- End of memory

The default combined data and return stack depth is 192 18-bit
cells.
  Return stack -- builds up toward data stack
  Data stack   -- builds down toward return stack
A stack overflow means one stack collides with the other stack.
If this happens, reload  eforth.bin  with the colorForth loader.

It is possible to edit  code.e4  to allocate more stack space by
increasing either one of the two following equates:
  D#  128 EQU #RP   \ size in cells  return stack
  D#   64 EQU #SP   \ size in cells  data stack

Utilities
---------
NUF? -- As part of utilities that may display a large amount of
        text, the word NUF? is used to throttle, pause or stop
        output:

  throttle -- press the number keys, 0=fastest to 9=slowest.
  pause    -- press any alpha key.
  stop     -- while display is paused press return.

Some words that use NUF? are WORDS, DUMP, and SEE.

DUMP displays four 18-bit numbers per line and ASCII text, even
if more than one character is packed in an 18-bit cell. DUMP may
be used to display memory in either DECIMAL or HEX.

ok hex 0 40 dump
 00000   212CF   26262   26262   26262  O bbbbbb
 00004      23       0       4      4E  #     N
 00008      4F      4F      50      1C  O O P
 0000C       6      44      5F      43    D _ C
 00010      4F      4E      1D       D  O N
 00014      44      5F      56      41  D _ V A
 00018      52      1E      14       4  R
 0001C      45      58      49      54  E X I T
 00020      2A      1B       7      45  *     E
 00024      58      45      43      55  X E C U
 00028      54      45       6      22  T E   "
 0002C       3      52      50      40    R P @
 00030      19      2C      43      52    , C R
 00034      50      21       9      32  P !   2
 00038       2      52      40      16    R @
 0003C      38      42      3E      52  8 B > R
 00040       C      3D      42      52    = B R
ok

SEE -- a very simplistic decompiler.
    SEE will run continuously. Use the NUF? keys to pause or
    stop.  Usage example: see hex <return>

Multitasker
-----------
The eForth multitasker is a cooperative round robin style, not
preemptive.

UP       ( -- a )         current task pointer
PAUSE    ( -- )           allow another task to execute
WAKE     ( -- xt )        execution token of a running task
PASS     ( -- xt )        execution token of a sleeping task
'S       ( tid a -- a )   index another task's variables
AWAKE    ( tid -- )       awake another task, set STATUS to WAKE
SLEEP    ( tid -- )       sleep another task, set STATUS to PASS
STOP     ( -- )           sleep current task, set STATUS to PASS
ACTIVATE ( tid -- )       initialize task
BUILD    ( tid -- )       link new task, do only once
GET      ( semaphore -- ) lock access to a resource
RELEASE  ( semaphore -- ) unlock access to a resource
USER     ( n "name" -- )  name user variables, n is the index
HAT      ( u s r "name" -- ( -- tid )
           define task area in ADDRESS UNITS
           u -- extra space for user variables
                (in addition to U1 and U2)
           s -- date stack depth
           r -- return stack depth

These are the default task variables:

\ u2\u1\tf\tid\tos\status\follower\r>--<s\tib>-|
D# 0         DUP USER FOLLOWER \ address of next task's STATUS
D# 1 CELLS - DUP USER STATUS   \ PASS or WAKE
D# 1 CELLS - DUP USER TOS      \ top of stack
D# 1 CELLS - DUP USER TID      \ back link tid
D# 1 CELLS - DUP USER TF       \ throw frame
D# 1 CELLS - DUP USER U1       \ free
D# 1 CELLS - DUP USER U2       \ free

Basic Task Template
-------------------
0  16 CELLS  DUP HAT TASK-1   \ create a task and allocate space
                              \ no extra variables
                              \ 16 cells for each stack
TASK-1 BUILD                  \ link task

: TASK-1-GO ( -- )            \ task that increments a variable
  0  TASK-1 U1 'S  !          \ init task user variable
  TASK-1 ACTIVATE             \ start task
  BEGIN                       \ task loop
    PAUSE                     \ pause to allow other tasks
    1 U1 +!                   \ increment variable U1
  AGAIN ;

: .U1 ( -- )
  TASK-1 U1 'S @ . ;          \ display the current value

NOTE: When initializing the user variable U1, the main eForth
task is the current task. The phrase   TASK-1 U1 'S   returns
the address of an instance of the user variable U1 belonging to
TASK-1.

To stop the task    -- TASK-1 SLEEP
To restart the task -- TASK-1 AWAKE

\ ==============================================================
\ Appendix

4. Standard Forth Documentation Requirements
--------------------------------------------

4.1 System documentation
------------------------

4.1.1 Implementation-defined options
------------------------------------
- aligned address requirements (3.1.3.3 Addresses);
  No requirement

- behavior of 6.1.1320 EMIT for non-graphic characters;
  All characters transmitted

- character editing of 6.1.0695 ACCEPT and 6.2.1390 EXPECT;
  ACCEPT: BS deletes; CR terminates
  EXPECT: not supported

- character set
  (3.1.2 Character types, 6.1.1320 EMIT, 6.1.1750 KEY);
  7-bit ASCII

- character-aligned address requirements (3.1.3.3 Addresses);
  No requirement

- character-set-extensions matching characteristics
  (3.4.2 Finding definition names);
  Case-insensitive

- conditions under which control characters
  match a space delimiter (3.4.1.1 Delimiters);
  All characters are distinct, only BL ( H# 020 ) matches space

- format of the control-flow stack (3.2.3.2 Control-flow stack);
  Addresses on data stack

- conversion of digits larger than thirty-five
  (3.2.1.2 Digit conversion);
  Case-sensitive, any character accepted

- display after input terminates in
  6.1.0695 ACCEPT and 6.2.1390 EXPECT;
  ACCEPT: text is displayed upon receipt of a character.
          When loading a file display of characters is
          suppressed.
  EXPECT: not supported

- exception abort sequence (as in 6.1.0680 ABORT");
  Implemented as -2 THROW, display the string

- input line terminator (3.2.4.1 User input device);
  Return key

- maximum size of a counted string, in characters
  (3.1.3.4 Counted strings, 6.1.2450 WORD);
  The string count is 18-bits,
  but the input buffer is limited to 128 characters

- maximum size of a parsed string (3.4.1 Parsing);
  128, limited by the input buffer

- maximum size of a definition name, in characters
  (3.3.1.2 Definition names);
  The count is 18-bits,
  but the input buffer is limited to 128 characters

- maximum string length for
  6.1.1345 ENVIRONMENT?, in characters;
  ENVIRONMENT? not supported

- method of selecting 3.2.4.1 User input device;
  Vectored thru 'KEY?

- method of selecting 3.2.4.2 User output device;
  Vectored thru 'EMIT

- methods of dictionary compilation (3.3 The Forth dictionary);
  Bit Threaded Code (btc) commonly called bitsy

- number of bits in one address unit (3.1.3.3 Addresses);
  18

- number representation and arithmetic
  (3.2.1.1 Internal number representation);
  2's complement

- ranges for n, +n, u, d, +d, and ud
  (3.1.3 Single-cell types, 3.1.4 Cell-pair types);
  Single: 18 bits
  Double: 36 bits

- read-only data-space regions (3.3.3 Data space);
  None

- size of buffer at 6.1.2450 WORD
  (3.3.3.6 Other transient regions);
  Input buffer is 128+2 characters. WORD not implemented

- size of one cell in address units (3.1.3 Single-cell types);
  1

- size of one character in address units
  (3.1.2 Character types);
  1

- size of the keyboard terminal input buffer
  (3.3.3.5 Input buffers);
  128+2

- size of the pictured numeric output string buffer
  (3.3.3.6 Other transient regions);
  80

- size of the scratch area whose address is returned by
  6.2.2000 PAD (3.3.3.6 Other transient regions);
  Size is variable, UNUSED - 80

- system case-sensitivity characteristics
  (3.4.2 Finding definition names);
  Case-insensitive

- system prompt (3.4 The Forth text interpreter, 6.1.2050 QUIT);
  "ok" CR LF

- type of division rounding
  (3.2.2.1 Integer division, 6.1.0100 */, 6.1.0110 */MOD,
    6.1.0230 /, 6.1.0240 /MOD, 6.1.1890 MOD);
  Floored, FM/MOD is default. SM/REM is provided

- values of 6.1.2250 STATE when true;
  -1 ( H# 03ffff )

- values returned after arithmetic overflow
  (3.2.2.2 Other integer operations);
  -1 ( H# 03ffff )

- whether the current definition can be found after
  6.1.1250 DOES> (6.1.0450 :);
  No

4.1.2 Ambiguous conditions
--------------------------
- a name is neither a valid definition name
  nor a valid number during text interpretation
  (3.4 The Forth text interpreter);
  -13 THROW

- a definition name exceeded the maximum length allowed
  (3.3.1.2 Definition names);
  All input limited to 128+2 by input buffer
  All string counts are 18-bit

- addressing a region not listed in 3.3.3 Data Space;
  No possible

- argument type incompatible with specified input parameter,
  e.g., passing a flag to a word expecting an n
  (3.1 Data types);
  Ignore and continue

- attempting to obtain the execution token,
  (e.g., with 6.1.0070 ', 6.1.1550 FIND, etc.)
  of a definition with undefined interpretation semantics;
  Return token

- dividing by zero
  (6.1.0100 */, 6.1.0110 */MOD, 6.1.0230 /, 6.1.0240 /MOD,
   6.1.1561 FM/MOD, 6.1.1890 MOD, 6.1.2214 SM/REM,
   6.1.2370 UM/MOD, 8.6.1.1820 M*/);
  Return maximum 18-bit value ( H# 3ffff H# 3ffff )

- insufficient data-stack space or
  return-stack space (stack overflow);
  Return stack builds up toward Data stack which builds down
  Stack collision, unknown result

- insufficient space for loop-control parameters;
  Ignore and continue

- insufficient space in the dictionary;
  No test made.
  Most likely crash system by overwriting return stack

- interpreting a word with undefined interpretation semantics;
  COMPILE-ONLY will ABORT" compile?"

- modifying the contents of the input buffer or a string literal
  (3.3.3.4 Text-literal regions, 3.3.3.5 Input buffers);
  Ignore and continue

- overflow of a pictured numeric output string;
  Overrun dictionary, most likely destroying last defined word

- parsed string overflow;
  No test made

- producing a result out of range, e.g.,
  multiplication (using *) results in a value
  too big to be represented by a single-cell integer
  (6.1.0090 *, 6.1.0100 */, 6.1.0110 */MOD, 6.1.0570 >NUMBER,
   6.1.1561 FM/MOD, 6.1.2214 SM/REM, 6.1.2370 UM/MOD,
   6.2.0970 CONVERT, 8.6.1.1820 M*/);
  2's complement "wrapping"

- reading from an empty data stack or
  return stack (stack underflow);
  Error message  "depth?"

- unexpected end of input buffer,
  resulting in an attempt to use a zero-length string as a name;
  Ignore and continue

- >IN greater than size of input buffer (3.4.1 Parsing);
  2's complement "wrapping",
  most likely  -13 THROW  interpretation error

- 6.1.2120 RECURSE appears after 6.1.1250 DOES>;
  Yes

- argument input source different than
  current input source for 6.2.2148 RESTORE-INPUT;
  RESTORE-INPUT not supported

- data space containing definitions is de-allocated
  (3.3.3.2 Contiguous regions);
  No test made; definitions may be over-written

- data space read/write with incorrect alignment
  (3.3.3.1 Address alignment);
  Allowed

- data-space pointer not properly aligned
  (6.1.0150 ,, 6.1.0860 C,);
  Allowed

- less than u+2 stack items (6.2.2030 PICK, 6.2.2150 ROLL);
  Ignore and continue

- loop-control parameters not available
  (6.1.0140 +LOOP, 6.1.1680 I, 6.1.1730 J,
   6.1.1760 LEAVE, 6.1.1800 LOOP, 6.1.2380 UNLOOP);
  Ignore and continue

- most recent definition does not have a name
  (6.1.1710 IMMEDIATE);
  IMMEDIATE bit set on previous definition

- name not defined by 6.2.2405 VALUE used by 6.2.2295 TO;
  Allowed

- name not found
  (6.1.0070 ', 6.1.2033 POSTPONE,
   6.1.2510 ['], 6.2.2530 [COMPILE]);
  -13 THROW

- parameters are not of the same type
  (6.1.1240 DO, 6.2.0620 ?DO, 6.2.2440 WITHIN);
  Allowed

- 6.1.2033 POSTPONE or
  6.2.2530 [COMPILE] applied to 6.2.2295 TO;
  TO not supported

- string longer than a counted string returned by 6.1.2450 WORD;
  WORD not supported. Use PARSE-WORD ( "ccc" -- ca u )

- u greater than or equal to the number of bits in a cell
  ( 6.1.1805 LSHIFT, 6.1.2162 RSHIFT);
  Allowed

- word not defined via 6.1.1000 CREATE
  (6.1.0550 >BODY, 6.1.1250 DOES>);
  Allowed

- words improperly used outside 6.1.0490 <# and 6.1.0040 #>
  (6.1.0030 #, 6.1.0050 #S, 6.1.1670 HOLD, 6.1.2210 SIGN);
  Allowed

\ ==============================================================

