CR .( ++++ Altera .hex file format generalized ++++ )
\ ==============================================================
\ todo
\ ==============================================================
\ 20070829 jwr added SAO and input parameter
\ 20070826 jwr moved parameterized SAVEHEX here
\ 20070826 bee change HEX[] win32forth compatible
\ 20070824 bee generalize for data size
\ 20070729 bee 16bit version
\ 20070720 bee move SAVEFILE to savefile.f
\ 20070326 bee make generic
\ 20070311 bee using PAD is ok with eForth, gforth, swiftforth
\ 20070311 bee rewrite and add vectored data address conversion
\ 20060425 jr  changed file name to match quartus id rev7.7
\ 20060227 jr  noted that rom/ram memory image is used rev7.6
\ 20060227 jr  changed template rev7.6
\ 20051125 jr  change input to romram.bin, directories
\ 20050719 jr  added xor to output numbers & some style edits
\ 20050705 jr  file names changed for Altera
\ 20050605 jf  delivered by Jeff Fox 6/4/05 bugfix
\ ==============================================================
\
\ Altera 16-bit .hex format
\   no spaces in the output, spaces here are for readability
\
\ :02 addr  00  ** **  cs <crlf> \ cs=100h - (byte.sum mod 256)
\ ...                            \ data records: 17 bytes each
\ :02 00a9  00  00 00  55 <crlf> \ 2+a9=ab,          100-ab=55
\ :02 00aa  00  41 b5  5e <crlf> \ 2+aa+41+b5=(1)a2, 100-a2=5e
\ ...
\ :00 0000  01         ff <crlf> \ end record: 13 bytes
\
\ ==============================================================

DECIMAL

\ win32forth   pad is 256 bytes ! \ not here relative
\ swiftforth : pad here 512 + ;
\ gforth     : pad here word-pno-size + aligned ; \ here+104

: HEX[] ( -- ca ) HERE 1024 + ; \ temporary buffer

CREATE "CR ( -- ca ) \ new line string
  13 ( cr ) C,  10 ( lf ) C,

VARIABLE CSUM   \ accumulate checksum
VARIABLE NBYTES \ data size
VARIABLE 'TA@   \ execution vector
VARIABLE SAO    \ Starting Address Offset (added to I)

    3 NBYTES !  \ default 18-bit words (24)
' ABORT 'TA@ !  \ default vector, set in metacpu.e4

: ## ( d -- d ) \ accumulate checksum and data
  OVER CSUM +!  # # ;

: HEX-FORMAT ( s d u -- ca u )
  >R  OVER - SAO !  R>
  BASE @ >R  HEX   0 CSUM !    \ save radix, output hex, csum=0
  HEX[] 0  2SWAP BOUNDS        \ source address and count
  ?DO ( n a u )  S" :" S+      \ header=:, 1 byte
    NBYTES @ 0 <# ## #> S+     \ count, 2 bytes
    I SAO @ + 0 <# ## ## #> S+ \ address=xxxx, 4 bytes
    0 0 <# ## #> S+            \ type of record=00, 2 bytes
    I   'TA@ @ EXECUTE         \ Host Target Address Conversion
      0 <#                     \ NBYTES defined in CPU.E4
        NBYTES @ 0 DO ## LOOP  \ data=xxxxxx, 2 bytes per NBYTES
      #> S+
    CSUM @ NEGATE              \ calculate checksum
      0 <# ## #> S+            \ csum=xx, 2 bytes, reset csum=0
    "CR 2 S+                   \ new line, 2 bytes
  LOOP                         \ total/record 13+n*2
  S" :00000001FF" S+           \ last record, 11 bytes
  "CR 2 S+                     \ new line, 2 bytes ( total 13 )
  R> BASE ! ;                  \ restore radix

: SAVEHEX ( src dst size ca u -- ) \ save .hex format file
  2>R                   \ save filename string
  HEX-FORMAT            \ convert to .hex format
  2R>  SAVEFILE ;       \ write file

