CR .( ++++ SAVEFILE ++++ )
\ ==============================================================
\ todo
\ ==============================================================
\ 20090727 bee update
\ ==============================================================

: FERR ( -- ) ." -File Failed" ABORT ;

\ a missing directory WILL cause a CREATE-FILE error

: SAVEFILE ( buffer-ca u  filespec-ca u -- )
  CR ." Saving " 2DUP ( filespec ) TYPE SPACE
    W/O ( write only ) BIN ( binary, modify fam )
  CREATE-FILE IF ." Create" FERR THEN  DUP >R
  WRITE-FILE IF ." Write" FERR THEN  R>
  CLOSE-FILE IF ." Close" FERR THEN ;

