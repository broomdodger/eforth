CR .( ++++ eForth nuf? - throttle output ++++ )
\ ==============================================================
\ todo
\ ==============================================================
\ 20070720 bee separate
\ ==============================================================

DECIMAL

: DIGIT? ( c base -- u f )
  >R [CHAR] 0 -  9 OVER < IF 7 -  DUP 10 < OR THEN DUP R> U< ;

CREATE NUF  66 , \ for millisecond delay

: NUF? ( -- f )
  KEY? IF KEY 10 DIGIT? \ 0-9 throttle
    IF 4 LSHIFT NUF ! 0 EXIT THEN DROP KEY 13 = EXIT
  THEN 0 NUF @ MS ;

