543000010000 221660145424 000651640000 000202002000 000200010000 000100000002 000000000002 000000000002
000000000002 000000000002 621011001462 040024546432 022000000001 757464222620 242126220000 200105026400
000100010001 000200000001 000300000001 000000000001 000000000001 000100010001 515502010001 400101500031
200422001200 440040400400 100004001000 040110001376 116413640166 016411040020 004011640000 000010010001
000110030000 000010000000 000010000002 000010000000 000010000230 010111000151 132402040005 000100010175
017101300531 073000000121 003001700400 040000010000 000100000600 040100010000 000100000400 040100010001
000100000444 042000210120 006403442004 220012004400 404004001000 020000000000 000000000000 000000000000
000000000000 000000000000 000000000000 000000000000 000000000000 000000000000 000000000000 000000000000
000000000000 000000000000 000000000000 000000000000 000000000000 000000000000 000000000000 000000000000
A3 PK SAD1          0312         SY                                             
       REM BCD ADDITION AND SUBTRACTION                               PKSAD10001
 SAD1  CHS                                                              SAD10002
       ADD C              READY FOR SUBTRACTION                         SAD10003
       STQ TEMP                                                         SAD10004
       ADD TEMP                                                         SAD10005
       TMI P1             IF A+B BOTH MINUS                             SAD10006
       PBT                                                              SAD10007
       TRA P1+1           IF A HAS DIFFERENT SIGN FROM B                SAD10008
       ADD C+2            IF A+B BOTH PLUS                              SAD10009
       TRA P2+1                                                         SAD10010
 P1    SUB C+2                                                          SAD10011
       SUB C                                                            SAD10012
       TMI P2+1           ABS. VALUE OF +A LESS THAN ABS. VALUE OF -B   SAD10013
       TNZ P2             SEE SMALL D UNDER USE IN WRITE UP             SAD10014
       LLS 0              SIGN NEEDED PUT IN AC                         SAD10015
       TMI OK             -0 PLUS +0                                    SAD10016
 P2    ADD C              ABS. VALUE OF+A GREATER OR EQUAL TO -B        SAD10017
       STO TEMP           SUM OR REMAINDER STORED                       SAD10018
       LRS 0              SAVE CORRECT SIGN IN MQ                       SAD10019
       ANA C+1            CARRY                                         SAD10020
       STO TEMP+1            OVER                                       SAD10021
       ARS 3                    NINES                                   SAD10022
       SBM TEMP                    CORRECTION                           SAD10023
       ADD TEMP+1                                                       SAD10024
       LLS 0              RESET CORRECT SIGN                            SAD10025
 OK    LRS 34                                                           SAD10026
       ALS 30                                                           SAD10027
       LLS 4              SIGN+OVERFLOW IN AC O.F. IN LAST BIT          SAD10028
       STO TEMP                                                         SAD10029
       ARS 30                                                           SAD10030
       LRS 5              SIGN+SUM WITHOUT OVERFLOW IN MQ               SAD10031
       CLA TEMP           SIGN+OVERFLOW ON AC                           SAD10032
       TRA 1,4                                                          SAD10033
 C     OCT +200000000000                                                SAD10034
       OCT +006060606060                                                SAD10035
       OCT -171111111112                                                SAD10036
 TEMP  BSS 2                                                            SAD10037
