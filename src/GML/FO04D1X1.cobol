      *   SIST ENDRET 20/09-85 13.29.45 AV   ANNE                       00000000
      *   SIST ENDRET 13/06-84 08.23.12 AV   BJARNE KJOS                00000010
      *IDENTIFIKASION                                                   00000020
      *    FO04D101 - HOVEDPROGRAM I COBOL                              00000030
      *HENSIKT                                                          00000040
      *    SELEKTERER RECORDS FRA PENSJONSREGISTERET                    00000050
      *    A: ALLE PERSONER SOM IKKE HAR STANDARD YTELSER (LISTER - TK) 00000060
001050 IDENTIFICATION DIVISION.                                         00000070
001100 PROGRAM-ID. FO04D101.                                            00000080
001150 AUTHOR. T. VRAALSEN.                                             00000090
001300 ENVIRONMENT DIVISION.                                            00000100
001350 CONFIGURATION SECTION.                                           00000110
       SOURCE-COMPUTER.                                                 00000120
       OBJECT-COMPUTER.                                                 00000130
001500 INPUT-OUTPUT SECTION.                                            00000140
001550 FILE-CONTROL.                                                    00000150
001600     SELECT KORTFILE ASSIGN TO UT-S-SYS004.                       00000160
001650     SELECT OUTFILE  ASSIGN TO UT-S-SYS005.                       00000170
001700     SELECT INNFILE  ASSIGN TO UT-S-SYS006.                       00000180
001800 DATA DIVISION.                                                   00000190
001850 FILE SECTION.                                                    00000200
001900 FD  OUTFILE                                                      00000210
001950     RECORDING MODE IS F                                          00000220
002000     BLOCK CONTAINS 0 RECORDS                                     00000230
002050     RECORD CONTAINS 125 CHARACTERS                               00000240
002100     LABEL RECORDS ARE STANDARD                                   00000250
002150     DATA RECORD IS TAPE-REC.                                     00000260
002200 01  TAPE-REC SYNC.                                               00000270
002250     02 T-DEL1.                                                   00000280
002300        03 T-FNR         PIC S9(11)  COMP-3.                      00000290
002350        03 T-NAVN        PIC X(25).                               00000300
002400        03 T-PSTAT1      PIC X.                                   00000310
              03 T-PSTAT2      PIC X.                                   00000320
              03 T-PSTAT3      PIC X.                                   00000320
002450        03 T-GRP         PIC S9(5)   COMP-3.                      00000330
002500        03 T-TPAVD       PIC S9(5)   COMP-3.                      00000340
002550        03 T-TPEGEN      PIC S9(5)   COMP-3.                      00000350
002600        03 T-EFORTIL     PIC S9(5)   COMP-3.                      00000360
002650        03 T-BFORTIL     PIC S9(5)   COMP-3.                      00000370
002700        03 T-FORINNT     PIC S9(5)   COMP-3.                      00000380
002950        03 T-SUMYD       PIC S9(5)   COMP-3.                      00000390
003000        03 T-TKNR        PIC S9(5)   COMP-3.                      00000400
003100        03 T-UFGRAD      PIC S9(3)   COMP-3.                      00000410
003150        03 T-TIL851      PIC S9(3)   COMP-3.                      00000420
003200        03 T-SAERTIL     PIC S9(5)   COMP-3.                      00000430
003250        03 T-KOMPTIL     PIC S9(3)   COMP-3.                      00000440
003300        03 T-VGRP        PIC S9(5)   COMP-3.                      00000450
003350        03 T-VTILP       PIC S9(5)   COMP-3.                      00000460
              03 T-NETTO-GP    PIC S9(5)   COMP-3.                      00000480
              03 T-NETTO-TP    PIC S9(5)   COMP-3.                      00000490
              03 T-EK-GR       PIC S9(5)   COMP-3.                      00000490
              03 T-BA-GR       PIC S9(5)   COMP-3.                      00000490
              03 T-SB-TILL     PIC S9(5)   COMP-3.                      00000490
              03 T-SB-GR       PIC S9(5)   COMP-3.                      00000490
              03 T-GT-TIL-L92  PIC S9(5)   COMP-3.                      00000490
              03 T-GT-TP       PIC S9(5)   COMP-3.                      00000490
              03 T-AFP         PIC S9(5)   COMP-3.                      00000490
003450     02 SORTNAVN     PICTURE X(25).                               00000500
003500 FD  KORTFILE                                                     00000510
003550     RECORDING MODE IS F                                          00000520
003600     LABEL RECORD ARE STANDARD                                    00000530
003650     BLOCK CONTAINS 0 RECORDS                                     00000540
003700     DATA RECORD IS KORT.                                         00000550
003750 01  KORT SYNC.                                                   00000560
003800     02  KKART       PICTURE X.                                   00000570
003850     02  KTRK        PICTURE 9(4).                                00000580
003900     02  KSORTB      PICTURE X.                                   00000590
003950     02 FILLER       PIC X(12).                                   00000600
004000 FD  INNFILE, RECORDING MODE IS F                                 00000610
004050     BLOCK CONTAINS 0 RECORDS                                     00000620
004100     RECORD CONTAINS 109 CHARACTERS, LABEL RECORD STANDARD,       00000630
004150     DATA RECORD IS INN-REC.                                      00000640
004200 01  INN-REC SYNC.                                                00000650
004250     02 FILLER           PIC X(9).                                00000660
           02 I-DEL1.                                                   00000670
              03 I-FNR         PIC S9(11)   COMP-3.                     00000680
              03 I-NAVN        PIC X(25).                               00000690
              03 I-PENSJT1     PIC X.                                   00000700
              03 I-PENSJT2     PIC X.                                   00000710
              03 I-PENSJT3     PIC X.                                   00000710
              03 I-GP          PIC S9(5)    COMP-3.                     00000720
              03 I-TPAVD       PIC S9(5)    COMP-3.                     00000730
              03 I-TPEGEN      PIC S9(5)    COMP-3.                     00000740
              03 I-ET          PIC S9(5)    COMP-3.                     00000750
              03 I-BT          PIC S9(5)    COMP-3.                     00000760
              03 I-FORVI       PIC S9(5)    COMP-3.                     00000770
              03 I-SUM-Y       PIC S9(5)    COMP-3.                     00000780
              03 I-TKNR        PIC S9(5)    COMP-3.                     00000790
              03 I-GRAD        PIC S9(3)    COMP-3.                     00000800
              03 I-T851        PIC S9(3)    COMP-3.                     00000810
              03 I-ST          PIC S9(5)    COMP-3.                     00000820
              03 I-KT          PIC S9(3)    COMP-3.                     00000830
              03 I-VT-GP       PIC S9(5)    COMP-3.                     00000840
              03 I-VT-TP       PIC S9(5)    COMP-3.                     00000850
              03 I-GP-N        PIC S9(5)    COMP-3.                     00000870
              03 I-TP-N        PIC S9(5)    COMP-3.                     00000880
              03 I-EK-GR       PIC S9(5)    COMP-3.                     00000880
              03 I-BA-GR       PIC S9(5)    COMP-3.                     00000880
              03 I-SB-TILL     PIC S9(5)    COMP-3.                     00000880
              03 I-SB-GR       PIC S9(5)    COMP-3.                     00000880
              03 I-GT-TIL-L92  PIC S9(5)    COMP-3.                     00000880
              03 I-GT-TP       PIC S9(5)    COMP-3.                     00000880
              03 I-AFP         PIC S9(5)    COMP-3.                     00000880
004550 WORKING-STORAGE SECTION.                                         00000900
004600 77  I               PICTURE S999    COMPUTATIONAL SYNC.          00000910
004750 77  I2                  PIC S999    COMP VALUE +001 SYNC.        00000920
005300 01  TABELL SYNC.                                                 00000930
005350     02 TAB OCCURS 460 TIMES.                                     00000940
005400         03  KART    PICTURE X.                                   00000950
005450         03  TRKK    PICTURE 9(4).                                00000960
005500         03  SORTB   PICTURE X.                                   00000970
005550 01  HJ-HKS SYNC.                                                 00000980
005600     02 FILLER           PIC X(31).                               00000990
005650     02 HKS-PSTAT        PIC X.                                   00001000
005700     02 FILLER           PIC X(33).                               00001010
005750     02 HKS-TKNR         PIC S9(5)   COMP-3.                      00001020
           02 FILLER           PIC X(21).                               00001030
005900 01  PARAM SYNC.                                                  00001040
005950     02 P-ID         PIC X(9).                                    00001050
006050     02 S-TILL       PIC 9(4).                                    00001060
006100     02 S-TILL-16    PIC 9(4).                                    00001070
           02 P-MP100      PIC 9(4).                                    00001080
           02 P-MP075      PIC 9(4).                                    00001090
006150     02 FILLER       PIC X(55).                                   00001100
006200 PROCEDURE DIVISION.                                              00001110
006250     ACCEPT PARAM.                                                00001120
006300     IF P-ID NOT = 'FO04D101:' GO TO B.                           00001130
006400     IF S-TILL NOT NUMERIC GO TO B.                               00001140
006450     IF S-TILL-16 NOT NUMERIC GO TO B.                            00001150
           IF P-MP100 NOT NUMERIC GO TO B.                              00001160
           IF P-MP075 NOT NUMERIC GO TO B.                              00001170
006500     GO TO NULLST.                                                00001180
006550 B.  DISPLAY PARAM, ' PARAM ER GALT'.                             00001190
006600     STOP RUN.                                                    00001200
006650 NULLST.                                                          00001210
006700     PERFORM NULL VARYING I FROM 1 BY 1                           00001220
006750     UNTIL I = 461.                                               00001230
006800     GO TO OPEN-KORT.                                             00001240
006850 NULL.                                                            00001250
006900     MOVE SPACES TO KART (I).                                     00001260
006950     MOVE ZEROES TO TRKK (I).                                     00001270
007000     MOVE SPACES TO SORTB (I).                                    00001280
007050 OPEN-KORT.                                                       00001290
007100     OPEN INPUT KORTFILE.                                         00001300
007150     MOVE ZEROES TO I.                                            00001310
007200 LES-KORT.                                                        00001320
007250     READ KORTFILE AT END GO TO OPEN-TAPE.                        00001330
007300     IF KSORTB = 'F' GO TO TABB.                                  00001340
007350     GO TO LES-KORT.                                              00001350
007400 TABB.                                                            00001360
007450     ADD +1 TO I.                                                 00001370
007500     MOVE KKART TO KART (I).                                      00001380
007550     MOVE KTRK   TO TRKK (I).                                     00001390
007600     MOVE KSORTB TO SORTB (I).                                    00001400
007650     GO TO LES-KORT.                                              00001410
007700 OPEN-TAPE.                                                       00001420
           CLOSE KORTFILE.                                              00001430
007750     ADD I TO I2.                                                 00001440
007800     OPEN OUTPUT OUTFILE.                                         00001450
           OPEN INPUT  INNFILE.                                         00001460
007850 LES-PREG.                                                        00001470
           READ INNFILE AT END GO TO EOF-INNFILE.                       00001480
           IF I-TPEGEN > ZERO GO TO SELECTED.                           00001490
           IF I-ET     > ZERO GO TO SELECTED.                           00001500
           IF I-FORVI  > ZERO GO TO SELECTED.                           00001510
           IF I-VT-GP  > ZERO GO TO SELECTED.                           00001520
           IF I-VT-TP  > ZERO GO TO SELECTED.                           00001530
           IF I-GRAD > 024 AND                                          00001540
              I-GRAD < 100 GO TO SELECTED.                              00001550
           IF I-ST NOT = S-TILL-16 AND                                  00001560
              I-ST NOT = S-TILL    GO TO SELECTED.                      00001570
           IF I-SUM-Y NOT = P-MP100 AND                                 00001580
              I-SUM-Y NOT = P-MP075 GO TO SELECTED.                     00001590
           GO TO LES-PREG.                                              00001600
010300 EOF-INNFILE.                                                     00001610
010350     CLOSE OUTFILE, INNFILE.                                      00001620
010400     STOP RUN.                                                    00001630
010450 SELECTED.                                                        00001640
           MOVE ZEROS TO T-GRP, T-TPEGEN, T-EFORTIL, T-BFORTIL, T-SUMYD,00001650
           T-FNR, T-FORINNT, T-TKNR, T-UFGRAD, T-SAERTIL, T-KOMPTIL,    00001660
           T-TPAVD, T-VGRP, T-VTILP, T-NETTO-GP, T-NETTO-TP, T-TIL851,  00001670
           T-EK-GR, T-BA-GR, T-SB-TILL, T-SB-GR,T-GT-TIL-L92,T-GT-TP,           
           T-AFP.                                                               
      *                                                                         
           MOVE SPACES TO T-NAVN, T-PSTAT1, T-PSTAT2, T-PSTAT3.         00001680
           MOVE I-DEL1 TO T-DEL1.                                       00001690
           MOVE ZERO TO I.                                              00001700
       TEST-TRK-TAB.                                                    00001710
           ADD 1 TO I.                                                  00001720
           IF I = I2                                                    00001730
014450     MOVE T-NAVN TO SORTNAVN                                      00001740
014500     GO TO SKRIV.                                                 00001750
014550     IF T-TKNR = TRKK (I)                                         00001760
014600     MOVE SPACES TO SORTNAVN                                      00001770
014650     GO TO SKRIV.                                                 00001780
014700     IF T-TKNR < TRKK (I), MOVE T-NAVN TO SORTNAVN                00001790
014750     GO TO SKRIV.                                                 00001800
014800     GO TO TEST-TRK-TAB.                                          00001810
014850 SKRIV.                                                           00001820
014900     WRITE TAPE-REC.                                              00001830
014950     GO TO LES-PREG.                                              00001840
