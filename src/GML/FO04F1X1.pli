      *   SIST ENDRET 26/06-91          AV   TUYEN                      00010000
      *   SIST ENDRET 23/09-85 09.28.09 AV   ANNE                       00020000
      *   SIST ENDRET 13/06-84 12.33.28 AV   BJARNE KJOS                00030000
001050 IDENTIFICATION DIVISION.                                         00040000
001100 PROGRAM-ID. FO04F1X1.                                            00050001
001150 AUTHOR. T. VRAALSEN.                                             00060000
001250 ENVIRONMENT DIVISION.                                            00070000
001300 CONFIGURATION SECTION.                                           00080000
001350 SOURCE-COMPUTER.                                                 00090000
001400 OBJECT-COMPUTER.                                                 00100000
001450 SPECIAL-NAMES.                                                   00110000
001500     C01 IS NS.                                                   00120000
001550 INPUT-OUTPUT SECTION.                                            00130000
001600 FILE-CONTROL.                                                    00140000
001650     SELECT INFILE    ASSIGN TO UT-S-SYS006.                      00150000
001700     SELECT UTFILE    ASSIGN TO UR-S-SYS005.                      00160000
001750     SELECT PARAMFILE ASSIGN TO UR-S-SYS004.                      00170000
001800 DATA DIVISION.                                                   00180000
001850 FILE SECTION.                                                    00190000
001900 FD  INFILE                                                       00200000
001950     RECORDING MODE IS F                                          00210000
002000     LABEL RECORD IS STANDARD                                     00220000
002050     BLOCK CONTAINS 0 RECORDS                                     00230000
002100     RECORD CONTAINS 125 CHARACTERS                               00240002
002150     DATA RECORD IS REC-1.                                        00250000
002200 01  REC-1 SYNC.                                                  00260000
002250     02 FNR              PIC S9(11) COMP-3.                       00270000
002300     02 NAVN             PIC X(25).                               00280000
002350     02 PSTAT1           PIC X.                                   00290000
           02 PSTAT2           PIC X.                                   00300000
           02 PSTAT3           PIC X.                                   00310000
           02 GRP              PIC S9(5)  COMP-3.                       00320000
           02 TPAVD            PIC S9(5)  COMP-3.                       00330000
           02 TPEGEN           PIC S9(5)  COMP-3.                       00340000
           02 EFORTIL          PIC S9(5)  COMP-3.                       00350000
           02 BFORTIL          PIC S9(5)  COMP-3.                       00360000
           02 FORINNT          PIC S9(5)  COMP-3.                       00370000
           02 SUMYD            PIC S9(5)  COMP-3.                       00380000
           02 TRKNR            PIC S9(5)  COMP-3.                       00390000
      *    02 AVD              PIC  X           .                       00400000
           02 UGRAD            PIC S999    COMP-3.                      00410000
           02 TIL851           PIC S9(3)   COMP-3.                      00420000
           02 SAERTIL          PIC S9(5)   COMP-3.                      00430000
           02 TAI              PIC S9(3)   COMP-3.                      00440000
           02 VGRP             PIC S9(5)   COMP-3.                      00450000
           02 VTLP             PIC S9(5)   COMP-3.                      00460000
           02 NETTO-GP         PIC S9(5)   COMP-3.                      00470000
           02 NETTO-TP         PIC S9(5)   COMP-3.                      00480000
           02 EK-GR            PIC S9(5)   COMP-3.                      00490000
           02 BA-GR            PIC S9(5)   COMP-3.                      00500000
           02 SB-TILL          PIC S9(5)   COMP-3.                      00510000
           02 SB-GR            PIC S9(5)   COMP-3.                      00520000
           02 GT-L92           PIC S9(5)   COMP-3.                      00530000
           02 GT-TP            PIC S9(5)   COMP-3.                      00540000
           02 AFP              PIC S9(5)   COMP-3.                      00550000
           02 SNAVN            PIC X(25).                               00560000
       FD  UTFILE                                                       00570000
           RECORDING MODE IS F                                          00580000
           LABEL RECORD IS OMITTED                                      00590000
003600     RECORD CONTAINS 132 CHARACTERS                               00600000
003650     BLOCK CONTAINS 0 RECORDS                                     00610000
003700     DATA RECORD IS REC-2.                                        00620000
003750 01  REC-2 SYNC.                                                  00630000
003800     02 FILLER           PIC X(132).                              00640000
      *                                                                 00650000
003850 FD  PARAMFILE                                                    00660000
003900     RECORDING MODE IS F                                          00670000
003950     LABEL RECORD IS OMITTED                                      00680000
004000     RECORD CONTAINS 80 CHARACTERS                                00690000
004050     BLOCK CONTAINS 0 RECORDS                                     00700000
004100     DATA RECORD IS PARAM.                                        00710000
004150 01  PARAM SYNC.                                                  00720000
004200     02 K-ART        PIC X.                                       00730000
004250     02 TKNR         PIC 9(4).                                    00740000
004300     02 FILLER       PIC X(75).                                   00750000
004350 WORKING-STORAGE SECTION.                                         00760000
       77  TRK-AKK      PIC 9(4) VALUE ZEROS.                           00770000
       77  SIDE-AKK     PIC 9(4) VALUE ZEROS.                           00780000
004500 77  LINJE-AKK    PIC 99   VALUE ZEROS.                           00790000
004550 77  TKNR-TEST    PIC 9(4).                                       00800000
004600 77  SVAR         PIC X.                                          00810000
      * LEGG INN DE 2 NESTE LINJENE .          TUYEN 8.7.91             00820000
004550 77  TRK-NR       PIC 9(4).                                       00830000
004600 77  TRK-NAVN     PIC X(23).                                      00840000
004650 77  SW-ALLE      PIC X       VALUE SPACE.                        00850000
       77  FORINNT-AKK  PIC S9(7)   VALUE ZERO.                         00860000
       77  X-FELT       PIC 9(4)   VALUE ZERO.                          00870000
       77  X-ALDER      PIC 9(4)   VALUE ZERO.                          00880000
       01  W-FNR        PIC 9(11) VALUE ZEROS.                          00890000
       01  X-FNR REDEFINES W-FNR.                                       00900000
          02  W-DAG     PIC 9(2).                                       00910000
          02  W-MN      PIC 9(2).                                       00920000
          02  W-AR      PIC 9(2).                                       00930000
          02  W-REST    PIC 9(5).                                       00940000
                                                                        00950000
004700 01  X-NAVN.                                                      00960000
004700    02   X-NAVN-5B                  PIC X(5).                     00970000
004700    02   X-FIL                      PIC X(1).                     00980000
004700    02   X-TAI                      PIC X(3).                     00990000
004700 01      W-AVD                      PIC X(1).                     01000000
004700 01  ARBEIDSOMRAADE SYNC.                                         01010000
004750     02 HEAD-1.                                                   01020000
004800        03 FILLER    PIC X(114)  VALUE SPACES.                    01030005
004850        03 FILLER    PIC X(9)    VALUE 'SIDE NR. '.               01040000
004900        03 SIDENR    PIC ZZZ9.                                    01050000
004800        03 FILLER    PIC X(005)  VALUE SPACES.                    01060005
004950     02 HEAD-2.                                                   01070000
005000        03 FILLER    PIC X(13)   VALUE 'TRYGDEKONTOR '.           01080000
005050        03 TRKNR-UT  PIC 9(4).                                    01090000
005100        03 FILLER    PIC XX      VALUE SPACES.                    01100000
005150        03 TKNAVN    PIC X(23).                                   01110000
005200        03 FIL-1     PIC X(10)   VALUE SPACES.                    01120000
005200        03 FIL-2     PIC X(1)   VALUE SPACES.                     01130000
005200        03 FILLER    PIC X(79)   VALUE SPACES.                    01140000
005250     02 HEAD-2A.                                                  01150000
005300        03 FILLER    PIC X(72)   VALUE  'STØNADSMOTTAKERE ETTER KA01160000
      -    'P. 7, 8, 10, ELLER KAP. 11 MED TILLEGGSPENSJON '.           01170000
              03 FILLER    PIC X(60)   VALUE '(TILLEGGSYTELSE) ELLER MED01180000
      -    ' REDUSERT YTELSE                  '.                        01190000
005500     02 HEAD-2C.                                                  01200000
005550        03 FILLER    PIC X(13)   VALUE 'BEREGNET DEN '.           01210000
005600        03 H2C-DATO1 PIC X(13).                                   01220000
              03 FILLER    PIC X(20)   VALUE ' MED GRUNNBELØP KR. '.    01230000
              03 BELOP     PIC ZZZZ9.                                   01240000
              03 FILLER    PIC X(33)   VALUE '. JF   RIKSTRYGDEVERKETS B01250000
      -    'REV AV '.                                                   01260000
005900        03 H2C-DATO2 PIC X(12).                                   01270000
              03 FILLER    PIC X       VALUE '.'.                       01280000
              03 FILLER    PIC X(34)   VALUE SPACES.                    01290000
                                                                        01300000
           02 HEAD-3.                                                   01310000
              03 FILLER    PIC X(11) VALUE '           '.               01320000
007400        03 FILLER    PIC X(12) VALUE '           '.               01330000
              03 FILLER    PIC X(04) VALUE 'PEN '.                      01340000
              03 FILLER    PIC X(05) VALUE 'UF   '.                     01350000
              03 FILLER    PIC X(06) VALUE '     '.                     01360000
              03 FILLER    PIC X(06) VALUE '     '.                     01370000
              03 FILLER    PIC X(06) VALUE '     '.                     01380000
              03 FILLER    PIC X(06) VALUE 'EKTE.'.                     01390000
              03 FILLER    PIC X(06) VALUE '     '.                     01400000
              03 FILLER    PIC X(06) VALUE '     '.                     01410000
              03 FILLER    PIC X(06) VALUE '     '.                     01420000
              03 FILLER    PIC X(06) VALUE 'GT-  '.                     01430000
              03 FILLER    PIC X(07) VALUE 'FORV.'.                     01440000
              03 FILLER    PIC X(06) VALUE '     '.                     01450000
              03 FILLER    PIC X(06) VALUE '     '.                     01460000
              03 FILLER    PIC X(06) VALUE '     '.                     01470000
              03 FILLER    PIC X(06) VALUE '     '.                     01480000
              03 FILLER    PIC X(05) VALUE '     '.                     01490000
              03 FILLER    PIC X(05) VALUE '    '.                      01500000
              03 FILLER    PIC X(05) VALUE '    '.                      01510000
              03 FILLER    PIC X(05) VALUE '    '.                      01520000
              03 FILLER    PIC X(06) VALUE '     '.                     01530004
                                                                        01540000
           02 HEAD-4.                                                   01550000
              03 FILLER    PIC X(11) VALUE '           '.               01560000
007400        03 FILLER    PIC X(12) VALUE '           '.               01570000
              03 FILLER    PIC X(04) VALUE 'SJON'.                      01580000
              03 FILLER    PIC X(05) VALUE 'GRAD '.                     01590000
              03 FILLER    PIC X(06) VALUE 'GRUNN'.                     01600000
              03 FILLER    PIC X(06) VALUE '     '.                     01610000
              03 FILLER    PIC X(06) VALUE '     '.                     01620000
              03 FILLER    PIC X(06) VALUE 'FELLE'.                     01630000
              03 FILLER    PIC X(06) VALUE 'BARN '.                     01640000
              03 FILLER    PIC X(06) VALUE 'SÆRB '.                     01650000
      *       03 FILLER    PIC X(06) VALUE 'TAI  '.                     01660003
              03 FILLER    PIC X(06) VALUE 'AFP- '.                     01670000
              03 FILLER    PIC X(06) VALUE 'TILL.'.                     01680000
              03 FILLER    PIC X(07) VALUE 'ERV. '.                     01690000
              03 FILLER    PIC X(06) VALUE '     '.                     01700000
              03 FILLER    PIC X(06) VALUE '     '.                     01710000
              03 FILLER    PIC X(06) VALUE '     '.                     01720000
              03 FILLER    PIC X(06) VALUE '     '.                     01730000
              03 FILLER    PIC X(05) VALUE 'SÆR- '.                     01740000
              03 FILLER    PIC X(05) VALUE 'TP- '.                      01750000
              03 FILLER    PIC X(05) VALUE 'SUM '.                      01760000
              03 FILLER    PIC X(06) VALUE '       '.                   01770003
                                                                        01780000
           02 HEAD-5.                                                   01790000
              03 FILLER    PIC X(11) VALUE '           '.               01800000
007400        03 FILLER    PIC X(12) VALUE '           '.               01810000
              03 FILLER    PIC X(04) VALUE 'TYP '.                      01820000
              03 FILLER    PIC X(05) VALUE 'ELL  '.                     01830000
              03 FILLER    PIC X(06) VALUE 'PEN- '.                     01840000
              03 FILLER    PIC X(06) VALUE ' TP. '.                     01850000
              03 FILLER    PIC X(06) VALUE ' TP. '.                     01860000
              03 FILLER    PIC X(06) VALUE 'TIL- '.                     01870000
              03 FILLER    PIC X(06) VALUE 'TIL- '.                     01880000
              03 FILLER    PIC X(06) VALUE 'TIL- '.                     01890000
      *       03 FILLER    PIC X(06) VALUE 'HELE '.                     01900003
              03 FILLER    PIC X(06) VALUE 'TIL- '.                     01910000
              03 FILLER    PIC X(06) VALUE 'LOV- '.                     01920000
              03 FILLER    PIC X(07) VALUE 'INN- '.                     01930000
              03 FILLER    PIC X(06) VALUE ' --NET'.                    01940000
              03 FILLER    PIC X(06) VALUE 'TO-- '.                     01950000
              03 FILLER    PIC X(06) VALUE 'VENTET'.                    01960000
              03 FILLER    PIC X(06) VALUE 'ILL.  '.                    01970000
              03 FILLER    PIC X(05) VALUE 'TIL- '.                     01980000
              03 FILLER    PIC X(05) VALUE 'GAR- '.                     01990000
              03 FILLER    PIC X(05) VALUE 'PR. '.                      02000000
              03 FILLER    PIC X(06) VALUE '      '.                    02010003
                                                                        02020000
007350     02 HEAD-6.                                                   02030000
007400        03 FILLER    PIC X(13) VALUE 'FØDSELSNR'.                 02040000
007400        03 FILLER    PIC X(09) VALUE 'NAVN     '.                 02050000
007400        03 FILLER    PIC X(01) VALUE ' '.                         02060000
              03 FILLER    PIC X(04) VALUE '1 2 '.                      02070000
              03 FILLER    PIC X(05) VALUE 'P67% '.                     02080000
              03 FILLER    PIC X(06) VALUE 'SJON '.                     02090000
              03 FILLER    PIC X(06) VALUE 'AVD. '.                     02100000
              03 FILLER    PIC X(06) VALUE 'EGEN '.                     02110000
              03 FILLER    PIC X(06) VALUE 'LEGG '.                     02120000
              03 FILLER    PIC X(06) VALUE 'LEGG '.                     02130000
              03 FILLER    PIC X(06) VALUE 'LEGG '.                     02140000
      *       03 FILLER    PIC X(06) VALUE '1000 '.                     02150003
              03 FILLER    PIC X(06) VALUE 'LEGG '.                     02160000
              03 FILLER    PIC X(06) VALUE '92   '.                     02170000
              03 FILLER    PIC X(07) VALUE 'TEKT '.                     02180000
              03 FILLER    PIC X(06) VALUE ' GP. '.                     02190000
              03 FILLER    PIC X(06) VALUE ' TP. '.                     02200000
              03 FILLER    PIC X(06) VALUE ' GP. '.                     02210000
              03 FILLER    PIC X(06) VALUE ' TP. '.                     02220000
              03 FILLER    PIC X(05) VALUE 'LEGG '.                     02230000
              03 FILLER    PIC X(05) VALUE 'ANTI'.                      02240000
              03 FILLER    PIC X(05) VALUE 'MND.'.                      02250000
              03 FILLER    PIC X(06) VALUE '      '.                    02260003
                                                                        02270000
007750     02 LINJE-1.                                                  02280000
007850        03 FNR-UT      PIC 9(06)B99999.                           02290000
007900        03 FILLER      PIC X(1)  VALUE SPACES.                    02300000
007950        03 NAVN-UT     PIC X(09).                                 02310000
007950        03 FILLER      PIC X(01) VALUE SPACE.                     02320000
              03 PSTAT1-UT   PIC X.                                     02330000
              03 FILLER      PIC X     VALUE SPACES.                    02340000
              03 PSTAT2-UT   PIC X.                                     02350000
008100        03 FILLER      PIC X     VALUE SPACES.                    02360000
              03 UGRAD-UT    PIC ZZZ.                                   02370000
008200        03 FILLER      PIC X     VALUE SPACES.                    02380000
008250        03 GRP-UT      PIC Z(4)9.                                 02390000
008300        03 FILLER      PIC X     VALUE SPACES.                    02400000
008350        03 TPAVD-UT    PIC Z(4)9.                                 02410000
008400        03 FILLER      PIC X     VALUE SPACES.                    02420000
008450        03 TPEGEN-UT   PIC Z(4)9.                                 02430000
              03 FILLER      PIC X     VALUE SPACES.                    02440000
008550        03 EFORTIL-UT  PIC Z(4)9.                                 02450000
              03 FILLER      PIC X     VALUE SPACES.                    02460000
008650        03 BFORTIL-UT  PIC Z(4)9.                                 02470000
              03 FILLER      PIC X     VALUE SPACES.                    02480000
008650        03 SBTILL-UT   PIC Z(4)9.                                 02490000
              03 FILLER      PIC X     VALUE SPACES.                    02500000
008650*       03 TAI-UT      PIC Z(4)9.                                 02510003
      *       03 FILLER      PIC X     VALUE SPACES.                    02520003
008650        03 AFP-UT      PIC Z(4)9.                                 02530000
              03 FILLER      PIC X     VALUE SPACES.                    02540000
008650        03 GT-LOV92-UT PIC Z(4)9.                                 02550000
              03 FILLER      PIC X     VALUE SPACES.                    02560000
                                                                        02570000
008750        03 FORINNT-UT  PIC Z(5)9.                                 02580000
              03 FILLER      PIC X     VALUE SPACES.                    02590000
              03 NETTO-GP-UT PIC Z(4)9.                                 02600000
              03 FILLER      PIC X     VALUE SPACES.                    02610000
              03 NETTO-TP-UT PIC Z(4)9.                                 02620000
008900        03 FILLER      PIC X     VALUE SPACES.                    02630000
008950        03 VGRP-UT     PIC Z(4)9.                                 02640000
008900        03 FILLER      PIC X     VALUE SPACES.                    02650000
              03 VTLP-UT     PIC Z(4)9.                                 02660000
008900        03 FILLER      PIC X     VALUE SPACES.                    02670000
009150        03 SAERTIL-UT  PIC Z(4)9.                                 02680000
009200        03 FILLER      PIC X   VALUE SPACES.                      02690000
              03 GT-TP-UT    PIC ZZZ9.                                  02700000
              03 FILLER      PIC X     VALUE SPACES.                    02710000
009350        03 SUMYD-UT    PIC ZZZZ9.                                 02720000
      * TA BORT DE 3 NESTE LINJENE                   TUYEN 8.7.91       02730000
009400*    02 NR-NAVN.                                                  02740000
      *       03 TRK-NR    PIC 9(4).                                    02750000
      *       03 TRK-NAVN  PIC X(23).                                   02760000
009550     02 KORT.                                                     02770000
009600        03 ID-X      PIC X(9)    VALUE SPACES.                    02780000
009650        03 FILLER    PIC X(7)    VALUE SPACES.                    02790000
              03 GRBELOP   PIC 9(5)    VALUE ZERO.                      02800000
009750        03 PPSTAT    PIC X.                                       02810000
009800        03 K-SPROS   PIC X(4).                                    02820000
009850        03 K-DATO    PIC X(13).                                   02830000
009851        03 FILLER    PIC X(1)    VALUE SPACES.                    02840000
009900        03 B-DATO    PIC X(13).                                   02850000
009950        03 FILLER    PIC X(27)   VALUE SPACES.                    02860000
010000 PROCEDURE DIVISION.                                              02870000
010050 START--X.                                                        02880000
010100     OPEN INPUT INFILE.                                           02890000
010100     OPEN OUTPUT UTFILE.                                          02900000
010150     OPEN INPUT PARAMFILE.                                        02910000
010200 LES-KORT.                                                        02920000
010250     ACCEPT KORT.                                                 02930000
010300     IF ID-X = 'FO04F101:', GO TO FLYTT-DATO.                     02940000
010350     DISPLAY KORT, ' PARAM-KORT ER GALT'.                         02950000
010400     STOP RUN.                                                    02960000
010450     GO TO LES-KORT.                                              02970000
010500 FLYTT-DATO.                                                      02980000
           MOVE GRBELOP TO BELOP.                                       02990000
010600     MOVE K-DATO  TO H2C-DATO1                                    03000000
010650     MOVE B-DATO  TO H2C-DATO2.                                   03010000
010700 LES-PARAM.                                                       03020000
010750     READ PARAMFILE AT END GO TO SLUTT.                           03030000
010800     IF K-ART = 'P' GO TO TEST-TKNR.                              03040000
010850     STOP 'PARAMETERKORT MANGLER'.                                03050000
010900     GO TO LES-PARAM.                                             03060000
010950 TEST-TKNR.                                                       03070000
011000     IF TKNR = 'AAAA'                                             03080000
011050     MOVE '9' TO SW-ALLE                                          03090000
011100     GO TO LES.                                                   03100000
011150     MOVE TKNR TO TKNR-TEST.                                      03110000
011200     CALL 'R001NRC' USING TKNR-TEST, SVAR.                        03120000
011250     IF SVAR = '0'                                                03130000
011300     GO TO LES.                                                   03140000
011350     STOP 'UGYLDIG TKNR I PARAMETERKORT'.                         03150000
011400     GO TO LES-PARAM.                                             03160000
011450 LES.                                                             03170000
011500     READ INFILE, AT END GO TO SLUTT.                             03180000
011550     IF PPSTAT = ' ' NEXT SENTENCE,                               03190000
011600     ELSE IF PPSTAT NOT = PSTAT1 GO TO LES.                       03200000
011650     IF SW-ALLE = '9'                                             03210000
011700     NEXT SENTENCE ELSE                                           03220000
011750     IF TRKNR < TKNR                                              03230000
011800     GO TO LES ELSE                                               03240000
011850     IF TRKNR > TKNR                                              03250000
011900     GO TO LES-PARAM.                                             03260000
011950     IF SIDE-AKK = ZEROS MOVE TRKNR TO TRK-AKK,                   03270000
012000     GO TO SKRIV-HEADING.                                         03280000
012050*    IF W-AVD   NOT = AVD   GO TO SKRIV-HEADING.                  03290000
           IF TRK-AKK NOT = TRKNR GO TO SKRIV-HEADING.                  03300000
012100     IF LINJE-AKK = 32 GO TO SKRIV-HEADING.                       03310000
012150     GO TO SKRIV-LINJER.                                          03320000
012200 SKRIV-HEADING.                                                   03330000
012250     ADD 1 TO SIDE-AKK.                                           03340000
012300     IF TRK-AKK NOT = TRKNR MOVE 1 TO SIDE-AKK.                   03350000
012350     MOVE ZEROS TO LINJE-AKK.                                     03360000
012400     MOVE SIDE-AKK TO SIDENR.                                     03370000
012450     MOVE TRKNR TO TRKNR-UT, TRK-NR, TRK-AKK.                     03380000
      *    MOVE AVD   TO W-AVD.                                         03390000
012500*    CALL 'TRKNAVN' USING NR-NAVN.                                03400000
012500*    LEGG INN NESTE LINJE ISTEDENFOR           TUYEN 8.7.91       03410000
012500     CALL 'R001NAC' USING TRK-NR, TRK-NAVN.                       03420000
012550     MOVE TRK-NAVN TO TKNAVN.                                     03430000
      *    IF TRK-NR = 1201                                             03440000
      *       MOVE 'AVDELING: ' TO FIL-1                                03450000
      *       MOVE AVD          TO FIL-2                                03460000
      *    ELSE                                                         03470000
      *       MOVE '          ' TO FIL-1                                03480000
      *       MOVE AVD          TO FIL-2.                               03490000
      *                                                                 03500000
012600     WRITE REC-2 FROM HEAD-1 AFTER NS.                            03510000
012650     WRITE REC-2 FROM HEAD-2 AFTER 1.                             03520000
012700     WRITE REC-2 FROM HEAD-2A AFTER 1.                            03530000
012750     WRITE REC-2 FROM HEAD-2C AFTER 1.                            03540000
012850     WRITE REC-2 FROM HEAD-3 AFTER 3.                             03550000
012900     WRITE REC-2 FROM HEAD-4 AFTER 1.                             03560000
012950     WRITE REC-2 FROM HEAD-5 AFTER 1.                             03570000
013000     WRITE REC-2 FROM HEAD-6 AFTER 1.                             03580000
013050 SKRIV-LINJER.                                                    03590000
013100     ADD 1 TO LINJE-AKK.                                          03600000
013150     MOVE UGRAD TO UGRAD-UT.                                      03610000
013200     MOVE FNR TO FNR-UT.                                          03620000
013250     MOVE NAVN TO NAVN-UT.                                        03630000
013200     MOVE FNR TO W-FNR.                                           03640000
           MOVE NAVN TO  NAVN-UT.                                       03650000
                                                                        03660000
013400     MOVE PSTAT1 TO PSTAT1-UT.                                    03670000
           MOVE PSTAT2 TO PSTAT2-UT.                                    03680000
013450     MOVE GRP TO GRP-UT.                                          03690000
013500     MOVE TPAVD TO TPAVD-UT.                                      03700000
013550     MOVE TPEGEN TO TPEGEN-UT.                                    03710000
                                                                        03720000
013600     COMPUTE  X-FELT    =  EFORTIL  + EK-GR.                      03730000
           MOVE X-FELT  TO EFORTIL-UT.                                  03740000
                                                                        03750000
013600     COMPUTE  X-FELT    = BFORTIL  + BA-GR.                       03760000
           MOVE X-FELT  TO BFORTIL-UT.                                  03770000
                                                                        03780000
013600     COMPUTE  X-FELT    = SB-TILL  + SB-GR.                       03790000
           MOVE X-FELT  TO SBTILL-UT.                                   03800000
                                                                        03810000
           COMPUTE X-ALDER = (W-AR + 70) * 100 + W-MN.                  03820000
      *    IF X-ALDER < 9506                                            03830003
      *       MOVE 0 TO TAI.                                            03840003
                                                                        03850000
      *    MOVE TAI TO TAI-UT.                                          03860003
           MOVE AFP TO AFP-UT.                                          03870000
           MOVE GT-L92 TO GT-LOV92-UT.                                  03880000
                                                                        03890000
           COMPUTE FORINNT-AKK = FORINNT * 100.                         03900000
013700     MOVE FORINNT-AKK TO FORINNT-UT.                              03910000
           MOVE NETTO-GP TO NETTO-GP-UT.                                03920000
           MOVE NETTO-TP TO NETTO-TP-UT.                                03930000
           MOVE VGRP TO VGRP-UT.                                        03940000
           MOVE VTLP TO VTLP-UT.                                        03950000
014050*    MOVE TIL851 TO TIL851-UT.                                    03960000
014100     MOVE SAERTIL TO SAERTIL-UT.                                  03970000
014150     MOVE GT-TP   TO GT-TP-UT.                                    03980000
014200     MOVE SUMYD TO SUMYD-UT.                                      03990000
014250     IF LINJE-AKK = 1                                             04000000
014300     WRITE REC-2 FROM LINJE-1 AFTER 2, ELSE                       04010000
014350     WRITE REC-2 FROM LINJE-1 AFTER 1.                            04020000
014400     GO TO LES.                                                   04030000
014450 SLUTT.                                                           04040000
014500     CLOSE INFILE, UTFILE.                                        04050000
014550     STOP RUN.                                                    04060000
