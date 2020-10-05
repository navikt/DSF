       IDENTIFICATION   DIVISION.                                       COF00010
       PROGRAM-ID. TELL-TRANS.                                          COF00020
       REMARKS.  **************************************************     COF00030
                 * PROGRAMMET PLUKKER UT DE DELER AV FR-REG       *     COF00040
                 * SOM SKAL KJØRES VIA STANARD-SYSTEMENE.         *     COF00040
                 *                                                *     COF00040
                 *                                                *     COF00040
                 *    RFOLKE   REGISTER.                          *     COF00040
                 *    LRECL=187                                   *     COF00040
                 **************************************************     COF00080
       AUTHER. CURTIS.                                                  COF00090
                                                                        COF00100
       ENVIRONMENT      DIVISION.                                       COF00110
                                                                                
       CONFIGURATION     SECTION.                                               
                                                                                
       OBJECT-COMPUTER. IBM-370,                                                
           PROGRAM COLLATING SEQUENCE IS IDIOT.                                 
                                                                                
       SPECIAL-NAMES.                                                           
           C01 IS KANAL-1                                                       
           , IDIOT IS 'ABCDEFGHIJKLMNOPQRSTUVWXYZÆØÅ'.                          
                                                                                
       INPUT-OUTPUT      SECTION.                                       COF00130
       FILE-CONTROL.                                                    COF00140
           SELECT TRKNR ASSIGN TO UT-S-SYSIN.                           COF00150
           SELECT UT-FR ASSIGN TO UT-S-OUTPUT.                          COF00150
           SELECT INN-FR ASSIGN TO UT-S-INPUT.                          COF00150
           SELECT UTLIST ASSIGN TO UT-S-SYSUT1.                         COF00160
       DATA             DIVISION.                                       COF00170
       FILE              SECTION.                                       COF00180
       FD  TRKNR                                                        COF00190
           LABEL RECORD IS OMITTED                                      COF00200
           BLOCK CONTAINS 0 RECORDS                                     COF00210
           DATA RECORD IS TRKTR.                                        COF00220
       01  TRKTR.                                                       COF00230
        02 FELTKODE     PIC X(2).                                       COF00240
        02 TRK-NR       PIC 9(4).                                       COF00240
        02 FILLER       PIC X(74).                                      COF00240
                                                                                
       FD  INN-FR                                                       COF00190
           LABEL RECORD IS OMITTED                                      COF00200
           BLOCK CONTAINS 0 RECORDS                                     COF00210
           DATA RECORD IS INNFR1.                                       COF00220
       01  INNFR1.                                                      COF00230
        02 INN-FNR     PIC S9(11) COMP-3.                                       
        02 FILLER      PIC X(59).                                               
        02 NAVN        PIC X(26).                                       COF00240
        02 INN-TRKNR   PIC S9(4) COMP-3.                                        
        02 ADDR        PIC X(71).                                               
        02 FILLER      PIC X(22).                                               
                                                                        COF00270
       FD  UT-FR                                                        COF00280
           LABEL RECORD IS OMITTED                                      COF00290
           BLOCK CONTAINS 0 RECORDS                                     COF00300
           DATA RECORD IS OUT-PUT.                                      COF00310
       01  OUT-PUT.                                                             
        02 UT-TRKNR     PIC 9(4).                                               
        02 UT-FNR       PIC 9(11).                                              
        02 UT-NAVN      PIC X(26).                                              
        02 UT-ADDR      PIC X(71).                                              
                                                                        COF00270
       FD  UTLIST                                                       COF00280
           LABEL RECORD IS OMITTED                                      COF00290
           BLOCK CONTAINS 0 RECORDS                                     COF00300
           DATA RECORD IS PRINT-REC1.                                   COF00310
       01  PRINT-REC1.                                                          
        02 PRINT1      PIC X(133).                                              
                                                                                
       WORKING-STORAGE   SECTION.                                       COF00360
                                                                                
       01  TELLER1  PIC 99999.                                                  
       PROCEDURE        DIVISION.                                       COF00440
           MOVE ZERO TO TELLER1.                                                
           OPEN INPUT INN-FR TRKNR OUTPUT UTLIST UT-FR.                 COF00450
       LESE.                                                            COF00460
           READ INN-FR AT END GO TO SLUTT.                              COF00470
           READ TRKNR AT END GO TO SLUTT.                               COF00470
       TEST-TRKNR.                                                              
           IF TRK-NR = INN-TRKNR GO TO FEIL-TRANS.                              
           IF TRK-NR > INN-TRKNR GO TO SKRIV-UT.                                
           IF TRK-NR < INN-TRKNR GO TO LES-TRK.                                 
       LES-TRK.                                                                 
           READ TRKNR AT END GO TO SLUTT.                                       
           GO TO TEST-TRKNR.                                                    
       FEIL-TRANS.                                                              
           READ INN-FR AT END GO TO SLUTT.                                      
           GO TO TEST-TRKNR.                                                    
       SKRIV-UT.                                                                
           ADD 1 TO TELLER1.                                                    
           MOVE INN-FNR TO UT-FNR.                                              
           MOVE INN-TRKNR TO UT-TRKNR.                                          
           MOVE NAVN TO UT-NAVN.                                                
           MOVE ADDR TO UT-ADDR.                                                
           WRITE OUT-PUT.                                                       
           READ INN-FR AT END GO TO SLUTT.                              COF00470
           GO TO TEST-TRKNR.                                                    
       SLUTT.                                                                   
           WRITE PRINT-REC1 FROM TELLER1 AFTER ADVANCING KANAL-1.               
           CLOSE INN-FR TRKNR UTLIST UT-FR.                             COF00650
           STOP RUN.                                                    COF00660
