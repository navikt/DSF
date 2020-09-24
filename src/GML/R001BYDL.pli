       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    R001BYDL.                                                 
       AUTHOR.        ARNE AMUNDSEN - AD ASSISTANSE A/S.                        
       DATE-COMPILED.                                                           
      *+---------------------------------------------------------------+        
      *!   FORMÅL : FINNER RIKTIG TRYGDEKONTORNR FOR OSLO OG AVD.NR    !        
      *!            FOR BERGEN                                         !        
      *!   ÅRSAK  : FILENE INNEHOLDER KOMMUNENR, MEN RAPPORTEN SKAL    !        
      *!            SKAL UT PR. TRYGDEKONTOR  / AVD.NR                 !        
      *!   INPUT  : P002.PPREGåå                                       !        
      *!   OPPSLAG: P001.VSKJEDE                                       !        
      *!            P293.IPMAIN                                        !        
      *!            P285.FRMAIN                                        !        
      *!            P290.BYDEL.VSAM                                    !        
      *!   OUTPUT : P004.PPREGåå                                       !        
      *+---------------------------------------------------------------+        
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
                                                                                
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
                                                                                
           SELECT REG   ASSIGN REG.                                             
           SELECT NYREG ASSIGN NYREG.                                           
                                                                                
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
       FD  REG                                                                  
           LABEL RECORDS STANDARD                                               
           BLOCK CONTAINS 0 RECORDS                                             
           DATA  RECORDS I-REC.                                                 
       01  I-REC.                                                               
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
003450        03 SORTNAVN     PICTURE X(25).                            00000500
                                                                                
       FD  NYREG                                                                
           LABEL RECORDS STANDARD                                               
           BLOCK CONTAINS 0 RECORDS                                             
           DATA  RECORDS NYREG-REC.                                             
       01  NYREG-REC.                                                           
              03 T-FNR         PIC S9(11)  COMP-3.                      00000290
              03 T-NAVN        PIC X(25).                               00000300
              03 T-PSTAT1      PIC X.                                   00000310
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
003000        03 T-AVD         PIC X.                                   00000400
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
              03 SORTNAVN     PICTURE X(25).                            00000500
                                                                                
                                                                                
       WORKING-STORAGE SECTION.                                                 
       01  IP01-PERSON-SEGM.                                                    
           10  IP01-PERSKEY.                                                    
               15  IP01-PERSNKEY   PIC X(7).                                    
               15  IP01-FNR-SISTE  PIC X.                                       
           10  IP01-PERSONKEY REDEFINES IP01-PERSKEY PIC S9(15) COMP-3.         
           10  IP01-NAVN           PIC X(25).                                   
           10  IP01-ADRESSE        PIC X(30).                                   
           10  IP01-POSTNR         PIC X(4).                                    
           10  IP01-TKAVD          PIC X(1).                                    
           10  IP01-BOKOMM         PIC X(4).                                    
           10  FILLER              PIC X(86).                                   
           10  IP01-SYSTAVD        PIC X(1).                                    
           10  FILLER              PIC X(20).                                   
       01  IP01-SSA1-PERSKEY.                                                   
           10  IP01-SSA1-SEGM-FELT PIC X(17) VALUE 'IP0PERSN(PERSNKEY'.         
           10  IP01-SSA1-REL-OP    PIC X(2)  VALUE ' ='.                        
           10  IP01-SSA1-PERSNKEY  PIC X(7).                                    
           10  IP01-SSA1-HP        PIC X     VALUE ')'.                         
                                                                                
                                                                                
       01  W-FNR-SNUDD.                                                         
           05  W-AR                PIC 99.                                      
           05  W-MND               PIC 99.                                      
           05  W-DAG               PIC 99.                                      
           05  W-PERSONNR          PIC 9(5).                                    
       01  W-FNR REDEFINES W-FNR-SNUDD PIC 9(11).                               
       01  W-TKNR                  PIC 99999 VALUE 0.                           
       01  W-IP-KEY.                                                            
           05  W-IP-TKNR           PIC 9999.                                    
           05  W-IP-FNR            PIC 9(11).                                   
       01  W-IP-KEYN REDEFINES W-IP-KEY PIC 9(15).                              
       01  IP-KEY-PACK             PIC S9(15) COMP-3.                           
       01  IP-KEY-X REDEFINES IP-KEY-PACK PIC X(7).                             
       01  W-ADRESSE.                                                           
           05  W-POSTNR            PIC X(4).                                    
           05  FILLER              PIC X(17).                                   
       01  W-STATUS                PIC 99.                                      
       01  MANGLER                 PIC X.                                       
       01  W-DD                    PIC 99    VALUE 0.                           
       01  W-KOMM                  PIC 9999  VALUE 0.                           
       01  DLI-GU                  PIC X(4)  VALUE 'GU  '.                      
                                                                                
       LINKAGE SECTION.                                                         
       01  IP-PCB.                                                              
           05  FILLER              PIC X(10).                                   
           05  IP-STATUS           PIC XX.                                      
           05  FILLER              PIC X(8).                                    
           05  IP-SEGM-NAVN        PIC X(8).                                    
           05  FILLER              PIC X(34).                                   
                                                                                
      *--------------------------------------------------------                 
       PROCEDURE DIVISION.                                                      
       0000.                                                                    
           ENTRY   'DLITCBL' USING IP-PCB.                                      
                                                                                
           OPEN     INPUT          REG.                                         
           OPEN     OUTPUT         NYREG.                                       
                                                                                
       1000.                                                                    
           READ     REG                                                         
             AT END GO TO 9000.                                                 
                                                                                
           IF   T-TKNR IN I-REC = 1201                                          
                PERFORM BERGEN                                                  
           ELSE                                                                 
                MOVE  ' '  TO T-AVD.                                            
                                                                                
           MOVE CORRESPONDING I-REC TO NYREG-REC .                              
           WRITE    NYREG-REC.                                                  
           GO TO    1000.                                                       
       9000.                                                                    
           CLOSE    REG                                                         
                    NYREG.                                                      
           STOP RUN.                                                            
                                                                                
       BERGEN SECTION.                                                          
                                                                                
           MOVE     T-FNR IN I-REC     TO W-FNR.                                
           MOVE     W-DAG              TO W-DD.                                 
           MOVE     W-AR               TO W-DAG.                                
           MOVE     W-DD               TO W-AR.                                 
           MOVE     W-FNR              TO W-IP-FNR.                             
           MOVE     T-TKNR IN I-REC    TO W-IP-TKNR.                            
           MOVE     W-IP-KEYN          TO IP-KEY-PACK.                          
           MOVE     IP-KEY-X           TO IP01-SSA1-PERSNKEY.                   
           CALL     'CBLTDLI' USING DLI-GU                                      
                                    IP-PCB                                      
                                    IP01-PERSON-SEGM                            
                                    IP01-SSA1-PERSKEY.                          
                                                                                
           IF       IP-STATUS NOT = '  '                                        
                    EXHIBIT NAMED T-FNR IN I-REC IP-STATUS W-IP-KEY             
                                                                                
                    MOVE  ' ' TO T-AVD                                          
                    GO TO 9999.                                                 
       8000.                                                                    
           MOVE     IP01-SYSTAVD TO T-AVD.                                      
           MOVE     IP01-BOKOMM TO W-KOMM.                                      
       9999.                                                                    
           EXIT.                                                                
