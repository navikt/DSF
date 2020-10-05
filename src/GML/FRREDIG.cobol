       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. LIST1X.                                                      
       DATE-WRITTEN.26/9/85.                                                    
       AUTHOR. CURTIS FLEKKE.                                                   
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SPECIAL-NAMES.                                                           
           C01 IS PAGE1.                                                        
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
           SELECT INPUTA ASSIGN TO UT-S-INPUT.                                  
             SELECT UTFIL ASSIGN TO UT-S-OUTPUT.                                
             SELECT INFIL ASSIGN TO UT-S-PARAM.                                 
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  INFIL                                                                
           LABEL RECORD IS OMITTED                                              
               BLOCK CONTAINS 0 RECORDS.                                        
       01  INN-AAR   PIC X(80).                                                 
       FD  INPUTA                                                               
           LABEL RECORD IS OMITTED                                              
               BLOCK CONTAINS 0 RECORDS.                                        
       01  INN-FR.                                                              
        02 TRKNR     PIC 9(4).                                                  
        02 FNR.                                                                 
         03 AAR      PIC 9(2).                                                  
         03 MND      PIC 9(2).                                                  
         03 DAG      PIC 9(2).                                                  
         03 FNR1     PIC 9(3).                                                  
         03 FNR2     PIC 9(2).                                                  
        02 NAVN      PIC X(26).                                                 
        02 GATE      PIC X(50).                                                 
        02 POSTSTED  PIC X(21).                                                 
                                                                                
       FD  UTFIL                                                                
           LABEL RECORD IS OMITTED                                              
               BLOCK CONTAINS 0 RECORDS.                                        
       01  UTPOSTEN    PIC  X(133).                                             
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  TELLER       PIC 9(2).                                               
                                                                                
       01  OMR-DATA.                                                            
        02 FILLER    PIC X(11) VALUE SPACES.                                    
        02 FNRO.                                                                
         03 DAGO     PIC 9(2).                                                  
         03 MNDO     PIC 9(2).                                                  
         03 AARO     PIC 9(2).                                                  
         03 S3       PIC X VALUE ' '.                                           
         03 FNR1O    PIC 9(3).                                                  
         03 S4       PIC X VALUE ' '.                                           
         03 FNR2O    PIC 9(2).                                                  
        02 FILLER    PIC X(4) VALUE SPACES.                                     
        02 NAVNO     PIC X(26).                                                 
        02 FILLER    PIC X(4) VALUE SPACES.                                     
        02 GATEO     PIC X(50).                                                 
        02 FILLER    PIC X(3) VALUE SPACES.                                     
        02 POSTSTEDO PIC X(21).                                                 
                                                                                
       01  OVERSKR.                                                             
        02 A PIC X VALUE SPACES.                                                
        02 B PIC X(17) VALUE 'PERSONNR.(DDMMÅÅ)'.                               
        02 FILLER PIC X(10).                                                    
        02 C PIC X(4) VALUE 'NAVN'.                                             
        02 FILLER PIC X(26).                                                    
        02 D PIC X(12) VALUE 'GATE ADRESSE'.                                    
        02 FILLER PIC X(41).                                                    
        02 E PIC X(12) VALUE 'POSTNR./STED'.                                    
        02 FILLER PIC X(19).                                                    
                                                                                
       01  TRK-OVERSK1.                                                         
        02 FILLER        PIC X(5).                                              
        02 TRKNR-L       PIC 9(4).                                              
        02 FILLER        PIC X(10).                                             
        02 LOVERSK       PIC X(15) VALUE 'FYLLER 67 ÅR I '.                     
        02 AARSTALL      PIC X(4).                                              
        02 FILLER        PIC X(85).                                             
                                                                                
       01  MRK-OVERSK.                                                          
        02 FILLER        PIC X(7).                                              
        02 TRKNR-M       PIC 9(4).                                              
                                                                                
       01  PARAMIN.                                                             
        02 INNAAR        PIC X(4).                                              
        02 FILLER        PIC X(76).                                             
                                                                                
       01  BLK PIC X(133) VALUE SPACES.                                         
       PROCEDURE DIVISION.                                                      
       STARTEN.                                                                 
           OPEN INPUT INPUTA INFIL                                              
                OUTPUT UTFIL.                                                   
           MOVE ZERO TO TRKNR-L.                                                
           MOVE ZERO TO TELLER.                                                 
           READ INFIL.                                                          
           MOVE INN-AAR TO PARAMIN.                                             
           MOVE INNAAR TO AARSTALL.                                             
       TEST-DATA.                                                               
           READ INPUTA AT END GO TO SLUTT.                                      
           IF TRKNR = TRKNR-L GO TO DANN-DATA.                                  
           GO TO DANN-NYSIDE.                                                   
       DANN-DATA.                                                               
           MOVE AAR TO AARO.                                                    
           MOVE MND TO MNDO.                                                    
           MOVE DAG TO DAGO.                                                    
           MOVE FNR1 TO FNR1O.                                                  
           MOVE FNR2 TO FNR2O.                                                  
           MOVE NAVN TO NAVNO.                                                  
           MOVE GATE TO GATEO.                                                  
           MOVE POSTSTED TO POSTSTEDO.                                          
       SKRIV-SIDE.                                                              
           IF TELLER = 45 GO TO DANN-NYSIDE.                                    
           IF TELLER > 45 GO TO DANN-NYSIDE.                                    
           WRITE UTPOSTEN FROM OMR-DATA.                                        
           WRITE UTPOSTEN FROM BLK.                                             
           ADD 2 TO TELLER.                                                     
           GO TO TEST-DATA.                                                     
       DANN-NYSIDE.                                                             
           MOVE ZERO TO TELLER.                                                 
           MOVE TRKNR TO TRKNR-L.                                               
           WRITE UTPOSTEN FROM BLK AFTER ADVANCING PAGE1.                       
           WRITE UTPOSTEN FROM TRK-OVERSK1.                                     
           WRITE UTPOSTEN FROM BLK.                                             
           WRITE UTPOSTEN FROM BLK.                                             
           WRITE UTPOSTEN FROM BLK.                                             
           WRITE UTPOSTEN FROM BLK.                                             
           WRITE UTPOSTEN FROM BLK.                                             
           WRITE UTPOSTEN FROM OVERSKR.                                         
           WRITE UTPOSTEN FROM BLK.                                             
           WRITE UTPOSTEN FROM BLK.                                             
           ADD 10 TO TELLER.                                                    
           GO TO DANN-DATA.                                                     
       SLUTT.                                                                   
           CLOSE INPUTA UTFIL.                                                  
           STOP RUN.                                                            
                                                                                
