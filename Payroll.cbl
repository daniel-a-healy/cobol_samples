 000100 //KC03L93 JOB TIME=(,1)                                                 
 000200 //        EXEC IGYWCLG                                                  
 000300 //COBOL.SYSIN DD *                                                      
 000400        IDENTIFICATION DIVISION.                                         
 000500        PROGRAM-ID. SAMPLE.                                              
 000600        ENVIRONMENT DIVISION.                                            
 000700        INPUT-OUTPUT SECTION.                                            
 000800        FILE-CONTROL.                                                    
 000900              SELECT EMPLOYEE-DATA ASSIGN INDD.                          
 001000              SELECT DATAOUT ASSIGN OUTDD.                               
 001100        DATA DIVISION.                                                   
 001200        FILE SECTION.                                                    
 001300        FD EMPLOYEE-DATA RECORDING MODE F                                
 001400               LABEL RECORDS ARE OMITTED.                                
 001500        01 EMPLOYEE-IN.                                                  
 001600             02 NAME         PIC X(10).                                  
 001700             02 HOURS        PIC 9(2).                                   
 001800             02 PAY-RATE     PIC 9(2)V99.                                
 001900             02 DEPENDENTS   PIC 9(2).                                   
 001910             02 FILLER       PIC X(62).                                  
 002000        FD DATAOUT RECORDING MODE F                                      
 002100               LABEL RECORDS ARE OMITTED.                                
 002200        01 PRINTOUT PIC X(80).                                           
 002300        WORKING-STORAGE SECTION.                                         
 002400        01 EOF            PIC XXX VALUE "NO".                            
 002410        01 BLANK-LINE     PIC X(80).                                     
 002411        01 GROSS-PAY      PIC 9(4)V99.                                   
 002412        01 DEDUCTION      PIC 9(3)V99.                                   
 002420        01 TAXABLE-INCOME PIC 9(4)V99.                                   
 002430        01 FED-TAX        PIC 9(4)V99.                                   
 002440        01 STATE-TAX      PIC 9(3)V99.                                   
 002450        01 NET-PAY        PIC 9(4)V99.                                   
 002500        01 RECORD-OUT.                                                   
 002600             02 NAMEOUT          PIC X(10).                              
 002700             02 FILLER           PIC X(3) VALUE SPACE.
 002800             02 HOURSOUT         PIC 9(2).                               
 002900             02 FILLER           PIC X(3) VALUE SPACE.                   
 003000             02 PAY-RATE-OUT     PIC $Z9.99.                             
 003010             02 FILLER           PIC X(3) VALUE SPACE.                   
 003020             02 GROSS-PAY-OUT    PIC $ZZZZ.99.                           
 003030             02 FILLER           PIC X(3) VALUE SPACE.                   
 003040             02 DEDUCTION-OUT    PIC $ZZ9.99.                            
 003050             02 FILLER           PIC X(3) VALUE SPACE.                   
 003060             02 FED-TAX-OUT      PIC $ZZZZ.99.                           
 003070             02 FILLER           PIC X(3) VALUE SPACE.                   
 003080             02 STATE-TAX-OUT    PIC $ZZZ.99.                            
 003100             02 FILLER           PIC X(3) VALUE SPACE.                   
 003110             02 NET-PAY-OUT      PIC $ZZZZ.99.                           
 003200        01 HEADER-ONE.                                                   
 003300             02 FILLER PIC X(12)  VALUE "EMPLOYEE".                      
 003400             02 FILLER PIC X(8)   VALUE "HOURS".                         
 003500             02 FILLER PIC X(8)   VALUE "PAY".                           
 003510             02 FILLER PIC X(8)   VALUE "GROSS".                         
 003520             02 FILLER PIC X(12)  VALUE "DEPENDENT".                     
 003530             02 FILLER PIC X(11)  VALUE "FEDERAL".                       
 003540             02 FILLER PIC X(11)  VALUE "STATE".                         
 003550             02 FILLER PIC X(11)  VALUE "NET".                           
 003551             02 FILLER PIC X(3)   VALUE SPACE.                           
 003560        01 HEADER-TWO.                                                   
 003570             02 FILLER PIC X(20) VALUE "NAME".                           
 003580             02 FILLER PIC X(8)  VALUE "RATE".                           
 003590             02 FILLER PIC X(8)  VALUE "PAY".                            
 003591             02 FILLER PIC X(12) VALUE "DEDUCTION".                      
 003592             02 FILLER PIC X(11) VALUE "TAXES".                          
 003593             02 FILLER PIC X(11) VALUE "TAXES".                          
 003594             02 FILLER PIC X(7)  VALUE "PAY".                            
 003600        PROCEDURE DIVISION.                                              
 003700             OPEN INPUT EMPLOYEE-DATA OUTPUT DATAOUT.                    
 003800             WRITE PRINTOUT FROM HEADER-ONE                              
 003810             WRITE PRINTOUT FROM HEADER-TWO                              
 003820             WRITE PRINTOUT FROM BLANK-LINE                              
 003900             PERFORM READ-RECORD.                                        
 004000             PERFORM UNTIL EOF = "YES"  
 004100                MOVE NAME TO NAMEOUT                                     
 004200                MOVE HOURS TO HOURSOUT                                   
 004300                MOVE PAY-RATE TO PAY-RATE-OUT                            
 004310                PERFORM CALCULATE-GROSS-PAY                              
 004311                MOVE GROSS-PAY TO GROSS-PAY-OUT                          
 004320                PERFORM CALCULATE-DEDUCTION                              
 004321                MOVE DEDUCTION TO DEDUCTION-OUT                          
 004322                PERFORM CALCULATE-TAXABLE-INCOME                         
 004330                PERFORM CALCULATE-FED-TAX                                
 004331                MOVE FED-TAX TO FED-TAX-OUT                              
 004340                PERFORM CALCULATE-STATE-TAX                              
 004341                MOVE STATE-TAX TO STATE-TAX-OUT                          
 004350                PERFORM CALCULATE-NET-PAY                                
 004360                MOVE NET-PAY TO NET-PAY-OUT                              
 004400                WRITE PRINTOUT FROM RECORD-OUT                           
 004500                PERFORM READ-RECORD                                      
 004600             END-PERFORM.                                                
 004700            CLOSE EMPLOYEE-DATA, DATAOUT.                                
 004800            STOP RUN.                                                    
 004900        READ-RECORD.                                                     
 005000            READ EMPLOYEE-DATA AT END MOVE "YES" TO EOF                  
 005100            END-READ.                                                    
 005110        CALCULATE-GROSS-PAY.                                             
 005120            COMPUTE GROSS-PAY = HOURS * PAY-RATE.                        
 005130        CALCULATE-DEDUCTION.                                             
 005140            COMPUTE DEDUCTION = 25 * DEPENDENTS.                         
 005141        CALCULATE-TAXABLE-INCOME.                                        
 005142            COMPUTE TAXABLE-INCOME = GROSS-PAY - DEDUCTION.              
 005150        CALCULATE-FED-TAX.                                               
 005160            COMPUTE FED-TAX = TAXABLE-INCOME * 0.2.                      
 005170        CALCULATE-STATE-TAX.                                             
 005180            COMPUTE STATE-TAX = TAXABLE-INCOME * 0.1.                    
 005190        CALCULATE-NET-PAY.                                               
 005191            COMPUTE NET-PAY = GROSS-PAY - FED-TAX - STATE-TAX.           
 005200 /*                                                                      
 005300 //GO.INDD DD *                                                          
 005400 ADAMS     40200001                                                      
 005500 KERRIGAN  55300010
 005600 SMITH     35150003                                                      
 005610 HARRINGTON80750000                                                      
 005620 TEST CASE 20100000                                                      
 005630 TEST CASE 20100004                                                      
 005700 /*                                                                      
 005800 //GO.SYSOUT DD SYSOUT=*                                                 
 005900 //GO.SYSUDUMP DD SYSOUT=A                                               
 006000 //GO.OUTDD DD SYSOUT=A                                                  

                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                                                 