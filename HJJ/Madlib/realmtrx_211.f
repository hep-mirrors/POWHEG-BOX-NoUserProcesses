      SUBROUTINE SREALMTRX_211(P1,ANS)
C  
C Generated by MadGraph II                                              
C MadGraph StandAlone Version
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)
C  
C FOR PROCESS : g d~ -> h d d~ d~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      Include "nexternal.inc"
      INTEGER                 NCOMB,     NCROSS         
      PARAMETER (             NCOMB=  32, NCROSS=  1)
      INTEGER    THEL
      PARAMETER (THEL=NCOMB*NCROSS)
      INTEGER NGRAPHS
      PARAMETER (NGRAPHS=  24)
C  
C ARGUMENTS 
C  
      REAL*8 P1(0:3,NEXTERNAL),ANS(NCROSS)
C  
C LOCAL VARIABLES 
C  
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL,NCOMB),NTRY
      REAL*8 T
      REAL*8 REALMTRX_211
      REAL*8 ZERO
      PARAMETER(ZERO=0d0)
      INTEGER IHEL,IDEN(NCROSS),IC(NEXTERNAL,NCROSS)
      INTEGER IPROC,JC(NEXTERNAL), I,L,K
      LOGICAL GOODHEL(NCOMB,NCROSS)
      DATA NTRY/0/
      INTEGER NGOOD,igood(ncomb),jhel
      data ngood /0/
      save igood,jhel
      REAL*8 hwgt
      integer maxamps
      parameter (maxamps=6000)
      Double Precision amp2(maxamps), jamp2(0:maxamps)
      common/to_Ramps_211/  amp2,       jamp2

      integer j,jj
      integer max_bhel
      parameter ( max_bhel =          32 )

      INTEGER NCOLOR
      DATA NCOLOR   /   8/          
      DATA GOODHEL/THEL*.FALSE./
      DATA (NHEL(IHEL,   1),IHEL=1, 6) /-1,-1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,   2),IHEL=1, 6) /-1,-1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,   3),IHEL=1, 6) /-1,-1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,   4),IHEL=1, 6) /-1,-1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,   5),IHEL=1, 6) /-1,-1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,   6),IHEL=1, 6) /-1,-1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,   7),IHEL=1, 6) /-1,-1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,   8),IHEL=1, 6) /-1,-1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,   9),IHEL=1, 6) /-1, 1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  10),IHEL=1, 6) /-1, 1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  11),IHEL=1, 6) /-1, 1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  12),IHEL=1, 6) /-1, 1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  13),IHEL=1, 6) /-1, 1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  14),IHEL=1, 6) /-1, 1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  15),IHEL=1, 6) /-1, 1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  16),IHEL=1, 6) /-1, 1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,  17),IHEL=1, 6) / 1,-1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  18),IHEL=1, 6) / 1,-1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  19),IHEL=1, 6) / 1,-1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  20),IHEL=1, 6) / 1,-1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  21),IHEL=1, 6) / 1,-1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  22),IHEL=1, 6) / 1,-1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  23),IHEL=1, 6) / 1,-1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  24),IHEL=1, 6) / 1,-1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,  25),IHEL=1, 6) / 1, 1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  26),IHEL=1, 6) / 1, 1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  27),IHEL=1, 6) / 1, 1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  28),IHEL=1, 6) / 1, 1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  29),IHEL=1, 6) / 1, 1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  30),IHEL=1, 6) / 1, 1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  31),IHEL=1, 6) / 1, 1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  32),IHEL=1, 6) / 1, 1,-1, 1, 1, 1/
      DATA (  IC(IHEL,  1),IHEL=1, 6) / 1, 2, 3, 4, 5, 6/
      DATA (IDEN(IHEL),IHEL=  1,  1) / 192/
C ----------
C BEGIN CODE
C ----------
      NTRY=NTRY+1
      DO IPROC=1,NCROSS
      DO IHEL=1,NEXTERNAL
         JC(IHEL) = +1
      ENDDO
      DO IHEL=1,NGRAPHS
          amp2(ihel)=0d0
      ENDDO
      jamp2(0)=dble(NCOLOR)
      DO IHEL=1,int(jamp2(0))
          jamp2(ihel)=0d0
      ENDDO
      ANS(IPROC) = 0D0
          DO IHEL=1,NCOMB
             IF (GOODHEL(IHEL,IPROC) .OR. NTRY .LT. 2) THEN
                 T=REALMTRX_211(P1,NHEL(1,IHEL),IHEL,JC(1))              
               ANS(IPROC)=ANS(IPROC)+T
               IF (T .GT. 0D0 .AND. .NOT. GOODHEL(IHEL,IPROC)) THEN
                   GOODHEL(IHEL,IPROC)=.TRUE.
                   NGOOD = NGOOD +1
                   IGOOD(NGOOD) = IHEL
               ENDIF
             ENDIF
          ENDDO
      ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC))
      ENDDO
      END
       
       
      REAL*8 FUNCTION REALMTRX_211(P,NHEL,HELL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : g d~ -> h d d~ d~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER (NGRAPHS=  24,NEIGEN=  8) 
      include "nexternal.inc"
      INTEGER    NWAVEFUNCS     , NCOLOR
      PARAMETER (NWAVEFUNCS=  43, NCOLOR=   8) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL), HELL
C  
C LOCAL VARIABLES 
C  
      INTEGER I,J
      COMPLEX*16 ZTEMP
      REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)
      COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)
      COMPLEX*16 W(18,NWAVEFUNCS)
C  
C GLOBAL VARIABLES
C  
      integer maxamps
      parameter (maxamps=6000)
      Double Precision amp2(maxamps), jamp2(0:maxamps)
      common/to_Ramps_211/  amp2,       jamp2
      integer max_bhel
      parameter ( max_bhel =          32 )
      include "coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1  )/            9/                                       
      DATA (CF(i,1  ),i=1  ,6  ) /    24,   -3,   -6,   21,   10,   -8/    
      DATA (CF(i,1  ),i=7  ,8  ) /     1,    1/                            
C               T[ 4, 6]T[ 2, 5, 1]                                        
      DATA Denom(2  )/            9/                                       
      DATA (CF(i,2  ),i=1  ,6  ) /    -3,   24,   21,   -6,    1,    1/    
      DATA (CF(i,2  ),i=7  ,8  ) /    10,   -8/                            
C               T[ 2, 5]T[ 4, 6, 1]                                        
      DATA Denom(3  )/            9/                                       
      DATA (CF(i,3  ),i=1  ,6  ) /    -6,   21,   24,   -3,   -8,   10/    
      DATA (CF(i,3  ),i=7  ,8  ) /     1,    1/                            
C               T[ 4, 6, 1]T[ 2, 5]                                        
      DATA Denom(4  )/            9/                                       
      DATA (CF(i,4  ),i=1  ,6  ) /    21,   -6,   -3,   24,    1,    1/    
      DATA (CF(i,4  ),i=7  ,8  ) /    -8,   10/                            
C               T[ 4, 6]T[ 2, 5, 1]                                        
      DATA Denom(5  )/            9/                                       
      DATA (CF(i,5  ),i=1  ,6  ) /    10,    1,   -8,    1,   24,   -6/    
      DATA (CF(i,5  ),i=7  ,8  ) /    21,   -3/                            
C               T[ 4, 5]T[ 2, 6, 1]                                        
      DATA Denom(6  )/            9/                                       
      DATA (CF(i,6  ),i=1  ,6  ) /    -8,    1,   10,    1,   -6,   24/    
      DATA (CF(i,6  ),i=7  ,8  ) /    -3,   21/                            
C               T[ 4, 5, 1]T[ 2, 6]                                        
      DATA Denom(7  )/            9/                                       
      DATA (CF(i,7  ),i=1  ,6  ) /     1,   10,    1,   -8,   21,   -3/    
      DATA (CF(i,7  ),i=7  ,8  ) /    24,   -6/                            
C               T[ 4, 5]T[ 2, 6, 1]                                        
      DATA Denom(8  )/            9/                                       
      DATA (CF(i,8  ),i=1  ,6  ) /     1,   -8,    1,   10,   -3,   21/    
      DATA (CF(i,8  ),i=7  ,8  ) /    -6,   24/                            
C               T[ 2, 6]T[ 4, 5, 1]                                        
C ----------
C BEGIN CODE
C ----------
      CALL VXXXXX(P(0,1   ),ZERO ,NHEL(1   ),-1*IC(1   ),W(1,1   ))        
      CALL OXXXXX(P(0,2   ),ZERO ,NHEL(2   ),-1*IC(2   ),W(1,2   ))        
      CALL SXXXXX(P(0,3   ),+1*IC(3   ),W(1,3   ))                         
      CALL OXXXXX(P(0,4   ),ZERO ,NHEL(4   ),+1*IC(4   ),W(1,4   ))        
      CALL IXXXXX(P(0,5   ),ZERO ,NHEL(5   ),-1*IC(5   ),W(1,5   ))        
      CALL IXXXXX(P(0,6   ),ZERO ,NHEL(6   ),-1*IC(6   ),W(1,6   ))        
      CALL FVIXXX(W(1,5   ),W(1,1   ),GG ,ZERO    ,ZERO    ,W(1,7   ))     
      CALL JIOXXX(W(1,7   ),W(1,4   ),GG ,ZERO    ,ZERO    ,W(1,8   ))     
      CALL JVSHXX(W(1,8   ),W(1,3   ),GH ,ZERO    ,ZERO    ,W(1,9   ))     
      CALL IOVXXX(W(1,6   ),W(1,2   ),W(1,9   ),GG ,AMP(1   ))             
      CALL JIOXXX(W(1,5   ),W(1,4   ),GG ,ZERO    ,ZERO    ,W(1,10  ))     
      CALL JVVXXX(W(1,10  ),W(1,1   ),G ,ZERO    ,ZERO    ,W(1,11  ))      
      CALL JVSHXX(W(1,11  ),W(1,3   ),GH ,ZERO    ,ZERO    ,W(1,12  ))     
      CALL IOVXXX(W(1,6   ),W(1,2   ),W(1,12  ),GG ,AMP(2   ))             
      CALL FVOXXX(W(1,4   ),W(1,1   ),GG ,ZERO    ,ZERO    ,W(1,13  ))     
      CALL JIOXXX(W(1,5   ),W(1,13  ),GG ,ZERO    ,ZERO    ,W(1,14  ))     
      CALL JVSHXX(W(1,14  ),W(1,3   ),GH ,ZERO    ,ZERO    ,W(1,15  ))     
      CALL IOVXXX(W(1,6   ),W(1,2   ),W(1,15  ),GG ,AMP(3   ))             
      CALL JVSHXX(W(1,1   ),W(1,3   ),GH ,ZERO    ,ZERO    ,W(1,16  ))     
      CALL FVOXXX(W(1,4   ),W(1,16  ),GG ,ZERO    ,ZERO    ,W(1,17  ))     
      CALL JIOXXX(W(1,5   ),W(1,17  ),GG ,ZERO    ,ZERO    ,W(1,18  ))     
      CALL IOVXXX(W(1,6   ),W(1,2   ),W(1,18  ),GG ,AMP(4   ))             
      CALL JVVXXX(W(1,10  ),W(1,16  ),G ,ZERO    ,ZERO    ,W(1,19  ))      
      CALL IOVXXX(W(1,6   ),W(1,2   ),W(1,19  ),GG ,AMP(5   ))             
      CALL FVOXXX(W(1,2   ),W(1,10  ),GG ,ZERO    ,ZERO    ,W(1,20  ))     
      CALL IOVXXX(W(1,6   ),W(1,20  ),W(1,16  ),GG ,AMP(6   ))             
      CALL FVOXXX(W(1,2   ),W(1,16  ),GG ,ZERO    ,ZERO    ,W(1,21  ))     
      CALL IOVXXX(W(1,6   ),W(1,21  ),W(1,10  ),GG ,AMP(7   ))             
      CALL FVIXXX(W(1,5   ),W(1,16  ),GG ,ZERO    ,ZERO    ,W(1,22  ))     
      CALL JIOXXX(W(1,22  ),W(1,4   ),GG ,ZERO    ,ZERO    ,W(1,23  ))     
      CALL IOVXXX(W(1,6   ),W(1,2   ),W(1,23  ),GG ,AMP(8   ))             
      CALL JVSHXX(W(1,10  ),W(1,3   ),GH ,ZERO    ,ZERO    ,W(1,24  ))     
      CALL FVOXXX(W(1,2   ),W(1,24  ),GG ,ZERO    ,ZERO    ,W(1,25  ))     
      CALL IOVXXX(W(1,6   ),W(1,25  ),W(1,1   ),GG ,AMP(9   ))             
      CALL JVVXXX(W(1,1   ),W(1,24  ),G ,ZERO    ,ZERO    ,W(1,26  ))      
      CALL IOVXXX(W(1,6   ),W(1,2   ),W(1,26  ),GG ,AMP(10  ))             
      CALL FVOXXX(W(1,2   ),W(1,1   ),GG ,ZERO    ,ZERO    ,W(1,27  ))     
      CALL IOVXXX(W(1,6   ),W(1,27  ),W(1,24  ),GG ,AMP(11  ))             
      CALL JVVSXX(W(1,1   ),W(1,10  ),W(1,3   ),DUM1 ,GH4 ,ZERO    ,       
     &     ZERO    ,W(1,28  ))                                             
      CALL IOVXXX(W(1,6   ),W(1,2   ),W(1,28  ),GG ,AMP(12  ))             
      CALL JIOXXX(W(1,5   ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,29  ))     
      CALL JVSHXX(W(1,29  ),W(1,3   ),GH ,ZERO    ,ZERO    ,W(1,30  ))     
      CALL FVOXXX(W(1,4   ),W(1,30  ),GG ,ZERO    ,ZERO    ,W(1,31  ))     
      CALL IOVXXX(W(1,6   ),W(1,31  ),W(1,1   ),GG ,AMP(13  ))             
      CALL JVVXXX(W(1,30  ),W(1,1   ),G ,ZERO    ,ZERO    ,W(1,32  ))      
      CALL IOVXXX(W(1,6   ),W(1,4   ),W(1,32  ),GG ,AMP(14  ))             
      CALL IOVXXX(W(1,6   ),W(1,13  ),W(1,30  ),GG ,AMP(15  ))             
      CALL JVVXXX(W(1,16  ),W(1,29  ),G ,ZERO    ,ZERO    ,W(1,33  ))      
      CALL IOVXXX(W(1,6   ),W(1,4   ),W(1,33  ),GG ,AMP(16  ))             
      CALL FVOXXX(W(1,4   ),W(1,29  ),GG ,ZERO    ,ZERO    ,W(1,34  ))     
      CALL IOVXXX(W(1,6   ),W(1,34  ),W(1,16  ),GG ,AMP(17  ))             
      CALL IOVXXX(W(1,6   ),W(1,17  ),W(1,29  ),GG ,AMP(18  ))             
      CALL JIOXXX(W(1,5   ),W(1,21  ),GG ,ZERO    ,ZERO    ,W(1,35  ))     
      CALL IOVXXX(W(1,6   ),W(1,4   ),W(1,35  ),GG ,AMP(19  ))             
      CALL JIOXXX(W(1,22  ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,36  ))     
      CALL IOVXXX(W(1,6   ),W(1,4   ),W(1,36  ),GG ,AMP(20  ))             
      CALL JIOXXX(W(1,7   ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,37  ))     
      CALL JVSHXX(W(1,37  ),W(1,3   ),GH ,ZERO    ,ZERO    ,W(1,38  ))     
      CALL IOVXXX(W(1,6   ),W(1,4   ),W(1,38  ),GG ,AMP(21  ))             
      CALL JVVXXX(W(1,1   ),W(1,29  ),G ,ZERO    ,ZERO    ,W(1,39  ))      
      CALL JVSHXX(W(1,39  ),W(1,3   ),GH ,ZERO    ,ZERO    ,W(1,40  ))     
      CALL IOVXXX(W(1,6   ),W(1,4   ),W(1,40  ),GG ,AMP(22  ))             
      CALL JIOXXX(W(1,5   ),W(1,27  ),GG ,ZERO    ,ZERO    ,W(1,41  ))     
      CALL JVSHXX(W(1,41  ),W(1,3   ),GH ,ZERO    ,ZERO    ,W(1,42  ))     
      CALL IOVXXX(W(1,6   ),W(1,4   ),W(1,42  ),GG ,AMP(23  ))             
      CALL JVVSXX(W(1,1   ),W(1,29  ),W(1,3   ),DUM1 ,GH4 ,ZERO    ,       
     &     ZERO    ,W(1,43  ))                                             
      CALL IOVXXX(W(1,6   ),W(1,4   ),W(1,43  ),GG ,AMP(24  ))             
      JAMP(   1) = +AMP(   1)-AMP(   2)-AMP(   5)+AMP(   8)+AMP(  10)
     &             +AMP(  12)
      JAMP(   2) = +AMP(   2)+AMP(   3)+AMP(   4)+AMP(   5)-AMP(  10)
     &             -AMP(  12)
      JAMP(   3) = +AMP(   6)+AMP(   9)
      JAMP(   4) = +AMP(   7)+AMP(  11)
      JAMP(   5) = -AMP(  13)-AMP(  17)
      JAMP(   6) = +AMP(  14)-AMP(  16)-AMP(  20)-AMP(  21)-AMP(  22)
     &             -AMP(  24)
      JAMP(   7) = -AMP(  14)+AMP(  16)-AMP(  19)+AMP(  22)-AMP(  23)
     &             +AMP(  24)
      JAMP(   8) = -AMP(  15)-AMP(  18)
      REALMTRX_211 = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP = (0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          REALMTRX_211 =REALMTRX_211+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
      END
       
       
