      SUBROUTINE SB_SF_250_009(P1,ANS)
C  
C Generated by MadGraph II                                              
C MadGraph StandAlone Version
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL-1)
C  
C FOR PROCESS : g s~ -> s s~ s~ g  
C  
C BORN AMPLITUDE IS g s~ -> s s~ s~  
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
      PARAMETER (NGRAPHS=  10)
C  
C ARGUMENTS 
C  
      REAL*8 P1(0:3,NEXTERNAL-1),ANS(NCROSS)
C  
C LOCAL VARIABLES 
C  
      REAL*8 P(0:3,NEXTERNAL-1)
      INTEGER NHEL(NEXTERNAL-1,NCOMB),NTRY
      REAL*8 T
      REAL*8 B_SF_250_009
      REAL*8 ZERO
      PARAMETER(ZERO=0d0)
      INTEGER IHEL,IDEN(NCROSS),IC(NEXTERNAL-1,NCROSS)
      INTEGER IPROC,JC(NEXTERNAL-1), I,L,K
      LOGICAL GOODHEL(NCOMB,NCROSS)
      DATA NTRY/0/
      INTEGER NGOOD,igood(ncomb),jhel
      data ngood /0/
      save igood,jhel
      REAL*8 hwgt
      integer maxamps
      parameter (maxamps=6000)
      Double Precision amp2(maxamps), jamp2(0:maxamps)
      common/to_amps_250/  amp2,       jamp2

      integer j,jj
      integer max_bhel
      parameter ( max_bhel =          32 )
      double complex saveamp(ngraphs,max_bhel)
      common/to_saveamp_250/saveamp
      double precision savemom(nexternal-1,2)
      common/to_savemom_250/savemom

      INTEGER NCOLOR
      DATA NCOLOR   /   8/          
      DATA GOODHEL/THEL*.FALSE./
      DATA (NHEL(IHEL,   1),IHEL=1, 5) /-1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,   2),IHEL=1, 5) /-1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,   3),IHEL=1, 5) /-1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,   4),IHEL=1, 5) /-1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,   5),IHEL=1, 5) /-1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,   6),IHEL=1, 5) /-1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,   7),IHEL=1, 5) /-1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,   8),IHEL=1, 5) /-1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,   9),IHEL=1, 5) /-1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  10),IHEL=1, 5) /-1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  11),IHEL=1, 5) /-1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  12),IHEL=1, 5) /-1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  13),IHEL=1, 5) /-1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  14),IHEL=1, 5) /-1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  15),IHEL=1, 5) /-1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  16),IHEL=1, 5) /-1, 1, 1, 1, 1/
      DATA (NHEL(IHEL,  17),IHEL=1, 5) / 1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  18),IHEL=1, 5) / 1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  19),IHEL=1, 5) / 1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  20),IHEL=1, 5) / 1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  21),IHEL=1, 5) / 1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  22),IHEL=1, 5) / 1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  23),IHEL=1, 5) / 1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  24),IHEL=1, 5) / 1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,  25),IHEL=1, 5) / 1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  26),IHEL=1, 5) / 1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  27),IHEL=1, 5) / 1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  28),IHEL=1, 5) / 1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  29),IHEL=1, 5) / 1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  30),IHEL=1, 5) / 1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  31),IHEL=1, 5) / 1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  32),IHEL=1, 5) / 1, 1, 1, 1, 1/
      DATA (  IC(IHEL,  1),IHEL=1, 5) / 1, 2, 3, 4, 5/
      DATA (IDEN(IHEL),IHEL=  1,  1) / 192/
      logical calculatedBorn
      integer skip
      common/cBorn/calculatedBorn,skip
C ----------
C BEGIN CODE
C ----------
      NTRY=NTRY+1
      DO IPROC=1,NCROSS
      DO IHEL=1,NEXTERNAL-1
         JC(IHEL) = +1
      ENDDO
      DO IHEL=1,NGRAPHS
          amp2(ihel)=0d0
      ENDDO
      jamp2(0)=dble(NCOLOR)
      DO IHEL=1,int(jamp2(0))
          jamp2(ihel)=0d0
      ENDDO
      if (calculatedBorn) then
         do j=1,nexternal-1
            if (savemom(j,1).ne.p1(0,j) .or.
     &              savemom(j,2).ne.p1(3,j)) then
               calculatedBorn=.false.
C          write (*,*) "momenta not the same in Born"
            endif
         enddo
      endif
      if (.not.calculatedBorn) then
         do j=1,nexternal-1
            savemom(j,1)=p1(0,j)
            savemom(j,2)=p1(3,j)
         enddo
         do j=1,max_bhel
            do jj=1,ngraphs
               saveamp(jj,j)=(0d0,0d0)
            enddo
         enddo
      endif
      ANS(IPROC) = 0D0
          DO IHEL=1,NCOMB
             IF (GOODHEL(IHEL,IPROC) .OR. NTRY .LT. 2) THEN
                 T=B_SF_250_009(P1,NHEL(1,IHEL),IHEL,JC(1))              
               ANS(IPROC)=ANS(IPROC)+T
               IF (T .NE. 0D0 .AND. .NOT. GOODHEL(IHEL,IPROC)) THEN
                   GOODHEL(IHEL,IPROC)=.TRUE.
                   NGOOD = NGOOD +1
                   IGOOD(NGOOD) = IHEL
               ENDIF
             ENDIF
          ENDDO
      ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC))
      ENDDO
      calculatedBorn=.true.
      END
       
       
      REAL*8 FUNCTION B_SF_250_009(P,NHEL,HELL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL-1)
C  
C FOR PROCESS : g s~ -> s s~ s~ g  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER (NGRAPHS=  10,NEIGEN=  8) 
      include "nexternal.inc"
      INTEGER    NWAVEFUNCS     , NCOLOR
      PARAMETER (NWAVEFUNCS=  15, NCOLOR=   8) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 P(0:3,NEXTERNAL-1)
      INTEGER NHEL(NEXTERNAL-1), IC(NEXTERNAL-1), HELL
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
      common/to_amps_250/  amp2,       jamp2
      integer max_bhel
      parameter ( max_bhel =          32 )
      double complex saveamp(ngraphs,max_bhel)
      common/to_saveamp_250/saveamp
      logical calculatedBorn
      integer skip
      common/cBorn/calculatedBorn,skip
      include "coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1  )/            3/                                       
      DATA (CF(i,1  ),i=1  ,6  ) /    -3,    0,    3,    0,    5,   -5/    
      DATA (CF(i,1  ),i=7  ,8  ) /    -4,    4/                            
C               T[ 3, 5]T[ 2, 4, 1]                                        
      DATA Denom(2  )/            6/                                       
      DATA (CF(i,2  ),i=1  ,6  ) /     0,  -21,  -21,    0,   -8,   -1/    
      DATA (CF(i,2  ),i=7  ,8  ) /    -8,   -1/                            
C               T[ 2, 4]T[ 3, 5, 1]                                        
      DATA Denom(3  )/            6/                                       
      DATA (CF(i,3  ),i=1  ,6  ) /     6,  -21,  -24,    3,  -10,    8/    
      DATA (CF(i,3  ),i=7  ,8  ) /    -1,   -1/                            
C               T[ 3, 5, 1]T[ 2, 4]                                        
      DATA Denom(4  )/            6/                                       
      DATA (CF(i,4  ),i=1  ,6  ) /     0,    0,    3,    3,    8,   -1/    
      DATA (CF(i,4  ),i=7  ,8  ) /    -1,    8/                            
C               T[ 3, 5]T[ 2, 4, 1]                                        
      DATA Denom(5  )/            3/                                       
      DATA (CF(i,5  ),i=1  ,6  ) /     5,   -4,   -5,    4,   -3,    3/    
      DATA (CF(i,5  ),i=7  ,8  ) /     0,    0/                            
C               T[ 3, 4, 1]T[ 2, 5]                                        
      DATA Denom(6  )/            6/                                       
      DATA (CF(i,6  ),i=1  ,6  ) /   -10,   -1,    8,   -1,    6,  -24/    
      DATA (CF(i,6  ),i=7  ,8  ) /   -21,    3/                            
C               T[ 3, 4]T[ 2, 5, 1]                                        
      DATA Denom(7  )/            6/                                       
      DATA (CF(i,7  ),i=1  ,6  ) /    -8,   -8,   -1,   -1,    0,  -21/    
      DATA (CF(i,7  ),i=7  ,8  ) /   -21,    0/                            
C               T[ 3, 4]T[ 2, 5, 1]                                        
      DATA Denom(8  )/            6/                                       
      DATA (CF(i,8  ),i=1  ,6  ) /     8,   -1,   -1,    8,    0,    3/    
      DATA (CF(i,8  ),i=7  ,8  ) /     0,    3/                            
C               T[ 2, 5]T[ 3, 4, 1]                                        
C ----------
C BEGIN CODE
C ----------
      if (.not. calculatedBorn) then
      CALL VXXXXX(P(0,1   ),ZERO ,NHEL(1   ),-1*IC(1   ),W(1,1   ))        
      CALL OXXXXX(P(0,2   ),ZERO ,NHEL(2   ),-1*IC(2   ),W(1,2   ))        
      CALL OXXXXX(P(0,3   ),ZERO ,NHEL(3   ),+1*IC(3   ),W(1,3   ))        
      CALL IXXXXX(P(0,4   ),ZERO ,NHEL(4   ),-1*IC(4   ),W(1,4   ))        
      CALL IXXXXX(P(0,5   ),ZERO ,NHEL(5   ),-1*IC(5   ),W(1,5   ))        
      CALL FVIXXX(W(1,4   ),W(1,1   ),GG ,ZERO    ,ZERO    ,W(1,6   ))     
      CALL JIOXXX(W(1,5   ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,7   ))     
      CALL IOVXXX(W(1,6   ),W(1,3   ),W(1,7   ),GG ,AMP(1   ))             
      CALL FVOXXX(W(1,3   ),W(1,1   ),GG ,ZERO    ,ZERO    ,W(1,8   ))     
      CALL JIOXXX(W(1,4   ),W(1,8   ),GG ,ZERO    ,ZERO    ,W(1,9   ))     
      CALL IOVXXX(W(1,5   ),W(1,2   ),W(1,9   ),GG ,AMP(2   ))             
      CALL JIOXXX(W(1,4   ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,10  ))     
      CALL FVIXXX(W(1,5   ),W(1,1   ),GG ,ZERO    ,ZERO    ,W(1,11  ))     
      CALL IOVXXX(W(1,11  ),W(1,2   ),W(1,10  ),GG ,AMP(3   ))             
      CALL VVVXXX(W(1,1   ),W(1,7   ),W(1,10  ),G ,AMP(4   ))              
      CALL FVOXXX(W(1,2   ),W(1,1   ),GG ,ZERO    ,ZERO    ,W(1,12  ))     
      CALL IOVXXX(W(1,5   ),W(1,12  ),W(1,10  ),GG ,AMP(5   ))             
      CALL JIOXXX(W(1,5   ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,13  ))     
      CALL IOVXXX(W(1,6   ),W(1,2   ),W(1,13  ),GG ,AMP(6   ))             
      CALL JIOXXX(W(1,4   ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,14  ))     
      CALL IOVXXX(W(1,11  ),W(1,3   ),W(1,14  ),GG ,AMP(7   ))             
      CALL VVVXXX(W(1,1   ),W(1,14  ),W(1,13  ),G ,AMP(8   ))              
      CALL IOVXXX(W(1,5   ),W(1,8   ),W(1,14  ),GG ,AMP(9   ))             
      CALL JIOXXX(W(1,4   ),W(1,12  ),GG ,ZERO    ,ZERO    ,W(1,15  ))     
      CALL IOVXXX(W(1,5   ),W(1,3   ),W(1,15  ),GG ,AMP(10  ))             
      do i=1,ngraphs
         saveamp(i,hell)=amp(i)
      enddo
      elseif (calculatedBorn) then
      do i=1,ngraphs
         amp(i)=saveamp(i,hell)
      enddo
      endif
      JAMP(   1) = +AMP(   1)-AMP(   4)
      JAMP(   2) = +AMP(   2)+AMP(   4)
      JAMP(   3) = +AMP(   3)
      JAMP(   4) = +AMP(   5)
      JAMP(   5) = -AMP(   6)-AMP(   8)
      JAMP(   6) = -AMP(   7)
      JAMP(   7) = +AMP(   8)-AMP(  10)
      JAMP(   8) = -AMP(   9)
      B_SF_250_009 = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP = (0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          B_SF_250_009 =B_SF_250_009+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
      END
       
       
