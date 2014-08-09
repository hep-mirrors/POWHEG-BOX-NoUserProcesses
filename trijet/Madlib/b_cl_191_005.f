      SUBROUTINE SB_CL_191_005(P1,ANS)
C  
C Generated by MadGraph II                                              
C MadGraph StandAlone Version
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL-1)
C  
C FOR PROCESS : b d -> d b g g  
C  
C BORN AMPLITUDE IS b d -> d b g  
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
      PARAMETER (NGRAPHS=   5)
C  
C ARGUMENTS 
C  
      REAL*8 P1(0:3,NEXTERNAL-1)
      COMPLEX*16 ANS(NCROSS*2)
C  
C LOCAL VARIABLES 
C  
      REAL*8 P(0:3,NEXTERNAL-1)
      INTEGER NHEL(NEXTERNAL-1,NCOMB),NTRY
      COMPLEX*16 T,T1
      COMPLEX*16 B_CL_191_005
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
      common/to_amps_191/  amp2,       jamp2

      integer j,jj
      integer max_bhel
      parameter ( max_bhel =          32 )
      double complex saveamp(ngraphs,max_bhel)
      common/to_saveamp_191/saveamp
      double precision savemom(nexternal-1,2)
      common/to_savemom_191/savemom

      INTEGER NCOLOR
      DATA NCOLOR   /   4/          
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
      DATA (IDEN(IHEL),IHEL=  1,  1) /  36/
      logical calculatedBorn
      integer skip
      common/cBorn/calculatedBorn,skip
C ----------
C BEGIN CODE
C ----------
      NTRY=NTRY+1
      skip=1
      do while(nhel(5,skip).ne.1)
         skip=skip+1
      enddo
      skip=skip-1
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
      ANS(IPROC+1) = 0D0
          DO IHEL=1,NCOMB
             IF ((GOODHEL(IHEL,IPROC) .OR. NTRY .LT. 2).and.
     &       nhel(5,ihel).eq.-1) THEN
                 T=B_CL_191_005(P1,NHEL(1,IHEL),IHEL,JC(1),T1)            
               ANS(IPROC)=ANS(IPROC)+T
               ANS(IPROC+1)=ANS(IPROC+1)+T1
               IF ( (T .NE. (0D0,0D0) .OR.T1 .NE. (0d0,0D0)) .AND.
     &                 .NOT. GOODHEL(IHEL,IPROC)) THEN
                   GOODHEL(IHEL,IPROC)=.TRUE.
                   NGOOD = NGOOD +1
                   IGOOD(NGOOD) = IHEL
               ENDIF
             ENDIF
          ENDDO
      ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC))
      ANS(IPROC+1)=ANS(IPROC+1)/DBLE(IDEN(IPROC))
      ENDDO
      calculatedBorn=.true.
      END
       
       
      COMPLEX*16 FUNCTION B_CL_191_005(P,NHEL,HELL,IC,BORNTILDE)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL-1)
C  
C FOR PROCESS : b d -> d b g g  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER (NGRAPHS=   5,NEIGEN=  4) 
      include "nexternal.inc"
      INTEGER    NWAVEFUNCS     , NCOLOR
      PARAMETER (NWAVEFUNCS=  11, NCOLOR=   4) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 P(0:3,NEXTERNAL-1)
      INTEGER NHEL(NEXTERNAL-1), IC(NEXTERNAL-1), HELL
      COMPLEX*16 BORNTILDE
C  
C LOCAL VARIABLES 
C  
      INTEGER I,J
      COMPLEX*16 ZTEMP
      REAL*8 RTEMP
      REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)
      COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)
      COMPLEX*16 W(18,NWAVEFUNCS)
      integer ihel
      COMPLEX*16 JAMPH(-1:1,NCOLOR)
C  
C GLOBAL VARIABLES
C  
      integer maxamps
      parameter (maxamps=6000)
      Double Precision amp2(maxamps), jamp2(0:maxamps)
      common/to_amps_191/  amp2,       jamp2
      integer max_bhel
      parameter ( max_bhel =          32 )
      double complex saveamp(ngraphs,max_bhel)
      common/to_saveamp_191/saveamp
      logical calculatedBorn
      integer skip
      common/cBorn/calculatedBorn,skip
      include "coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1  )/            3/                                       
      DATA (CF(i,1  ),i=1  ,4  ) /     8,   -2,    7,   -1/                
C               T[ 4, 2]T[ 3, 1, 5]                                        
      DATA Denom(2  )/            3/                                       
      DATA (CF(i,2  ),i=1  ,4  ) /    -2,    8,   -1,    7/                
C               T[ 4, 2, 5]T[ 3, 1]                                        
      DATA Denom(3  )/            3/                                       
      DATA (CF(i,3  ),i=1  ,4  ) /     7,   -1,    8,   -2/                
C               T[ 4, 2]T[ 3, 1, 5]                                        
      DATA Denom(4  )/            3/                                       
      DATA (CF(i,4  ),i=1  ,4  ) /    -1,    7,   -2,    8/                
C               T[ 3, 1]T[ 4, 2, 5]                                        
C ----------
C BEGIN CODE
C ----------
      B_CL_191_005 = (0D0,0D0)  
      BORNTILDE = (0d0,0d0)
      do ihel=-1,1,2
      if (.not. calculatedBorn) then
      CALL IXXXXX(P(0,1   ),BMASS ,NHEL(1   ),+1*IC(1   ),W(1,1   ))       
      CALL IXXXXX(P(0,2   ),ZERO ,NHEL(2   ),+1*IC(2   ),W(1,2   ))        
      CALL OXXXXX(P(0,3   ),ZERO ,NHEL(3   ),+1*IC(3   ),W(1,3   ))        
      CALL OXXXXX(P(0,4   ),BMASS ,NHEL(4   ),+1*IC(4   ),W(1,4   ))       
      CALL VXXXXX(P(0,  5),ZERO,ihel,+1*IC(  5),W(1,  5))
      CALL JIOXXX(W(1,2   ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,6   ))     
      CALL FVOXXX(W(1,4   ),W(1,6   ),GG ,BMASS   ,ZERO    ,W(1,7   ))     
      CALL IOVXXX(W(1,1   ),W(1,7   ),W(1,5   ),GG ,AMP(1   ))             
      CALL JIOXXX(W(1,1   ),W(1,4   ),GG ,ZERO    ,ZERO    ,W(1,8   ))     
      CALL FVIXXX(W(1,2   ),W(1,5   ),GG ,ZERO    ,ZERO    ,W(1,9   ))     
      CALL IOVXXX(W(1,9   ),W(1,3   ),W(1,8   ),GG ,AMP(2   ))             
      CALL FVOXXX(W(1,3   ),W(1,5   ),GG ,ZERO    ,ZERO    ,W(1,10  ))     
      CALL IOVXXX(W(1,2   ),W(1,10  ),W(1,8   ),GG ,AMP(3   ))             
      CALL FVIXXX(W(1,1   ),W(1,6   ),GG ,BMASS   ,ZERO    ,W(1,11  ))     
      CALL IOVXXX(W(1,11  ),W(1,4   ),W(1,5   ),GG ,AMP(4   ))             
      CALL VVVXXX(W(1,5   ),W(1,6   ),W(1,8   ),G ,AMP(5   ))              
      do i=1,ngraphs
         if(ihel.eq.-1)then
            saveamp(i,hell)=amp(i)
         elseif(ihel.eq.1)then
            saveamp(i,hell+skip)=amp(i)
         else
            write(*,*) "ERROR #1 in born.f"
            stop
         endif
      enddo
      elseif (calculatedBorn) then
      do i=1,ngraphs
         if(ihel.eq.-1)then
            amp(i)=saveamp(i,hell)
         elseif(ihel.eq.1)then
            amp(i)=saveamp(i,hell+skip)
         else
            write(*,*) "ERROR #1 in born.f"
            stop
         endif
      enddo
      endif
      JAMP(   1) = +AMP(   1)
      JAMP(   2) = +AMP(   2)+AMP(   5)
      JAMP(   3) = +AMP(   3)-AMP(   5)
      JAMP(   4) = +AMP(   4)
      DO I = 1, NCOLOR
          ZTEMP = (0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          RTEMP=DBLE(ZTEMP*DCONJG(JAMP(I))/DENOM(I))
          if (ihel.eq.-1) then
             B_CL_191_005=B_CL_191_005 +RTEMP
          elseif (ihel.eq.1) then
             B_CL_191_005=B_CL_191_005 +(0d0,1d0)*RTEMP
          endif
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
          Jamph(ihel,i)=Jamp(i)
      Enddo
       
      Enddo
      DO I = 1, NCOLOR
          ZTEMP = (0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMPH(1,J)
          ENDDO
          BORNTILDE = BORNTILDE + ZTEMP*DCONJG(JAMPH(-1,I))/DENOM(I)
      ENDDO
      END
       
       
