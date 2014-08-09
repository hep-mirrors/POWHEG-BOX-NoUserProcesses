      SUBROUTINE SB_SF_276_014(P1,ANS)
C  
C Generated by MadGraph II                                              
C MadGraph StandAlone Version
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL-1)
C  
C FOR PROCESS : g g -> g g g g  
C  
C BORN AMPLITUDE IS g g -> g g g  
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
      PARAMETER (NGRAPHS=  45)
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
      REAL*8 B_SF_276_014
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
      common/to_amps_276/  amp2,       jamp2

      integer j,jj
      integer max_bhel
      parameter ( max_bhel =          32 )
      double complex saveamp(ngraphs,max_bhel)
      common/to_saveamp_276/saveamp
      double precision savemom(nexternal-1,2)
      common/to_savemom_276/savemom

      INTEGER NCOLOR
      DATA NCOLOR   /  24/          
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
      DATA (IDEN(IHEL),IHEL=  1,  1) /1536/
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
                 T=B_SF_276_014(P1,NHEL(1,IHEL),IHEL,JC(1))              
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
       
       
      REAL*8 FUNCTION B_SF_276_014(P,NHEL,HELL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL-1)
C  
C FOR PROCESS : g g -> g g g g  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER (NGRAPHS=  45,NEIGEN= 24) 
      include "nexternal.inc"
      INTEGER    NWAVEFUNCS     , NCOLOR
      PARAMETER (NWAVEFUNCS=  41, NCOLOR=  24) 
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
      common/to_amps_276/  amp2,       jamp2
      integer max_bhel
      parameter ( max_bhel =          32 )
      double complex saveamp(ngraphs,max_bhel)
      common/to_saveamp_276/saveamp
      logical calculatedBorn
      integer skip
      common/cBorn/calculatedBorn,skip
      include "coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1  )/           72/                                       
      DATA (CF(i,1  ),i=1  ,6  ) /    56,  -16,  -16,   56,  -16,   11/    
      DATA (CF(i,1  ),i=7  ,12 ) /    56,  -16,  -70,  128,   -7,    2/    
      DATA (CF(i,1  ),i=13 ,18 ) /   -88,   65,  -25,   65,   65,  -70/    
      DATA (CF(i,1  ),i=19 ,24 ) /    65,  -25,    2,   -7,   -7,    2/    
C               F[1, 5, 2, 3, 4]                                           
      DATA Denom(2  )/           72/                                       
      DATA (CF(i,2  ),i=1  ,6  ) /   -16,   56,  -70,   65,  -25,   65/    
      DATA (CF(i,2  ),i=7  ,12 ) /   -16,   56,  -16,  -25,   65,  -70/    
      DATA (CF(i,2  ),i=13 ,18 ) /    65,   11,  -16,   56,  -88,    2/    
      DATA (CF(i,2  ),i=19 ,24 ) /    -7,  128,   -7,    2,    2,   -7/    
C               F[1, 5, 3, 2, 4]                                           
      DATA Denom(3  )/           72/                                       
      DATA (CF(i,3  ),i=1  ,6  ) /   -16,  -70, -511,   65,  -70,   56/    
      DATA (CF(i,3  ),i=7  ,12 ) /    65,   11, -124,   -7,   20,  -70/    
      DATA (CF(i,3  ),i=13 ,18 ) /     2,   74,    2,   -7,   -7, -124/    
      DATA (CF(i,3  ),i=19 ,24 ) /    74,    2,  -70,   20,    2,  -52/    
C               F[1, 5, 4, 2, 3]                                           
      DATA Denom(4  )/           72/                                       
      DATA (CF(i,4  ),i=1  ,6  ) /    56,   65,   65,   56,  -16,  -70/    
      DATA (CF(i,4  ),i=7  ,12 ) /   -25,  -16,   -7,   65,   -7,    2/    
      DATA (CF(i,4  ),i=13 ,18 ) /   -25,    2,  -88,  128,  -16,   11/    
      DATA (CF(i,4  ),i=19 ,24 ) /   -16,   56,   65,  -70,   -7,    2/    
C               F[1, 5, 4, 3, 2]                                           
      DATA Denom(5  )/           72/                                       
      DATA (CF(i,5  ),i=1  ,6  ) /   -16,  -25,  -70,  -16,   56,   65/    
      DATA (CF(i,5  ),i=7  ,12 ) /    65,   56,    2,  -25,    2,   -7/    
      DATA (CF(i,5  ),i=13 ,18 ) /    65,   -7,  128,  -88,   56,  -16/    
      DATA (CF(i,5  ),i=19 ,24 ) /    11,  -16,  -70,   65,    2,   -7/    
C               F[1, 2, 3, 4, 5]                                           
      DATA Denom(6  )/           72/                                       
      DATA (CF(i,6  ),i=1  ,6  ) /    11,   65,   56,  -70,   65, -511/    
      DATA (CF(i,6  ),i=7  ,12 ) /   -70,  -16,   74,    2,  -70,   20/    
      DATA (CF(i,6  ),i=13 ,18 ) /    -7, -124,   -7,    2,    2,   74/    
      DATA (CF(i,6  ),i=19 ,24 ) /  -124,   -7,   20,  -70,  -52,    2/    
C               F[1, 3, 2, 4, 5]                                           
      DATA Denom(7  )/           72/                                       
      DATA (CF(i,7  ),i=1  ,6  ) /    56,  -16,   65,  -25,   65,  -70/    
      DATA (CF(i,7  ),i=7  ,12 ) /    56,  -16,   11,   65,  -70,   65/    
      DATA (CF(i,7  ),i=13 ,18 ) /   -25,  -16,   56,  -16,  128,   -7/    
      DATA (CF(i,7  ),i=19 ,24 ) /     2,  -88,    2,   -7,   -7,    2/    
C               F[1, 4, 2, 3, 5]                                           
      DATA Denom(8  )/           72/                                       
      DATA (CF(i,8  ),i=1  ,6  ) /   -16,   56,   11,  -16,   56,  -16/    
      DATA (CF(i,8  ),i=7  ,12 ) /   -16,   56,   65,  -88,    2,   -7/    
      DATA (CF(i,8  ),i=13 ,18 ) /   128,  -70,   65,  -25,  -25,   65/    
      DATA (CF(i,8  ),i=19 ,24 ) /   -70,   65,   -7,    2,    2,   -7/    
C               F[1, 4, 3, 2, 5]                                           
      DATA Denom(9  )/           72/                                       
      DATA (CF(i,9  ),i=1  ,6  ) /   -70,  -16, -124,   -7,    2,   74/    
      DATA (CF(i,9  ),i=7  ,12 ) /    11,   65, -511,    2,   74, -124/    
      DATA (CF(i,9  ),i=13 ,18 ) /    -7,   56,  -70,   65,    2,  -70/    
      DATA (CF(i,9  ),i=19 ,24 ) /    20,   -7,    2,  -52,  -70,   20/    
C               F[1, 3, 2, 5, 4]                                           
      DATA Denom(10 )/           72/                                       
      DATA (CF(i,10 ),i=1  ,6  ) /   128,  -25,   -7,   65,  -25,    2/    
      DATA (CF(i,10 ),i=7  ,12 ) /    65,  -88,    2,   56,   65,  -70/    
      DATA (CF(i,10 ),i=13 ,18 ) /   -16,   -7,  -16,   56,   56,    2/    
      DATA (CF(i,10 ),i=19 ,24 ) /    -7,  -16,  -70,   65,  -16,   11/    
C               F[1, 4, 3, 5, 2]                                           
      DATA Denom(11 )/           72/                                       
      DATA (CF(i,11 ),i=1  ,6  ) /    -7,   65,   20,   -7,    2,  -70/    
      DATA (CF(i,11 ),i=7  ,12 ) /   -70,    2,   74,   65, -511,   56/    
      DATA (CF(i,11 ),i=13 ,18 ) /   -70, -124,   11,  -16,    2,  -52/    
      DATA (CF(i,11 ),i=19 ,24 ) /     2,   -7,   20,  -70,   74, -124/    
C               F[1, 3, 5, 2, 4]                                           
      DATA Denom(12 )/           72/                                       
      DATA (CF(i,12 ),i=1  ,6  ) /     2,  -70,  -70,    2,   -7,   20/    
      DATA (CF(i,12 ),i=7  ,12 ) /    65,   -7, -124,  -70,   56, -511/    
      DATA (CF(i,12 ),i=13 ,18 ) /    65,   74,  -16,   11,   -7,    2/    
      DATA (CF(i,12 ),i=19 ,24 ) /   -52,    2,  -70,   20, -124,   74/    
C               F[1, 4, 2, 5, 3]                                           
      DATA Denom(13 )/           72/                                       
      DATA (CF(i,13 ),i=1  ,6  ) /   -88,   65,    2,  -25,   65,   -7/    
      DATA (CF(i,13 ),i=7  ,12 ) /   -25,  128,   -7,  -16,  -70,   65/    
      DATA (CF(i,13 ),i=13 ,18 ) /    56,    2,   56,  -16,  -16,   -7/    
      DATA (CF(i,13 ),i=19 ,24 ) /     2,   56,   65,  -70,   11,  -16/    
C               F[1, 2, 5, 3, 4]                                           
      DATA Denom(14 )/           72/                                       
      DATA (CF(i,14 ),i=1  ,6  ) /    65,   11,   74,    2,   -7, -124/    
      DATA (CF(i,14 ),i=7  ,12 ) /   -16,  -70,   56,   -7, -124,   74/    
      DATA (CF(i,14 ),i=13 ,18 ) /     2, -511,   65,  -70,   -7,   20/    
      DATA (CF(i,14 ),i=19 ,24 ) /   -70,    2,  -52,    2,   20,  -70/    
C               F[1, 4, 5, 2, 3]                                           
      DATA Denom(15 )/           72/                                       
      DATA (CF(i,15 ),i=1  ,6  ) /   -25,  -16,    2,  -88,  128,   -7/    
      DATA (CF(i,15 ),i=7  ,12 ) /    56,   65,  -70,  -16,   11,  -16/    
      DATA (CF(i,15 ),i=13 ,18 ) /    56,   65,   56,  -16,   65,   -7/    
      DATA (CF(i,15 ),i=19 ,24 ) /     2,  -25,    2,   -7,  -70,   65/    
C               F[1, 4, 5, 3, 2]                                           
      DATA Denom(16 )/           72/                                       
      DATA (CF(i,16 ),i=1  ,6  ) /    65,   56,   -7,  128,  -88,    2/    
      DATA (CF(i,16 ),i=7  ,12 ) /   -16,  -25,   65,   56,  -16,   11/    
      DATA (CF(i,16 ),i=13 ,18 ) /   -16,  -70,  -16,   56,  -25,    2/    
      DATA (CF(i,16 ),i=19 ,24 ) /    -7,   65,   -7,    2,   65,  -70/    
C               F[1, 2, 3, 5, 4]                                           
      DATA Denom(17 )/           72/                                       
      DATA (CF(i,17 ),i=1  ,6  ) /    65,  -88,   -7,  -16,   56,    2/    
      DATA (CF(i,17 ),i=7  ,12 ) /   128,  -25,    2,   56,    2,   -7/    
      DATA (CF(i,17 ),i=13 ,18 ) /   -16,   -7,   65,  -25,   56,   65/    
      DATA (CF(i,17 ),i=19 ,24 ) /   -70,  -16,   11,  -16,   65,  -70/    
C               F[1, 5, 3, 4, 2]                                           
      DATA Denom(18 )/           72/                                       
      DATA (CF(i,18 ),i=1  ,6  ) /   -70,    2, -124,   11,  -16,   74/    
      DATA (CF(i,18 ),i=7  ,12 ) /    -7,   65,  -70,    2,  -52,    2/    
      DATA (CF(i,18 ),i=13 ,18 ) /    -7,   20,   -7,    2,   65, -511/    
      DATA (CF(i,18 ),i=19 ,24 ) /    56,  -70, -124,   74,  -70,   20/    
C               F[1, 3, 4, 2, 5]                                           
      DATA Denom(19 )/           72/                                       
      DATA (CF(i,19 ),i=1  ,6  ) /    65,   -7,   74,  -16,   11, -124/    
      DATA (CF(i,19 ),i=7  ,12 ) /     2,  -70,   20,   -7,    2,  -52/    
      DATA (CF(i,19 ),i=13 ,18 ) /     2,  -70,    2,   -7,  -70,   56/    
      DATA (CF(i,19 ),i=19 ,24 ) /  -511,   65,   74, -124,   20,  -70/    
C               F[1, 5, 2, 4, 3]                                           
      DATA Denom(20 )/           72/                                       
      DATA (CF(i,20 ),i=1  ,6  ) /   -25,  128,    2,   56,  -16,   -7/    
      DATA (CF(i,20 ),i=7  ,12 ) /   -88,   65,   -7,  -16,   -7,    2/    
      DATA (CF(i,20 ),i=13 ,18 ) /    56,    2,  -25,   65,  -16,  -70/    
      DATA (CF(i,20 ),i=19 ,24 ) /    65,   56,  -16,   11,  -70,   65/    
C               F[1, 2, 4, 3, 5]                                           
      DATA Denom(21 )/           72/                                       
      DATA (CF(i,21 ),i=1  ,6  ) /     2,   -7,  -70,   65,  -70,   20/    
      DATA (CF(i,21 ),i=7  ,12 ) /     2,   -7,    2,  -70,   20,  -70/    
      DATA (CF(i,21 ),i=13 ,18 ) /    65,  -52,    2,   -7,   11, -124/    
      DATA (CF(i,21 ),i=19 ,24 ) /    74,  -16, -511,   56, -124,   74/    
C               F[1, 2, 5, 4, 3]                                           
      DATA Denom(22 )/           72/                                       
      DATA (CF(i,22 ),i=1  ,6  ) /    -7,    2,   20,  -70,   65,  -70/    
      DATA (CF(i,22 ),i=7  ,12 ) /    -7,    2,  -52,   65,  -70,   20/    
      DATA (CF(i,22 ),i=13 ,18 ) /   -70,    2,   -7,    2,  -16,   74/    
      DATA (CF(i,22 ),i=19 ,24 ) /  -124,   11,   56, -511,   74, -124/    
C               F[1, 3, 4, 5, 2]                                           
      DATA Denom(23 )/           72/                                       
      DATA (CF(i,23 ),i=1  ,6  ) /    -7,    2,    2,   -7,    2,  -52/    
      DATA (CF(i,23 ),i=7  ,12 ) /    -7,    2,  -70,  -16,   74, -124/    
      DATA (CF(i,23 ),i=13 ,18 ) /    11,   20,  -70,   65,   65,  -70/    
      DATA (CF(i,23 ),i=19 ,24 ) /    20,  -70, -124,   74, -511,   56/    
C               F[1, 3, 5, 4, 2]                                           
      DATA Denom(24 )/           72/                                       
      DATA (CF(i,24 ),i=1  ,6  ) /     2,   -7,  -52,    2,   -7,    2/    
      DATA (CF(i,24 ),i=7  ,12 ) /     2,   -7,   20,   11, -124,   74/    
      DATA (CF(i,24 ),i=13 ,18 ) /   -16,  -70,   65,  -70,  -70,   20/    
      DATA (CF(i,24 ),i=19 ,24 ) /   -70,   65,   74, -124,   56, -511/    
C               F[1, 2, 4, 5, 3]                                           
C ----------
C BEGIN CODE
C ----------
      if (.not. calculatedBorn) then
      CALL VXXXXX(P(0,1   ),ZERO ,NHEL(1   ),-1*IC(1   ),W(1,1   ))        
      CALL VXXXXX(P(0,2   ),ZERO ,NHEL(2   ),-1*IC(2   ),W(1,2   ))        
      CALL VXXXXX(P(0,3   ),ZERO ,NHEL(3   ),+1*IC(3   ),W(1,3   ))        
      CALL VXXXXX(P(0,4   ),ZERO ,NHEL(4   ),+1*IC(4   ),W(1,4   ))        
      CALL VXXXXX(P(0,5   ),ZERO ,NHEL(5   ),+1*IC(5   ),W(1,5   ))        
      CALL JVVXXX(W(1,3   ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,6   ))      
      CALL JVVXXX(W(1,4   ),W(1,6   ),G ,ZERO    ,ZERO    ,W(1,7   ))      
      CALL VVVXXX(W(1,5   ),W(1,1   ),W(1,7   ),G ,AMP(1   ))              
      CALL JVVXXX(W(1,4   ),W(1,1   ),G ,ZERO    ,ZERO    ,W(1,8   ))      
      CALL JVVXXX(W(1,5   ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,9   ))      
      CALL VVVXXX(W(1,8   ),W(1,9   ),W(1,3   ),G ,AMP(2   ))              
      CALL JVVXXX(W(1,5   ),W(1,3   ),G ,ZERO    ,ZERO    ,W(1,10  ))      
      CALL VVVXXX(W(1,8   ),W(1,2   ),W(1,10  ),G ,AMP(3   ))              
      CALL JVVXXX(W(1,6   ),W(1,1   ),G ,ZERO    ,ZERO    ,W(1,11  ))      
      CALL VVVXXX(W(1,5   ),W(1,4   ),W(1,11  ),G ,AMP(4   ))              
      CALL JGGGXX(W(1,5   ),W(1,2   ),W(1,3   ),G ,W(1,12  ))              
      CALL VVVXXX(W(1,4   ),W(1,1   ),W(1,12  ),G ,AMP(5   ))              
      CALL JGGGXX(W(1,3   ),W(1,5   ),W(1,2   ),G ,W(1,13  ))              
      CALL VVVXXX(W(1,4   ),W(1,1   ),W(1,13  ),G ,AMP(6   ))              
      CALL JGGGXX(W(1,2   ),W(1,3   ),W(1,5   ),G ,W(1,14  ))              
      CALL VVVXXX(W(1,4   ),W(1,1   ),W(1,14  ),G ,AMP(7   ))              
      CALL GGGGXX(W(1,4   ),W(1,1   ),W(1,6   ),W(1,5   ),G ,AMP(8   ))    
      CALL GGGGXX(W(1,6   ),W(1,4   ),W(1,1   ),W(1,5   ),G ,AMP(9   ))    
      CALL GGGGXX(W(1,1   ),W(1,6   ),W(1,4   ),W(1,5   ),G ,AMP(10  ))    
      CALL VVVXXX(W(1,5   ),W(1,6   ),W(1,8   ),G ,AMP(11  ))              
      CALL JVVXXX(W(1,4   ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,15  ))      
      CALL JVVXXX(W(1,5   ),W(1,1   ),G ,ZERO    ,ZERO    ,W(1,16  ))      
      CALL VVVXXX(W(1,16  ),W(1,15  ),W(1,3   ),G ,AMP(12  ))              
      CALL JVVXXX(W(1,1   ),W(1,3   ),G ,ZERO    ,ZERO    ,W(1,17  ))      
      CALL JVVXXX(W(1,4   ),W(1,17  ),G ,ZERO    ,ZERO    ,W(1,18  ))      
      CALL VVVXXX(W(1,5   ),W(1,2   ),W(1,18  ),G ,AMP(13  ))              
      CALL VVVXXX(W(1,1   ),W(1,15  ),W(1,10  ),G ,AMP(14  ))              
      CALL JVVXXX(W(1,17  ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,19  ))      
      CALL VVVXXX(W(1,5   ),W(1,4   ),W(1,19  ),G ,AMP(15  ))              
      CALL JGGGXX(W(1,1   ),W(1,5   ),W(1,3   ),G ,W(1,20  ))              
      CALL VVVXXX(W(1,4   ),W(1,2   ),W(1,20  ),G ,AMP(16  ))              
      CALL JGGGXX(W(1,3   ),W(1,1   ),W(1,5   ),G ,W(1,21  ))              
      CALL VVVXXX(W(1,4   ),W(1,2   ),W(1,21  ),G ,AMP(17  ))              
      CALL JGGGXX(W(1,5   ),W(1,3   ),W(1,1   ),G ,W(1,22  ))              
      CALL VVVXXX(W(1,4   ),W(1,2   ),W(1,22  ),G ,AMP(18  ))              
      CALL GGGGXX(W(1,4   ),W(1,2   ),W(1,17  ),W(1,5   ),G ,AMP(19  ))    
      CALL GGGGXX(W(1,17  ),W(1,4   ),W(1,2   ),W(1,5   ),G ,AMP(20  ))    
      CALL GGGGXX(W(1,2   ),W(1,17  ),W(1,4   ),W(1,5   ),G ,AMP(21  ))    
      CALL VVVXXX(W(1,5   ),W(1,17  ),W(1,15  ),G ,AMP(22  ))              
      CALL JVVXXX(W(1,4   ),W(1,3   ),G ,ZERO    ,ZERO    ,W(1,23  ))      
      CALL VVVXXX(W(1,16  ),W(1,2   ),W(1,23  ),G ,AMP(23  ))              
      CALL VVVXXX(W(1,1   ),W(1,9   ),W(1,23  ),G ,AMP(24  ))              
      CALL JVVXXX(W(1,1   ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,24  ))      
      CALL JVVXXX(W(1,4   ),W(1,24  ),G ,ZERO    ,ZERO    ,W(1,25  ))      
      CALL VVVXXX(W(1,5   ),W(1,3   ),W(1,25  ),G ,AMP(25  ))              
      CALL JVVXXX(W(1,24  ),W(1,3   ),G ,ZERO    ,ZERO    ,W(1,26  ))      
      CALL VVVXXX(W(1,5   ),W(1,4   ),W(1,26  ),G ,AMP(26  ))              
      CALL JGGGXX(W(1,1   ),W(1,2   ),W(1,5   ),G ,W(1,27  ))              
      CALL VVVXXX(W(1,4   ),W(1,3   ),W(1,27  ),G ,AMP(27  ))              
      CALL JGGGXX(W(1,5   ),W(1,1   ),W(1,2   ),G ,W(1,28  ))              
      CALL VVVXXX(W(1,4   ),W(1,3   ),W(1,28  ),G ,AMP(28  ))              
      CALL JGGGXX(W(1,2   ),W(1,5   ),W(1,1   ),G ,W(1,29  ))              
      CALL VVVXXX(W(1,4   ),W(1,3   ),W(1,29  ),G ,AMP(29  ))              
      CALL GGGGXX(W(1,4   ),W(1,3   ),W(1,24  ),W(1,5   ),G ,AMP(30  ))    
      CALL GGGGXX(W(1,24  ),W(1,4   ),W(1,3   ),W(1,5   ),G ,AMP(31  ))    
      CALL GGGGXX(W(1,3   ),W(1,24  ),W(1,4   ),W(1,5   ),G ,AMP(32  ))    
      CALL VVVXXX(W(1,5   ),W(1,24  ),W(1,23  ),G ,AMP(33  ))              
      CALL JGGGXX(W(1,4   ),W(1,2   ),W(1,3   ),G ,W(1,30  ))              
      CALL VVVXXX(W(1,5   ),W(1,1   ),W(1,30  ),G ,AMP(34  ))              
      CALL JGGGXX(W(1,3   ),W(1,4   ),W(1,2   ),G ,W(1,31  ))              
      CALL VVVXXX(W(1,5   ),W(1,1   ),W(1,31  ),G ,AMP(35  ))              
      CALL JGGGXX(W(1,2   ),W(1,3   ),W(1,4   ),G ,W(1,32  ))              
      CALL VVVXXX(W(1,5   ),W(1,1   ),W(1,32  ),G ,AMP(36  ))              
      CALL JGGGXX(W(1,1   ),W(1,4   ),W(1,3   ),G ,W(1,33  ))              
      CALL VVVXXX(W(1,5   ),W(1,2   ),W(1,33  ),G ,AMP(37  ))              
      CALL JGGGXX(W(1,3   ),W(1,1   ),W(1,4   ),G ,W(1,34  ))              
      CALL VVVXXX(W(1,5   ),W(1,2   ),W(1,34  ),G ,AMP(38  ))              
      CALL JGGGXX(W(1,4   ),W(1,3   ),W(1,1   ),G ,W(1,35  ))              
      CALL VVVXXX(W(1,5   ),W(1,2   ),W(1,35  ),G ,AMP(39  ))              
      CALL JGGGXX(W(1,1   ),W(1,2   ),W(1,4   ),G ,W(1,36  ))              
      CALL VVVXXX(W(1,5   ),W(1,3   ),W(1,36  ),G ,AMP(40  ))              
      CALL JGGGXX(W(1,4   ),W(1,1   ),W(1,2   ),G ,W(1,37  ))              
      CALL VVVXXX(W(1,5   ),W(1,3   ),W(1,37  ),G ,AMP(41  ))              
      CALL JGGGXX(W(1,2   ),W(1,4   ),W(1,1   ),G ,W(1,38  ))              
      CALL VVVXXX(W(1,5   ),W(1,3   ),W(1,38  ),G ,AMP(42  ))              
      CALL JGGGXX(W(1,1   ),W(1,2   ),W(1,3   ),G ,W(1,39  ))              
      CALL VVVXXX(W(1,5   ),W(1,4   ),W(1,39  ),G ,AMP(43  ))              
      CALL JGGGXX(W(1,3   ),W(1,1   ),W(1,2   ),G ,W(1,40  ))              
      CALL VVVXXX(W(1,5   ),W(1,4   ),W(1,40  ),G ,AMP(44  ))              
      CALL JGGGXX(W(1,2   ),W(1,3   ),W(1,1   ),G ,W(1,41  ))              
      CALL VVVXXX(W(1,5   ),W(1,4   ),W(1,41  ),G ,AMP(45  ))              
      do i=1,ngraphs
         saveamp(i,hell)=amp(i)
      enddo
      elseif (calculatedBorn) then
      do i=1,ngraphs
         amp(i)=saveamp(i,hell)
      enddo
      endif
      JAMP(   1) =      2*( -AMP(   1)+AMP(   2)-AMP(   5)+AMP(   7)
     &             -AMP(   8)+AMP(   9)-AMP(  11)+AMP(  23)-AMP(  24)
     &             -AMP(  28)+AMP(  29)-AMP(  35)+AMP(  36)-AMP(  37)
     &             +AMP(  39))
      JAMP(   2) =      2*( +AMP(   1)-AMP(   3)+AMP(   6)-AMP(   7)
     &             +AMP(   8)-AMP(   9)+AMP(  11)-AMP(  12)+AMP(  14)
     &             +AMP(  16)-AMP(  18)+AMP(  34)-AMP(  36)+AMP(  41)
     &             -AMP(  42))
      JAMP(   3) =      2*( +AMP(   1)-AMP(   4)-AMP(   9)+AMP(  10)
     &             -AMP(  12)+AMP(  15)+AMP(  16)-AMP(  17)+AMP(  19)
     &             -AMP(  21)+AMP(  22)+AMP(  34)-AMP(  36)+AMP(  44)
     &             -AMP(  45))
      JAMP(   4) =      2*( -AMP(   1)+AMP(   4)+AMP(   9)-AMP(  10)
     &             +AMP(  23)+AMP(  26)+AMP(  27)-AMP(  28)+AMP(  30)
     &             -AMP(  32)+AMP(  33)-AMP(  35)+AMP(  36)-AMP(  43)
     &             +AMP(  45))
      JAMP(   5) =      2*( +AMP(   1)-AMP(   4)-AMP(   9)+AMP(  10)
     &             -AMP(  23)-AMP(  26)-AMP(  27)+AMP(  28)-AMP(  30)
     &             +AMP(  32)-AMP(  33)+AMP(  35)-AMP(  36)+AMP(  43)
     &             -AMP(  45))
      JAMP(   6) =      2*( -AMP(   1)+AMP(   4)+AMP(   9)-AMP(  10)
     &             +AMP(  12)-AMP(  15)-AMP(  16)+AMP(  17)-AMP(  19)
     &             +AMP(  21)-AMP(  22)-AMP(  34)+AMP(  36)-AMP(  44)
     &             +AMP(  45))
      JAMP(   7) =      2*( -AMP(   1)+AMP(   3)-AMP(   6)+AMP(   7)
     &             -AMP(   8)+AMP(   9)-AMP(  11)+AMP(  12)-AMP(  14)
     &             -AMP(  16)+AMP(  18)-AMP(  34)+AMP(  36)-AMP(  41)
     &             +AMP(  42))
      JAMP(   8) =      2*( +AMP(   1)-AMP(   2)+AMP(   5)-AMP(   7)
     &             +AMP(   8)-AMP(   9)+AMP(  11)-AMP(  23)+AMP(  24)
     &             +AMP(  28)-AMP(  29)+AMP(  35)-AMP(  36)+AMP(  37)
     &             -AMP(  39))
      JAMP(   9) =      2*( +AMP(   2)-AMP(   4)-AMP(   5)+AMP(   7)
     &             -AMP(   8)+AMP(  10)-AMP(  11)-AMP(  13)+AMP(  15)
     &             +AMP(  20)-AMP(  21)-AMP(  37)+AMP(  38)+AMP(  44)
     &             -AMP(  45))
      JAMP(  10) =      2*( +AMP(   2)-AMP(   3)-AMP(   5)+AMP(   6)
     &             -AMP(  24)-AMP(  25)-AMP(  27)+AMP(  29)-AMP(  30)
     &             +AMP(  31)-AMP(  33)-AMP(  37)+AMP(  39)-AMP(  40)
     &             +AMP(  41))
      JAMP(  11) =      2*( -AMP(   2)+AMP(   3)+AMP(   5)-AMP(   6)
     &             +AMP(  13)-AMP(  14)-AMP(  17)+AMP(  18)+AMP(  19)
     &             -AMP(  20)+AMP(  22)+AMP(  37)-AMP(  38)-AMP(  41)
     &             +AMP(  42))
      JAMP(  12) =      2*( +AMP(   2)-AMP(   3)-AMP(   5)+AMP(   6)
     &             -AMP(  13)+AMP(  14)+AMP(  17)-AMP(  18)-AMP(  19)
     &             +AMP(  20)-AMP(  22)-AMP(  37)+AMP(  38)+AMP(  41)
     &             -AMP(  42))
      JAMP(  13) =      2*( -AMP(   2)+AMP(   3)+AMP(   5)-AMP(   6)
     &             +AMP(  24)+AMP(  25)+AMP(  27)-AMP(  29)+AMP(  30)
     &             -AMP(  31)+AMP(  33)+AMP(  37)-AMP(  39)+AMP(  40)
     &             -AMP(  41))
      JAMP(  14) =      2*( -AMP(   2)+AMP(   4)+AMP(   5)-AMP(   7)
     &             +AMP(   8)-AMP(  10)+AMP(  11)+AMP(  13)-AMP(  15)
     &             -AMP(  20)+AMP(  21)+AMP(  37)-AMP(  38)-AMP(  44)
     &             +AMP(  45))
      JAMP(  15) =      2*( +AMP(   3)-AMP(   4)-AMP(   6)+AMP(   7)
     &             -AMP(   8)+AMP(  10)-AMP(  11)+AMP(  25)-AMP(  26)
     &             -AMP(  31)+AMP(  32)+AMP(  40)-AMP(  41)+AMP(  43)
     &             -AMP(  45))
      JAMP(  16) =      2*( -AMP(   3)+AMP(   4)+AMP(   6)-AMP(   7)
     &             +AMP(   8)-AMP(  10)+AMP(  11)-AMP(  25)+AMP(  26)
     &             +AMP(  31)-AMP(  32)-AMP(  40)+AMP(  41)-AMP(  43)
     &             +AMP(  45))
      JAMP(  17) =      2*( +AMP(  12)-AMP(  14)-AMP(  16)+AMP(  18)
     &             -AMP(  23)-AMP(  25)-AMP(  27)+AMP(  28)-AMP(  30)
     &             +AMP(  31)-AMP(  33)-AMP(  34)+AMP(  35)-AMP(  40)
     &             +AMP(  42))
      JAMP(  18) =      2*( -AMP(  12)+AMP(  13)+AMP(  16)-AMP(  17)
     &             +AMP(  19)-AMP(  20)+AMP(  22)+AMP(  23)-AMP(  24)
     &             -AMP(  28)+AMP(  29)+AMP(  34)-AMP(  35)-AMP(  38)
     &             +AMP(  39))
      JAMP(  19) =      2*( +AMP(  12)-AMP(  13)-AMP(  16)+AMP(  17)
     &             -AMP(  19)+AMP(  20)-AMP(  22)-AMP(  23)+AMP(  24)
     &             +AMP(  28)-AMP(  29)-AMP(  34)+AMP(  35)+AMP(  38)
     &             -AMP(  39))
      JAMP(  20) =      2*( -AMP(  12)+AMP(  14)+AMP(  16)-AMP(  18)
     &             +AMP(  23)+AMP(  25)+AMP(  27)-AMP(  28)+AMP(  30)
     &             -AMP(  31)+AMP(  33)+AMP(  34)-AMP(  35)+AMP(  40)
     &             -AMP(  42))
      JAMP(  21) =      2*( +AMP(  13)-AMP(  15)-AMP(  20)+AMP(  21)
     &             -AMP(  24)-AMP(  26)-AMP(  27)+AMP(  29)-AMP(  30)
     &             +AMP(  32)-AMP(  33)-AMP(  38)+AMP(  39)+AMP(  43)
     &             -AMP(  44))
      JAMP(  22) =      2*( -AMP(  13)+AMP(  15)+AMP(  20)-AMP(  21)
     &             +AMP(  24)+AMP(  26)+AMP(  27)-AMP(  29)+AMP(  30)
     &             -AMP(  32)+AMP(  33)+AMP(  38)-AMP(  39)-AMP(  43)
     &             +AMP(  44))
      JAMP(  23) =      2*( +AMP(  14)-AMP(  15)+AMP(  17)-AMP(  18)
     &             -AMP(  19)+AMP(  21)-AMP(  22)+AMP(  25)-AMP(  26)
     &             -AMP(  31)+AMP(  32)+AMP(  40)-AMP(  42)+AMP(  43)
     &             -AMP(  44))
      JAMP(  24) =      2*( -AMP(  14)+AMP(  15)-AMP(  17)+AMP(  18)
     &             +AMP(  19)-AMP(  21)+AMP(  22)-AMP(  25)+AMP(  26)
     &             +AMP(  31)-AMP(  32)-AMP(  40)+AMP(  42)-AMP(  43)
     &             +AMP(  44))
      B_SF_276_014 = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP = (0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          B_SF_276_014 =B_SF_276_014+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
      END
       
       
