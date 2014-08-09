      SUBROUTINE SB_CL_276_003(P1,ANS)
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
      REAL*8 P1(0:3,NEXTERNAL-1)
      COMPLEX*16 ANS(NCROSS*2)
C  
C LOCAL VARIABLES 
C  
      REAL*8 P(0:3,NEXTERNAL-1)
      INTEGER NHEL(NEXTERNAL-1,NCOMB),NTRY
      COMPLEX*16 T,T1
      COMPLEX*16 B_CL_276_003
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
      skip=1
      do while(nhel(3,skip).ne.1)
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
     &       nhel(3,ihel).eq.-1) THEN
                 T=B_CL_276_003(P1,NHEL(1,IHEL),IHEL,JC(1),T1)            
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
       
       
      COMPLEX*16 FUNCTION B_CL_276_003(P,NHEL,HELL,IC,BORNTILDE)
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
      DATA Denom(1  )/          108/                                       
      DATA (CF(i,1  ),i=1  ,6  ) /   455,  -58,   14,   68,  -58,   -4/    
      DATA (CF(i,1  ),i=7  ,12 ) /    68,  -40,   68,   68,   14,   -4/    
      DATA (CF(i,1  ),i=13 ,18 ) /   -58,  -58,   -4,   14,   14,   68/    
      DATA (CF(i,1  ),i=19 ,24 ) /   -58,   -4,   -4,   14,    5,    5/    
C               F[1, 5, 2, 3, 4]                                           
      DATA Denom(2  )/          108/                                       
      DATA (CF(i,2  ),i=1  ,6  ) /   -58,  455,   68,   14,   -4,  -58/    
      DATA (CF(i,2  ),i=7  ,12 ) /   -40,   68,   14,   -4,  -58,   68/    
      DATA (CF(i,2  ),i=13 ,18 ) /    14,   -4,  -58,   68,  -58,   -4/    
      DATA (CF(i,2  ),i=19 ,24 ) /    14,   68,    5,    5,   -4,   14/    
C               F[1, 5, 3, 2, 4]                                           
      DATA Denom(3  )/          108/                                       
      DATA (CF(i,3  ),i=1  ,6  ) /    14,   68,  455,  -58,   68,  -40/    
      DATA (CF(i,3  ),i=7  ,12 ) /   -58,   -4,   68,    5,   -4,   14/    
      DATA (CF(i,3  ),i=13 ,18 ) /     5,  -58,   -4,   14,   14,   68/    
      DATA (CF(i,3  ),i=19 ,24 ) /   -58,   -4,   14,   -4,  -58,   68/    
C               F[1, 5, 4, 2, 3]                                           
      DATA Denom(4  )/          108/                                       
      DATA (CF(i,4  ),i=1  ,6  ) /    68,   14,  -58,  455,  -40,   68/    
      DATA (CF(i,4  ),i=7  ,12 ) /    -4,  -58,   14,   14,    5,    5/    
      DATA (CF(i,4  ),i=13 ,18 ) /    -4,   -4,  -58,   68,  -58,   -4/    
      DATA (CF(i,4  ),i=19 ,24 ) /    14,   68,  -58,   68,   14,   -4/    
C               F[1, 5, 4, 3, 2]                                           
      DATA Denom(5  )/          108/                                       
      DATA (CF(i,5  ),i=1  ,6  ) /   -58,   -4,   68,  -40,  455,  -58/    
      DATA (CF(i,5  ),i=7  ,12 ) /    14,   68,   -4,   -4,    5,    5/    
      DATA (CF(i,5  ),i=13 ,18 ) /    14,   14,   68,  -58,   68,   14/    
      DATA (CF(i,5  ),i=19 ,24 ) /    -4,  -58,   68,  -58,   -4,   14/    
C               F[1, 2, 3, 4, 5]                                           
      DATA Denom(6  )/          108/                                       
      DATA (CF(i,6  ),i=1  ,6  ) /    -4,  -58,  -40,   68,  -58,  455/    
      DATA (CF(i,6  ),i=7  ,12 ) /    68,   14,  -58,    5,   14,   -4/    
      DATA (CF(i,6  ),i=13 ,18 ) /     5,   68,   14,   -4,   -4,  -58/    
      DATA (CF(i,6  ),i=19 ,24 ) /    68,   14,   -4,   14,   68,  -58/    
C               F[1, 3, 2, 4, 5]                                           
      DATA Denom(7  )/          108/                                       
      DATA (CF(i,7  ),i=1  ,6  ) /    68,  -40,  -58,   -4,   14,   68/    
      DATA (CF(i,7  ),i=7  ,12 ) /   455,  -58,   -4,   14,   68,  -58/    
      DATA (CF(i,7  ),i=13 ,18 ) /    -4,   14,   68,  -58,   68,   14/    
      DATA (CF(i,7  ),i=19 ,24 ) /    -4,  -58,    5,    5,   14,   -4/    
C               F[1, 4, 2, 3, 5]                                           
      DATA Denom(8  )/          108/                                       
      DATA (CF(i,8  ),i=1  ,6  ) /   -40,   68,   -4,  -58,   68,   14/    
      DATA (CF(i,8  ),i=7  ,12 ) /   -58,  455,  -58,  -58,   -4,   14/    
      DATA (CF(i,8  ),i=13 ,18 ) /    68,   68,   14,   -4,   -4,  -58/    
      DATA (CF(i,8  ),i=19 ,24 ) /    68,   14,   14,   -4,    5,    5/    
C               F[1, 4, 3, 2, 5]                                           
      DATA Denom(9  )/          108/                                       
      DATA (CF(i,9  ),i=1  ,6  ) /    68,   14,   68,   14,   -4,  -58/    
      DATA (CF(i,9  ),i=7  ,12 ) /    -4,  -58,  455,   -4,  -58,   68/    
      DATA (CF(i,9  ),i=13 ,18 ) /    14,  -40,   68,  -58,    5,   14/    
      DATA (CF(i,9  ),i=19 ,24 ) /    -4,    5,  -58,   68,   14,   -4/    
C               F[1, 3, 2, 5, 4]                                           
      DATA Denom(10 )/          108/                                       
      DATA (CF(i,10 ),i=1  ,6  ) /    68,   -4,    5,   14,   -4,    5/    
      DATA (CF(i,10 ),i=7  ,12 ) /    14,  -58,   -4,  455,  -58,   68/    
      DATA (CF(i,10 ),i=13 ,18 ) /   -40,   14,  -58,   68,   68,   -4/    
      DATA (CF(i,10 ),i=19 ,24 ) /    14,  -58,   68,  -58,   14,   -4/    
C               F[1, 4, 3, 5, 2]                                           
      DATA Denom(11 )/          108/                                       
      DATA (CF(i,11 ),i=1  ,6  ) /    14,  -58,   -4,    5,    5,   14/    
      DATA (CF(i,11 ),i=7  ,12 ) /    68,   -4,  -58,  -58,  455,  -40/    
      DATA (CF(i,11 ),i=13 ,18 ) /    68,   68,   -4,   14,   -4,   68/    
      DATA (CF(i,11 ),i=19 ,24 ) /   -58,   14,   -4,   14,  -58,   68/    
C               F[1, 3, 5, 2, 4]                                           
      DATA Denom(12 )/          108/                                       
      DATA (CF(i,12 ),i=1  ,6  ) /    -4,   68,   14,    5,    5,   -4/    
      DATA (CF(i,12 ),i=7  ,12 ) /   -58,   14,   68,   68,  -40,  455/    
      DATA (CF(i,12 ),i=13 ,18 ) /   -58,  -58,   14,   -4,   14,  -58/    
      DATA (CF(i,12 ),i=19 ,24 ) /    68,   -4,   14,   -4,   68,  -58/    
C               F[1, 4, 2, 5, 3]                                           
      DATA Denom(13 )/          108/                                       
      DATA (CF(i,13 ),i=1  ,6  ) /   -58,   14,    5,   -4,   14,    5/    
      DATA (CF(i,13 ),i=7  ,12 ) /    -4,   68,   14,  -40,   68,  -58/    
      DATA (CF(i,13 ),i=13 ,18 ) /   455,   -4,   68,  -58,  -58,   14/    
      DATA (CF(i,13 ),i=19 ,24 ) /    -4,   68,  -58,   68,   -4,   14/    
C               F[1, 2, 5, 3, 4]                                           
      DATA Denom(14 )/          108/                                       
      DATA (CF(i,14 ),i=1  ,6  ) /   -58,   -4,  -58,   -4,   14,   68/    
      DATA (CF(i,14 ),i=7  ,12 ) /    14,   68,  -40,   14,   68,  -58/    
      DATA (CF(i,14 ),i=13 ,18 ) /    -4,  455,  -58,   68,    5,   -4/    
      DATA (CF(i,14 ),i=19 ,24 ) /    14,    5,   68,  -58,   -4,   14/    
C               F[1, 4, 5, 2, 3]                                           
      DATA Denom(15 )/          108/                                       
      DATA (CF(i,15 ),i=1  ,6  ) /    -4,  -58,   -4,  -58,   68,   14/    
      DATA (CF(i,15 ),i=7  ,12 ) /    68,   14,   68,  -58,   -4,   14/    
      DATA (CF(i,15 ),i=13 ,18 ) /    68,  -58,  455,  -40,   14,    5/    
      DATA (CF(i,15 ),i=19 ,24 ) /     5,   -4,   -4,   14,   68,  -58/    
C               F[1, 4, 5, 3, 2]                                           
      DATA Denom(16 )/          108/                                       
      DATA (CF(i,16 ),i=1  ,6  ) /    14,   68,   14,   68,  -58,   -4/    
      DATA (CF(i,16 ),i=7  ,12 ) /   -58,   -4,  -58,   68,   14,   -4/    
      DATA (CF(i,16 ),i=13 ,18 ) /   -58,   68,  -40,  455,   -4,    5/    
      DATA (CF(i,16 ),i=19 ,24 ) /     5,   14,   14,   -4,  -58,   68/    
C               F[1, 2, 3, 5, 4]                                           
      DATA Denom(17 )/          108/                                       
      DATA (CF(i,17 ),i=1  ,6  ) /    14,  -58,   14,  -58,   68,   -4/    
      DATA (CF(i,17 ),i=7  ,12 ) /    68,   -4,    5,   68,   -4,   14/    
      DATA (CF(i,17 ),i=13 ,18 ) /   -58,    5,   14,   -4,  455,  -58/    
      DATA (CF(i,17 ),i=19 ,24 ) /    68,  -40,   -4,   14,  -58,   68/    
C               F[1, 5, 3, 4, 2]                                           
      DATA Denom(18 )/          108/                                       
      DATA (CF(i,18 ),i=1  ,6  ) /    68,   -4,   68,   -4,   14,  -58/    
      DATA (CF(i,18 ),i=7  ,12 ) /    14,  -58,   14,   -4,   68,  -58/    
      DATA (CF(i,18 ),i=13 ,18 ) /    14,   -4,    5,    5,  -58,  455/    
      DATA (CF(i,18 ),i=19 ,24 ) /   -40,   68,   68,  -58,   14,   -4/    
C               F[1, 3, 4, 2, 5]                                           
      DATA Denom(19 )/          108/                                       
      DATA (CF(i,19 ),i=1  ,6  ) /   -58,   14,  -58,   14,   -4,   68/    
      DATA (CF(i,19 ),i=7  ,12 ) /    -4,   68,   -4,   14,  -58,   68/    
      DATA (CF(i,19 ),i=13 ,18 ) /    -4,   14,    5,    5,   68,  -40/    
      DATA (CF(i,19 ),i=19 ,24 ) /   455,  -58,  -58,   68,   -4,   14/    
C               F[1, 5, 2, 4, 3]                                           
      DATA Denom(20 )/          108/                                       
      DATA (CF(i,20 ),i=1  ,6  ) /    -4,   68,   -4,   68,  -58,   14/    
      DATA (CF(i,20 ),i=7  ,12 ) /   -58,   14,    5,  -58,   14,   -4/    
      DATA (CF(i,20 ),i=13 ,18 ) /    68,    5,   -4,   14,  -40,   68/    
      DATA (CF(i,20 ),i=19 ,24 ) /   -58,  455,   14,   -4,   68,  -58/    
C               F[1, 2, 4, 3, 5]                                           
      DATA Denom(21 )/          108/                                       
      DATA (CF(i,21 ),i=1  ,6  ) /    -4,    5,   14,  -58,   68,   -4/    
      DATA (CF(i,21 ),i=7  ,12 ) /     5,   14,  -58,   68,   -4,   14/    
      DATA (CF(i,21 ),i=13 ,18 ) /   -58,   68,   -4,   14,   -4,   68/    
      DATA (CF(i,21 ),i=19 ,24 ) /   -58,   14,  455,  -40,   68,  -58/    
C               F[1, 2, 5, 4, 3]                                           
      DATA Denom(22 )/          108/                                       
      DATA (CF(i,22 ),i=1  ,6  ) /    14,    5,   -4,   68,  -58,   14/    
      DATA (CF(i,22 ),i=7  ,12 ) /     5,   -4,   68,  -58,   14,   -4/    
      DATA (CF(i,22 ),i=13 ,18 ) /    68,  -58,   14,   -4,   14,  -58/    
      DATA (CF(i,22 ),i=19 ,24 ) /    68,   -4,  -40,  455,  -58,   68/    
C               F[1, 3, 4, 5, 2]                                           
      DATA Denom(23 )/          108/                                       
      DATA (CF(i,23 ),i=1  ,6  ) /     5,   -4,  -58,   14,   -4,   68/    
      DATA (CF(i,23 ),i=7  ,12 ) /    14,    5,   14,   14,  -58,   68/    
      DATA (CF(i,23 ),i=13 ,18 ) /    -4,   -4,   68,  -58,  -58,   14/    
      DATA (CF(i,23 ),i=19 ,24 ) /    -4,   68,   68,  -58,  455,  -40/    
C               F[1, 3, 5, 4, 2]                                           
      DATA Denom(24 )/          108/                                       
      DATA (CF(i,24 ),i=1  ,6  ) /     5,   14,   68,   -4,   14,  -58/    
      DATA (CF(i,24 ),i=7  ,12 ) /    -4,    5,   -4,   -4,   68,  -58/    
      DATA (CF(i,24 ),i=13 ,18 ) /    14,   14,  -58,   68,   68,   -4/    
      DATA (CF(i,24 ),i=19 ,24 ) /    14,  -58,  -58,   68,  -40,  455/    
C               F[1, 2, 4, 5, 3]                                           
C ----------
C BEGIN CODE
C ----------
      B_CL_276_003 = (0D0,0D0)  
      BORNTILDE = (0d0,0d0)
      do ihel=-1,1,2
      if (.not. calculatedBorn) then
      CALL VXXXXX(P(0,1   ),ZERO ,NHEL(1   ),-1*IC(1   ),W(1,1   ))        
      CALL VXXXXX(P(0,2   ),ZERO ,NHEL(2   ),-1*IC(2   ),W(1,2   ))        
      CALL VXXXXX(P(0,  3),ZERO,ihel,+1*IC(  3),W(1,  3))
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
      DO I = 1, NCOLOR
          ZTEMP = (0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          RTEMP=DBLE(ZTEMP*DCONJG(JAMP(I))/DENOM(I))
          if (ihel.eq.-1) then
             B_CL_276_003=B_CL_276_003 +RTEMP
          elseif (ihel.eq.1) then
             B_CL_276_003=B_CL_276_003 +(0d0,1d0)*RTEMP
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
       
       
