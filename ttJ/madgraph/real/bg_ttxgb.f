      SUBROUTINE Sbg_ttxgb(P1,ANS)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL_REAL)
C  
C FOR PROCESS : b g -> t t~ g b  
C  
C Crossing   1 is b g -> t t~ g b  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      Include "../genps.inc"
      INTEGER                 NCOMB,     NCROSS         
      PARAMETER (             NCOMB=  64, NCROSS=  1)
      INTEGER    THEL
      PARAMETER (THEL=NCOMB*NCROSS)
C  
C ARGUMENTS 
C  
      REAL*8 P1(0:3,NEXTERNAL_REAL),ANS(NCROSS)
C  
C LOCAL VARIABLES 
C  
      INTEGER NHEL(NEXTERNAL_REAL,NCOMB),NTRY
      REAL*8 T, P(0:3,NEXTERNAL_REAL)
      REAL*8 bg_ttxgb
      INTEGER IHEL,IDEN(NCROSS),IC(NEXTERNAL_REAL,NCROSS)
      INTEGER IPROC,JC(NEXTERNAL_REAL), I
      LOGICAL GOODHEL(NCOMB,NCROSS)
      INTEGER NGRAPHS
      REAL*8 hwgt, xtot, xtry, xrej, xr, yfrac(0:ncomb)
      INTEGER idum, ngood, igood(ncomb), jhel, j, jj
      LOGICAL warned
      REAL     xran1
      EXTERNAL xran1
C  
C GLOBAL VARIABLES
C  
      Double Precision amp2(maxamps), jamp2(0:maxamps)
      common/to_amps_bg_ttxgb/  amp2,       jamp2

      character*79         hel_buff
      common/to_helicity/  hel_buff

      REAL*8 POL(2)
      common/to_polarization/ POL

      integer          isum_hel
      logical                    multi_channel
      common/to_matrix/isum_hel, multi_channel
      INTEGER MAPCONFIG(0:LMAXCONFIGS), ICONFIG
      common/to_mconfigs/mapconfig, iconfig
      DATA NTRY,IDUM /0,-1/
      DATA xtry, xrej, ngood /0,0,0/
      DATA warned/.false./
      
      SAVE yfrac, igood, jhel
      DATA NGRAPHS /   38/          
      DATA jamp2(0) /  20/          
      DATA GOODHEL/THEL*.FALSE./
      DATA (NHEL(IHEL,   1),IHEL=1, 6) /-1,-1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,   2),IHEL=1, 6) /-1,-1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,   3),IHEL=1, 6) /-1,-1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,   4),IHEL=1, 6) /-1,-1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,   5),IHEL=1, 6) /-1,-1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,   6),IHEL=1, 6) /-1,-1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,   7),IHEL=1, 6) /-1,-1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,   8),IHEL=1, 6) /-1,-1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,   9),IHEL=1, 6) /-1,-1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  10),IHEL=1, 6) /-1,-1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  11),IHEL=1, 6) /-1,-1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  12),IHEL=1, 6) /-1,-1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  13),IHEL=1, 6) /-1,-1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  14),IHEL=1, 6) /-1,-1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  15),IHEL=1, 6) /-1,-1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  16),IHEL=1, 6) /-1,-1, 1, 1, 1, 1/
      DATA (NHEL(IHEL,  17),IHEL=1, 6) /-1, 1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  18),IHEL=1, 6) /-1, 1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  19),IHEL=1, 6) /-1, 1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  20),IHEL=1, 6) /-1, 1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  21),IHEL=1, 6) /-1, 1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  22),IHEL=1, 6) /-1, 1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  23),IHEL=1, 6) /-1, 1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  24),IHEL=1, 6) /-1, 1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,  25),IHEL=1, 6) /-1, 1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  26),IHEL=1, 6) /-1, 1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  27),IHEL=1, 6) /-1, 1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  28),IHEL=1, 6) /-1, 1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  29),IHEL=1, 6) /-1, 1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  30),IHEL=1, 6) /-1, 1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  31),IHEL=1, 6) /-1, 1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  32),IHEL=1, 6) /-1, 1, 1, 1, 1, 1/
      DATA (NHEL(IHEL,  33),IHEL=1, 6) / 1,-1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  34),IHEL=1, 6) / 1,-1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  35),IHEL=1, 6) / 1,-1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  36),IHEL=1, 6) / 1,-1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  37),IHEL=1, 6) / 1,-1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  38),IHEL=1, 6) / 1,-1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  39),IHEL=1, 6) / 1,-1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  40),IHEL=1, 6) / 1,-1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,  41),IHEL=1, 6) / 1,-1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  42),IHEL=1, 6) / 1,-1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  43),IHEL=1, 6) / 1,-1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  44),IHEL=1, 6) / 1,-1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  45),IHEL=1, 6) / 1,-1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  46),IHEL=1, 6) / 1,-1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  47),IHEL=1, 6) / 1,-1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  48),IHEL=1, 6) / 1,-1, 1, 1, 1, 1/
      DATA (NHEL(IHEL,  49),IHEL=1, 6) / 1, 1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  50),IHEL=1, 6) / 1, 1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  51),IHEL=1, 6) / 1, 1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  52),IHEL=1, 6) / 1, 1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  53),IHEL=1, 6) / 1, 1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  54),IHEL=1, 6) / 1, 1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  55),IHEL=1, 6) / 1, 1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  56),IHEL=1, 6) / 1, 1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,  57),IHEL=1, 6) / 1, 1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  58),IHEL=1, 6) / 1, 1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  59),IHEL=1, 6) / 1, 1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  60),IHEL=1, 6) / 1, 1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  61),IHEL=1, 6) / 1, 1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  62),IHEL=1, 6) / 1, 1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  63),IHEL=1, 6) / 1, 1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  64),IHEL=1, 6) / 1, 1, 1, 1, 1, 1/
      DATA (  IC(IHEL,  1),IHEL=1, 6) / 1, 2, 3, 4, 5, 6/
      DATA (IDEN(IHEL),IHEL=  1,  1) /  96/
C ----------
C BEGIN CODE
C ----------
      NTRY=NTRY+1
      DO IPROC=1,NCROSS
      CALL SWITCHMOM(P1,P,IC(1,IPROC),JC,NEXTERNAL_REAL)
      DO IHEL=1,NEXTERNAL_REAL
         JC(IHEL) = +1
      ENDDO
       
      IF (multi_channel) THEN
          DO IHEL=1,NGRAPHS
              amp2(ihel)=0d0
              jamp2(ihel)=0d0
          ENDDO
          DO IHEL=1,int(jamp2(0))
              jamp2(ihel)=0d0
          ENDDO
      ENDIF
      ANS(IPROC) = 0D0
      write(hel_buff,'(16i5)') (0,i=1,NEXTERNAL_REAL)
      IF (ISUM_HEL .EQ. 0 .OR. NTRY .LT. 10) THEN
          DO IHEL=1,NCOMB
             IF (GOODHEL(IHEL,IPROC) .OR. NTRY .LT. 2) THEN
                 T=bg_ttxgb(P ,NHEL(1,IHEL),JC(1))            
               DO JJ=1,nincoming
                 IF(POL(JJ).NE.1d0.AND.
     &              NHEL(JJ,IHEL).EQ.INT(SIGN(1d0,POL(JJ)))) THEN
                   T=T*ABS(POL(JJ))
                 ELSE IF(POL(JJ).NE.1d0)THEN
                   T=T*(2d0-ABS(POL(JJ)))
                 ENDIF
               ENDDO
               ANS(IPROC)=ANS(IPROC)+T
               IF (T .NE. 0D0 .AND. .NOT.    GOODHEL(IHEL,IPROC)) THEN
                   GOODHEL(IHEL,IPROC)=.TRUE.
                   NGOOD = NGOOD +1
                   IGOOD(NGOOD) = IHEL
               ENDIF
             ENDIF
          ENDDO
          JHEL = 1
          ISUM_HEL=MIN(ISUM_HEL,NGOOD)
      ELSE              !RANDOM HELICITY
          DO J=1,ISUM_HEL
              JHEL=JHEL+1
              IF (JHEL .GT. NGOOD) JHEL=1
              HWGT = REAL(NGOOD)/REAL(ISUM_HEL)
              IHEL = IGOOD(JHEL)
              T=bg_ttxgb(P ,NHEL(1,IHEL),JC(1))            
              DO JJ=1,nincoming
                IF(POL(JJ).NE.1d0.AND.
     &             NHEL(JJ,IHEL).EQ.INT(SIGN(1d0,POL(JJ)))) THEN
                  T=T*ABS(POL(JJ))
                ELSE IF(POL(JJ).NE.1d0)THEN
                  T=T*(2d0-ABS(POL(JJ)))
                ENDIF
              ENDDO
              ANS(IPROC)=ANS(IPROC)+T*HWGT
          ENDDO
          IF (ISUM_HEL .EQ. 1) THEN
              WRITE(HEL_BUFF,'(16i5)')(NHEL(i,IHEL),i=1,NEXTERNAL_REAL)
          ENDIF
      ENDIF
      IF (MULTI_CHANNEL) THEN
          XTOT=0D0
          DO IHEL=1,MAPCONFIG(0)
              XTOT=XTOT+AMP2(MAPCONFIG(IHEL))
          ENDDO
          IF (XTOT.NE.0D0) THEN
              ANS(IPROC)=ANS(IPROC)*AMP2(MAPCONFIG(ICONFIG))/XTOT
          ELSE
              ANS(IPROC)=0D0
          ENDIF
      ENDIF
      ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC))
      ENDDO
      END
       
       
      REAL*8 FUNCTION bg_ttxgb(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL_REAL)
C  
C FOR PROCESS : b g -> t t~ g b  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER (NGRAPHS=  38,NEIGEN= 20) 
      include "../genps.inc"
      INTEGER    NWAVEFUNCS     , NCOLOR
      PARAMETER (NWAVEFUNCS=  58, NCOLOR=  20) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 P(0:3,NEXTERNAL_REAL)
      INTEGER NHEL(NEXTERNAL_REAL), IC(NEXTERNAL_REAL)
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
      Double Precision amp2(maxamps), jamp2(0:maxamps)
      common/to_amps_bg_ttxgb/  amp2,       jamp2
      include "../coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1  )/           18/                                       
      DATA (CF(i,1  ),i=1  ,6  ) /    64,   -8,  -16,   56,   -8,    1/    
      DATA (CF(i,1  ),i=7  ,12 ) /    10,    2,   -7,    1,   -6,   57/    
      DATA (CF(i,1  ),i=13 ,18 ) /    -6,   12,   -6,    2,   -7,    2/    
      DATA (CF(i,1  ),i=19 ,20 ) /    -7,   -6/                            
C               T[ 6, 4]T[ 3, 1, 2, 5]                                     
      DATA Denom(2  )/           18/                                       
      DATA (CF(i,2  ),i=1  ,6  ) /    -8,   64,   56,  -16,    1,   10/    
      DATA (CF(i,2  ),i=7  ,12 ) /     1,   -7,    2,   -8,   12,   -6/    
      DATA (CF(i,2  ),i=13 ,18 ) /    -6,   -6,   -6,    2,   -7,  -16/    
      DATA (CF(i,2  ),i=19 ,20 ) /    56,   57/                            
C               T[ 6, 4, 5]T[ 3, 1, 2]                                     
      DATA Denom(3  )/           18/                                       
      DATA (CF(i,3  ),i=1  ,6  ) /   -16,   56,   64,   -8,    2,    2/    
      DATA (CF(i,3  ),i=7  ,12 ) /    -7,   -8,    1,   -7,    2,   -7/    
      DATA (CF(i,3  ),i=13 ,18 ) /     2,  -16,   -7,   -6,   -6,   -6/    
      DATA (CF(i,3  ),i=19 ,20 ) /    57,   56/                            
C               T[ 6, 4, 5]T[ 3, 1, 2]                                     
      DATA Denom(4  )/           18/                                       
      DATA (CF(i,4  ),i=1  ,6  ) /    56,  -16,   -8,   64,   -7,   -7/    
      DATA (CF(i,4  ),i=7  ,12 ) /     2,    1,   -8,    2,  -16,   56/    
      DATA (CF(i,4  ),i=13 ,18 ) /     2,    2,   -7,   -6,   -6,   12/    
      DATA (CF(i,4  ),i=19 ,20 ) /    -6,   -7/                            
C               T[ 6, 4]T[ 3, 1, 2, 5]                                     
      DATA Denom(5  )/           18/                                       
      DATA (CF(i,5  ),i=1  ,6  ) /    -8,    1,    2,   -7,   64,   -8/    
      DATA (CF(i,5  ),i=7  ,12 ) /     1,    2,   -7,   10,   -6,   -6/    
      DATA (CF(i,5  ),i=13 ,18 ) /    12,   -6,   57,  -16,   56,    2/    
      DATA (CF(i,5  ),i=19 ,20 ) /    -7,   -6/                            
C               T[ 6, 4]T[ 3, 1, 5, 2]                                     
      DATA Denom(6  )/           18/                                       
      DATA (CF(i,6  ),i=1  ,6  ) /     1,   10,    2,   -7,   -8,   64/    
      DATA (CF(i,6  ),i=7  ,12 ) /    -8,  -16,   56,    1,   57,   -6/    
      DATA (CF(i,6  ),i=13 ,18 ) /    -6,   -6,   -6,   56,  -16,   -7/    
      DATA (CF(i,6  ),i=19 ,20 ) /     2,   12/                            
C               T[ 6, 4, 2]T[ 3, 1, 5]                                     
      DATA Denom(7  )/           18/                                       
      DATA (CF(i,7  ),i=1  ,6  ) /    10,    1,   -7,    2,    1,   -8/    
      DATA (CF(i,7  ),i=7  ,12 ) /    64,   56,  -16,   -8,   -6,   12/    
      DATA (CF(i,7  ),i=13 ,18 ) /    -6,   57,   -6,   -7,    2,   -7/    
      DATA (CF(i,7  ),i=19 ,20 ) /     2,   -6/                            
C               T[ 6, 4, 5, 2]T[ 3, 1]                                     
      DATA Denom(8  )/           18/                                       
      DATA (CF(i,8  ),i=1  ,6  ) /     2,   -7,   -8,    1,    2,  -16/    
      DATA (CF(i,8  ),i=7  ,12 ) /    56,   64,   -8,   -7,   -7,    2/    
      DATA (CF(i,8  ),i=13 ,18 ) /    -7,   56,    2,   -6,   12,   -6/    
      DATA (CF(i,8  ),i=19 ,20 ) /    -6,  -16/                            
C               T[ 3, 1]T[ 6, 4, 5, 2]                                     
      DATA Denom(9  )/           18/                                       
      DATA (CF(i,9  ),i=1  ,6  ) /    -7,    2,    1,   -8,   -7,   56/    
      DATA (CF(i,9  ),i=7  ,12 ) /   -16,   -8,   64,    2,   56,  -16/    
      DATA (CF(i,9  ),i=13 ,18 ) /    -7,   -7,    2,   57,   -6,   -6/    
      DATA (CF(i,9  ),i=19 ,20 ) /    -6,    2/                            
C               T[ 6, 4, 2]T[ 3, 1, 5]                                     
      DATA Denom(10 )/           18/                                       
      DATA (CF(i,10 ),i=1  ,6  ) /     1,   -8,   -7,    2,   10,    1/    
      DATA (CF(i,10 ),i=7  ,12 ) /    -8,   -7,    2,   64,   -6,   -6/    
      DATA (CF(i,10 ),i=13 ,18 ) /    57,   -6,   12,   -7,    2,   56/    
      DATA (CF(i,10 ),i=19 ,20 ) /   -16,   -6/                            
C               T[ 6, 4, 2, 5]T[ 3, 1]                                     
      DATA Denom(11 )/           18/                                       
      DATA (CF(i,11 ),i=1  ,6  ) /    -6,   12,    2,  -16,   -6,   57/    
      DATA (CF(i,11 ),i=7  ,12 ) /    -6,   -7,   56,   -6,   64,   -8/    
      DATA (CF(i,11 ),i=13 ,18 ) /    -8,    1,    1,   56,   -7,  -16/    
      DATA (CF(i,11 ),i=19 ,20 ) /     2,   10/                            
C               T[ 3, 1, 5]T[ 6, 4, 2]                                     
      DATA Denom(12 )/           18/                                       
      DATA (CF(i,12 ),i=1  ,6  ) /    57,   -6,   -7,   56,   -6,   -6/    
      DATA (CF(i,12 ),i=7  ,12 ) /    12,    2,  -16,   -6,   -8,   64/    
      DATA (CF(i,12 ),i=13 ,18 ) /     1,   10,   -8,   -7,   -7,    2/    
      DATA (CF(i,12 ),i=19 ,20 ) /     2,    1/                            
C               T[ 6, 4]T[ 3, 1, 2, 5]                                     
      DATA Denom(13 )/           18/                                       
      DATA (CF(i,13 ),i=1  ,6  ) /    -6,   -6,    2,    2,   12,   -6/    
      DATA (CF(i,13 ),i=7  ,12 ) /    -6,   -7,   -7,   57,   -8,    1/    
      DATA (CF(i,13 ),i=13 ,18 ) /    64,   -8,   10,  -16,    2,   56/    
      DATA (CF(i,13 ),i=19 ,20 ) /    -7,    1/                            
C               T[ 3, 1]T[ 6, 4, 2, 5]                                     
      DATA Denom(14 )/           18/                                       
      DATA (CF(i,14 ),i=1  ,6  ) /    12,   -6,  -16,    2,   -6,   -6/    
      DATA (CF(i,14 ),i=7  ,12 ) /    57,   56,   -7,   -6,    1,   10/    
      DATA (CF(i,14 ),i=13 ,18 ) /    -8,   64,    1,    2,    2,   -7/    
      DATA (CF(i,14 ),i=19 ,20 ) /    -7,   -8/                            
C               T[ 3, 1]T[ 6, 4, 5, 2]                                     
      DATA Denom(15 )/           18/                                       
      DATA (CF(i,15 ),i=1  ,6  ) /    -6,   -6,   -7,   -7,   57,   -6/    
      DATA (CF(i,15 ),i=7  ,12 ) /    -6,    2,    2,   12,    1,   -8/    
      DATA (CF(i,15 ),i=13 ,18 ) /    10,    1,   64,   -7,   56,    2/    
      DATA (CF(i,15 ),i=19 ,20 ) /   -16,   -8/                            
C               T[ 6, 4]T[ 3, 1, 5, 2]                                     
      DATA Denom(16 )/           18/                                       
      DATA (CF(i,16 ),i=1  ,6  ) /     2,    2,   -6,   -6,  -16,   56/    
      DATA (CF(i,16 ),i=7  ,12 ) /    -7,   -6,   57,   -7,   56,   -7/    
      DATA (CF(i,16 ),i=13 ,18 ) /   -16,    2,   -7,   64,   -8,   -8/    
      DATA (CF(i,16 ),i=19 ,20 ) /     1,    2/                            
C               T[ 6, 4, 2]T[ 3, 1, 5]                                     
      DATA Denom(17 )/           18/                                       
      DATA (CF(i,17 ),i=1  ,6  ) /    -7,   -7,   -6,   -6,   56,  -16/    
      DATA (CF(i,17 ),i=7  ,12 ) /     2,   12,   -6,    2,   -7,   -7/    
      DATA (CF(i,17 ),i=13 ,18 ) /     2,    2,   56,   -8,   64,    1/    
      DATA (CF(i,17 ),i=19 ,20 ) /    -8,  -16/                            
C               T[ 6, 4]T[ 3, 1, 5, 2]                                     
      DATA Denom(18 )/           18/                                       
      DATA (CF(i,18 ),i=1  ,6  ) /     2,  -16,   -6,   12,    2,   -7/    
      DATA (CF(i,18 ),i=7  ,12 ) /    -7,   -6,   -6,   56,  -16,    2/    
      DATA (CF(i,18 ),i=13 ,18 ) /    56,   -7,    2,   -8,    1,   64/    
      DATA (CF(i,18 ),i=19 ,20 ) /    -8,   -7/                            
C               T[ 3, 1]T[ 6, 4, 2, 5]                                     
      DATA Denom(19 )/           18/                                       
      DATA (CF(i,19 ),i=1  ,6  ) /    -7,   56,   57,   -6,   -7,    2/    
      DATA (CF(i,19 ),i=7  ,12 ) /     2,   -6,   -6,  -16,    2,    2/    
      DATA (CF(i,19 ),i=13 ,18 ) /    -7,   -7,  -16,    1,   -8,   -8/    
      DATA (CF(i,19 ),i=19 ,20 ) /    64,   56/                            
C               T[ 6, 4, 5]T[ 3, 1, 2]                                     
      DATA Denom(20 )/           18/                                       
      DATA (CF(i,20 ),i=1  ,6  ) /    -6,   57,   56,   -7,   -6,   12/    
      DATA (CF(i,20 ),i=7  ,12 ) /    -6,  -16,    2,   -6,   10,    1/    
      DATA (CF(i,20 ),i=13 ,18 ) /     1,   -8,   -8,    2,  -16,   -7/    
      DATA (CF(i,20 ),i=19 ,20 ) /    56,   64/                            
C               T[ 3, 1, 2]T[ 6, 4, 5]                                     
C ----------
C BEGIN CODE
C ----------
      CALL IXXXXX(P(0,1   ),BMASS ,NHEL(1   ),+1*IC(1   ),W(1,1   ))       
      CALL VXXXXX(P(0,2   ),ZERO ,NHEL(2   ),-1*IC(2   ),W(1,2   ))        
      CALL OXXXXX(P(0,3   ),TMASS ,NHEL(3   ),+1*IC(3   ),W(1,3   ))       
      CALL IXXXXX(P(0,4   ),TMASS ,NHEL(4   ),-1*IC(4   ),W(1,4   ))       
      CALL VXXXXX(P(0,5   ),ZERO ,NHEL(5   ),+1*IC(5   ),W(1,5   ))        
      CALL OXXXXX(P(0,6   ),BMASS ,NHEL(6   ),+1*IC(6   ),W(1,6   ))       
      CALL FVOXXX(W(1,3   ),W(1,2   ),GG ,TMASS   ,TWIDTH  ,W(1,7   ))     
      CALL JIOXXX(W(1,4   ),W(1,7   ),GG ,ZERO    ,ZERO    ,W(1,8   ))     
      CALL JVVXXX(W(1,5   ),W(1,8   ),G ,ZERO    ,ZERO    ,W(1,9   ))      
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,9   ),GG ,AMP(1   ))             
      CALL FVIXXX(W(1,1   ),W(1,8   ),GG ,BMASS   ,ZERO    ,W(1,10  ))     
      CALL IOVXXX(W(1,10  ),W(1,6   ),W(1,5   ),GG ,AMP(2   ))             
      CALL FVIXXX(W(1,1   ),W(1,5   ),GG ,BMASS   ,ZERO    ,W(1,11  ))     
      CALL IOVXXX(W(1,11  ),W(1,6   ),W(1,8   ),GG ,AMP(3   ))             
      CALL JVVXXX(W(1,5   ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,12  ))      
      CALL FVOXXX(W(1,3   ),W(1,12  ),GG ,TMASS   ,TWIDTH  ,W(1,13  ))     
      CALL JIOXXX(W(1,4   ),W(1,13  ),GG ,ZERO    ,ZERO    ,W(1,14  ))     
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,14  ),GG ,AMP(4   ))             
      CALL FVOXXX(W(1,3   ),W(1,5   ),GG ,TMASS   ,TWIDTH  ,W(1,15  ))     
      CALL FVOXXX(W(1,15  ),W(1,2   ),GG ,TMASS   ,TWIDTH  ,W(1,16  ))     
      CALL JIOXXX(W(1,4   ),W(1,16  ),GG ,ZERO    ,ZERO    ,W(1,17  ))     
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,17  ),GG ,AMP(5   ))             
      CALL FVIXXX(W(1,4   ),W(1,5   ),GG ,TMASS   ,TWIDTH  ,W(1,18  ))     
      CALL JIOXXX(W(1,18  ),W(1,7   ),GG ,ZERO    ,ZERO    ,W(1,19  ))     
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,19  ),GG ,AMP(6   ))             
      CALL FVOXXX(W(1,7   ),W(1,5   ),GG ,TMASS   ,TWIDTH  ,W(1,20  ))     
      CALL JIOXXX(W(1,4   ),W(1,20  ),GG ,ZERO    ,ZERO    ,W(1,21  ))     
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,21  ),GG ,AMP(7   ))             
      CALL FVIXXX(W(1,4   ),W(1,2   ),GG ,TMASS   ,TWIDTH  ,W(1,22  ))     
      CALL JIOXXX(W(1,22  ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,23  ))     
      CALL JVVXXX(W(1,5   ),W(1,23  ),G ,ZERO    ,ZERO    ,W(1,24  ))      
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,24  ),GG ,AMP(8   ))             
      CALL FVIXXX(W(1,1   ),W(1,23  ),GG ,BMASS   ,ZERO    ,W(1,25  ))     
      CALL IOVXXX(W(1,25  ),W(1,6   ),W(1,5   ),GG ,AMP(9   ))             
      CALL IOVXXX(W(1,11  ),W(1,6   ),W(1,23  ),GG ,AMP(10  ))             
      CALL FVIXXX(W(1,4   ),W(1,12  ),GG ,TMASS   ,TWIDTH  ,W(1,26  ))     
      CALL JIOXXX(W(1,26  ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,27  ))     
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,27  ),GG ,AMP(11  ))             
      CALL JIOXXX(W(1,22  ),W(1,15  ),GG ,ZERO    ,ZERO    ,W(1,28  ))     
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,28  ),GG ,AMP(12  ))             
      CALL FVIXXX(W(1,18  ),W(1,2   ),GG ,TMASS   ,TWIDTH  ,W(1,29  ))     
      CALL JIOXXX(W(1,29  ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,30  ))     
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,30  ),GG ,AMP(13  ))             
      CALL FVIXXX(W(1,22  ),W(1,5   ),GG ,TMASS   ,TWIDTH  ,W(1,31  ))     
      CALL JIOXXX(W(1,31  ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,32  ))     
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,32  ),GG ,AMP(14  ))             
      CALL JIOXXX(W(1,4   ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,33  ))     
      CALL JVVXXX(W(1,33  ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,34  ))      
      CALL JVVXXX(W(1,5   ),W(1,34  ),G ,ZERO    ,ZERO    ,W(1,35  ))      
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,35  ),GG ,AMP(15  ))             
      CALL FVIXXX(W(1,11  ),W(1,33  ),GG ,BMASS   ,ZERO    ,W(1,36  ))     
      CALL IOVXXX(W(1,36  ),W(1,6   ),W(1,2   ),GG ,AMP(16  ))             
      CALL FVIXXX(W(1,1   ),W(1,34  ),GG ,BMASS   ,ZERO    ,W(1,37  ))     
      CALL IOVXXX(W(1,37  ),W(1,6   ),W(1,5   ),GG ,AMP(17  ))             
      CALL FVIXXX(W(1,11  ),W(1,2   ),GG ,BMASS   ,ZERO    ,W(1,38  ))     
      CALL IOVXXX(W(1,38  ),W(1,6   ),W(1,33  ),GG ,AMP(18  ))             
      CALL IOVXXX(W(1,11  ),W(1,6   ),W(1,34  ),GG ,AMP(19  ))             
      CALL JVVXXX(W(1,33  ),W(1,12  ),G ,ZERO    ,ZERO    ,W(1,39  ))      
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,39  ),GG ,AMP(20  ))             
      CALL FVIXXX(W(1,1   ),W(1,33  ),GG ,BMASS   ,ZERO    ,W(1,40  ))     
      CALL FVIXXX(W(1,40  ),W(1,5   ),GG ,BMASS   ,ZERO    ,W(1,41  ))     
      CALL IOVXXX(W(1,41  ),W(1,6   ),W(1,2   ),GG ,AMP(21  ))             
      CALL FVIXXX(W(1,40  ),W(1,2   ),GG ,BMASS   ,ZERO    ,W(1,42  ))     
      CALL IOVXXX(W(1,42  ),W(1,6   ),W(1,5   ),GG ,AMP(22  ))             
      CALL FVIXXX(W(1,1   ),W(1,12  ),GG ,BMASS   ,ZERO    ,W(1,43  ))     
      CALL IOVXXX(W(1,43  ),W(1,6   ),W(1,33  ),GG ,AMP(23  ))             
      CALL IOVXXX(W(1,40  ),W(1,6   ),W(1,12  ),GG ,AMP(24  ))             
      CALL JIOXXX(W(1,4   ),W(1,15  ),GG ,ZERO    ,ZERO    ,W(1,44  ))     
      CALL JVVXXX(W(1,44  ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,45  ))      
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,45  ),GG ,AMP(25  ))             
      CALL FVIXXX(W(1,1   ),W(1,44  ),GG ,BMASS   ,ZERO    ,W(1,46  ))     
      CALL IOVXXX(W(1,46  ),W(1,6   ),W(1,2   ),GG ,AMP(26  ))             
      CALL FVIXXX(W(1,1   ),W(1,2   ),GG ,BMASS   ,ZERO    ,W(1,47  ))     
      CALL IOVXXX(W(1,47  ),W(1,6   ),W(1,44  ),GG ,AMP(27  ))             
      CALL JIOXXX(W(1,18  ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,48  ))     
      CALL JVVXXX(W(1,48  ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,49  ))      
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,49  ),GG ,AMP(28  ))             
      CALL FVIXXX(W(1,1   ),W(1,48  ),GG ,BMASS   ,ZERO    ,W(1,50  ))     
      CALL IOVXXX(W(1,50  ),W(1,6   ),W(1,2   ),GG ,AMP(29  ))             
      CALL IOVXXX(W(1,47  ),W(1,6   ),W(1,48  ),GG ,AMP(30  ))             
      CALL JGGGXX(W(1,5   ),W(1,2   ),W(1,33  ),G ,W(1,51  ))              
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,51  ),GG ,AMP(31  ))             
      CALL JGGGXX(W(1,33  ),W(1,5   ),W(1,2   ),G ,W(1,52  ))              
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,52  ),GG ,AMP(32  ))             
      CALL JGGGXX(W(1,2   ),W(1,33  ),W(1,5   ),G ,W(1,53  ))              
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,53  ),GG ,AMP(33  ))             
      CALL JVVXXX(W(1,5   ),W(1,33  ),G ,ZERO    ,ZERO    ,W(1,54  ))      
      CALL JVVXXX(W(1,54  ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,55  ))      
      CALL IOVXXX(W(1,1   ),W(1,6   ),W(1,55  ),GG ,AMP(34  ))             
      CALL FVIXXX(W(1,1   ),W(1,54  ),GG ,BMASS   ,ZERO    ,W(1,56  ))     
      CALL IOVXXX(W(1,56  ),W(1,6   ),W(1,2   ),GG ,AMP(35  ))             
      CALL FVIXXX(W(1,47  ),W(1,33  ),GG ,BMASS   ,ZERO    ,W(1,57  ))     
      CALL IOVXXX(W(1,57  ),W(1,6   ),W(1,5   ),GG ,AMP(36  ))             
      CALL IOVXXX(W(1,47  ),W(1,6   ),W(1,54  ),GG ,AMP(37  ))             
      CALL FVIXXX(W(1,47  ),W(1,5   ),GG ,BMASS   ,ZERO    ,W(1,58  ))     
      CALL IOVXXX(W(1,58  ),W(1,6   ),W(1,33  ),GG ,AMP(38  ))             
      JAMP(   1) = +AMP(   1)+AMP(   4)-AMP(   7)+AMP(  15)+AMP(  20)
     &             +AMP(  31)-AMP(  33)
      JAMP(   2) = -AMP(   1)-AMP(   6)-AMP(  15)-AMP(  28)-AMP(  32)
     &             +AMP(  33)-AMP(  34)
      JAMP(   3) = -AMP(   2)-AMP(  17)
      JAMP(   4) = -AMP(   3)-AMP(  19)
      JAMP(   5) = -AMP(   4)-AMP(   5)-AMP(  20)-AMP(  25)-AMP(  31)
     &             +AMP(  32)+AMP(  34)
      JAMP(   6) = +AMP(   8)-AMP(  12)-AMP(  15)+AMP(  25)-AMP(  32)
     &             +AMP(  33)-AMP(  34)
      JAMP(   7) = -AMP(   8)-AMP(  11)-AMP(  14)+AMP(  15)+AMP(  20)
     &             +AMP(  31)-AMP(  33)
      JAMP(   8) = -AMP(   9)+AMP(  17)
      JAMP(   9) = -AMP(  10)+AMP(  19)
      JAMP(  10) = +AMP(  11)-AMP(  13)-AMP(  20)+AMP(  28)-AMP(  31)
     &             +AMP(  32)+AMP(  34)
      JAMP(  11) = -AMP(  16)+AMP(  35)
      JAMP(  12) = -AMP(  18)+AMP(  23)
      JAMP(  13) = -AMP(  21)+AMP(  24)-AMP(  35)
      JAMP(  14) = -AMP(  22)-AMP(  24)
      JAMP(  15) = -AMP(  23)-AMP(  38)
      JAMP(  16) = -AMP(  26)
      JAMP(  17) = -AMP(  27)+AMP(  37)
      JAMP(  18) = -AMP(  29)
      JAMP(  19) = -AMP(  30)-AMP(  37)
      JAMP(  20) = -AMP(  36)
      bg_ttxgb = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP = (0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          bg_ttxgb =bg_ttxgb+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       
