      SUBROUTINE Sug_ttxu(P1,ANS,ANSJK,ANSMUNU)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL_BORN)
C  
C FOR PROCESS : u g -> t t~ u  
C  
C Crossing   1 is u g -> t t~ u  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      Include "../genps.inc"
      INTEGER                 NCOMB,     NCROSS         
      PARAMETER (             NCOMB=  32, NCROSS=  1)
      INTEGER    THEL
      PARAMETER (THEL=NCOMB*NCROSS)
C  
C ARGUMENTS 
C  
      REAL*8 P1(0:3,NEXTERNAL_BORN),ANS(NCROSS)
C ARGUMENTS ADDED TO HANDLE COLOR AND SPIN CORRELATED BORNS
      REAL*8 ansjk(NEXTERNAL_BORN,NEXTERNAL_BORN,ncross)
      REAL*8 ansmunu(0:3,0:3,NEXTERNAL_BORN,ncross)
C 
C EXTRA LOCAL VARIABLES
      complex *16 eps1(6),eps2(6)
      real * 8 amp2munu(NEXTERNAL_BORN,0:3,0:3) 
      integer NCOLOR
      parameter (NCOLOR=   4 )
      integer opphel,pol1,pol2,mu,nu

C  
C LOCAL VARIABLES 
C  
      INTEGER NHEL(NEXTERNAL_BORN,NCOMB),NTRY
      REAL*8 T, P(0:3,NEXTERNAL_BORN)
      REAL*8 ug_ttxu
      INTEGER IHEL,IDEN(NCROSS),IC(NEXTERNAL_BORN,NCROSS)
      INTEGER IPROC,JC(NEXTERNAL_BORN), I
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
      common/to_amps_ug_ttxu/  amp2,       jamp2

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
      DATA NGRAPHS /    5/          
      DATA jamp2(0) /   4/          
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
      DATA (IDEN(IHEL),IHEL=  1,  1) /  96/
C ----------
C
C  MODIFIED TO INTERFACE TO POWHEG
C  VARIABLES NEEDED TO EVALUATE COLOR AND 
C  SPIN-CORRELATED BORN AMPLITUDES
C 
      integer II,jjj,kkk
      integer bflav(NEXTERNAL_BORN)
      DATA (bflav(i),i=1,NEXTERNAL_BORN) / 2, 0, 6, -6, 2 /
      complex * 16 ztemp2,amphel(NCOMB,NCOLOR)
      real * 8 amp2jk(NEXTERNAL_BORN,NEXTERNAL_BORN) 
      integer helindex,helcjn
      real * 8 cDENOM(NCOLOR), cCF(NCOLOR,NCOLOR)
      common/to_POWHEG_ug_ttxu/amp2jk,cCF,cDENOM,amphel,helindex
C          
C BEGIN CODE
C ----------
      NTRY=NTRY+1
      DO IPROC=1,NCROSS
      CALL SWITCHMOM(P1,P,IC(1,IPROC),JC,NEXTERNAL_BORN)
      DO IHEL=1,NEXTERNAL_BORN
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
C
      do jjj=1,nexternal_born 
       do mu=0,3
        do nu=0,3
          ansmunu(mu,nu,jjj,iproc)=0d0
          amp2munu(jjj,mu,nu)=0d0
        enddo
       enddo
      do kkk=1,nexternal_born 
         ansjk(jjj,kkk,iproc)=0d0
         amp2jk(jjj,kkk)=0d0
      enddo
      enddo
      do ii=1,ncolor
         do ihel=1,ncomb
            amphel(ihel,ii)=(0d0,0d0)
         enddo
      enddo
C
      write(hel_buff,'(16i5)') (0,i=1,NEXTERNAL_BORN)
      IF (ISUM_HEL .EQ. 0 .OR. NTRY .LT. 10) THEN
          DO IHEL=1,NCOMB
             IF (GOODHEL(IHEL,IPROC) .OR. NTRY .LT. 2) THEN
                helindex=ihel
                 T=ug_ttxu(P ,NHEL(1,IHEL),JC(1))            
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
              T=ug_ttxu(P ,NHEL(1,IHEL),JC(1))            
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
              WRITE(HEL_BUFF,'(16i5)')(NHEL(i,IHEL),i=1,NEXTERNAL_BORN)
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
C 
      do jjj=1,nexternal_born
         do kkk=jjj+1,nexternal_born
              ansjk(jjj,kkk,iproc)=amp2jk(jjj,kkk)/dble(iden(iproc))
              ansjk(kkk,jjj,iproc)=ansjk(jjj,kkk,iproc)
         enddo
         if(bflav(jjj).eq.0) then
            do ihel=1,ncomb
               pol1=nhel(jjj,ihel)
               pol2=-pol1
               opphel=1
               do i=1,nexternal_born
                  if (i.ne.jjj) then
                     opphel=opphel+((nhel(i,ihel)+1)/2)*2
     $                    **(nexternal_born-i)
                  endif
               enddo
               opphel=opphel+((-nhel(jjj,ihel)+1)/2)*2**(nexternal_born
     $              -jjj)
               helcjn=1
               if(jjj.gt.2) helcjn=-1
               call vxxxxx(p(0,jjj),0d0,pol1,helcjn,eps1)
               call vxxxxx(p(0,jjj),0d0,pol2,-helcjn,eps2)
               do mu=0,3
                  do nu=0,3
                     do i=1,ncolor
                         ZTEMP2 = (0.D0,0.D0)
                        do ii=1,ncolor
                           ztemp2=ztemp2+cCF(II,I)*amphel(ihel,II)
                        enddo
                        amp2munu(jjj,mu,nu)=amp2munu(jjj,mu,nu)
c Same helicities
     $                       +ztemp2*dconjg(amphel(ihel ,i))/cDENOM(I)
     $                       *eps1(mu+1) *dconjg(eps1(nu+1))
c Opposite helicities
     $                       +ztemp2*dconjg(amphel(opphel,i))/cDENOM(I)
     $                       *eps1(mu+1)*eps2(nu+1)
                     enddo
                     ansmunu(mu,nu,jjj,iproc)=amp2munu(jjj,mu,nu)
     $           /real(IDEN(IPROC))
                  enddo
               enddo
            enddo
         endif
       enddo
C
      ENDDO
      
C 
      END
       
       
      REAL*8 FUNCTION ug_ttxu(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL_BORN)
C  
C FOR PROCESS : u g -> t t~ u  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER (NGRAPHS=   5,NEIGEN=  4) 
      include "../genps.inc"
      INTEGER    NWAVEFUNCS     , NCOLOR
      PARAMETER (NWAVEFUNCS=  12, NCOLOR=   4) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 P(0:3,NEXTERNAL_BORN)
      INTEGER NHEL(NEXTERNAL_BORN), IC(NEXTERNAL_BORN)
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
      common/to_amps_ug_ttxu/  amp2,       jamp2
      include "../coupl.inc"
C  
C 
      INTEGER NCOMB
      parameter (NCOMB=  32)
C
      REAL*8 DENOM_CLBS(NCOLOR,NEXTERNAL_BORN,NEXTERNAL_BORN) 
      REAL*8 CF_CLBS(NCOLOR,NCOLOR,NEXTERNAL_BORN,NEXTERNAL_BORN)
C COLOR DATA
      DATA Denom_CLBS(1  ,1,2)/         2/                                    
      DATA (CF_CLBS(i,1  ,1,2),i=1  ,4  ) / -7,  0,  0, -7/             
C               T[ 5,4]T[ 3,1,2]                                        
      DATA Denom_CLBS(2  ,1,2)/         1/                                    
      DATA (CF_CLBS(i,2  ,1,2),i=1  ,4  ) /  0, -1,  0,  1/             
C               T[ 5,4,2]T[ 3,1]                                        
      DATA Denom_CLBS(3  ,1,2)/         2/                                    
      DATA (CF_CLBS(i,3  ,1,2),i=1  ,4  ) /  0,  0,  1,  1/             
C               T[ 3,1]T[ 5,4,2]                                        
      DATA Denom_CLBS(4  ,1,2)/         2/                                    
      DATA (CF_CLBS(i,4  ,1,2),i=1  ,4  ) / -7,  2,  1, -8/             
C               T[ 5,4]T[ 3,1,2]                                        
C  
      DATA Denom_CLBS(1  ,1,3)/        18/                                    
      DATA (CF_CLBS(i,1  ,1,3),i=1  ,4  ) /  7,  7,  6,  6/             
C               T[ 5,4]T[ 3,1,2]                                        
      DATA Denom_CLBS(2  ,1,3)/        18/                                    
      DATA (CF_CLBS(i,2  ,1,3),i=1  ,4  ) /  7,-56,-57,  6/             
C               T[ 5,4,2]T[ 3,1]                                        
      DATA Denom_CLBS(3  ,1,3)/        18/                                    
      DATA (CF_CLBS(i,3  ,1,3),i=1  ,4  ) /  6,-57,-56,  7/             
C               T[ 3,1]T[ 5,4,2]                                        
      DATA Denom_CLBS(4  ,1,3)/        18/                                    
      DATA (CF_CLBS(i,4  ,1,3),i=1  ,4  ) /  6,  6,  7,  7/             
C               T[ 5,4]T[ 3,1,2]                                        
C  
      DATA Denom_CLBS(1  ,1,4)/         9/                                    
      DATA (CF_CLBS(i,1  ,1,4),i=1  ,4  ) / -8,  1,  6, -3/             
C               T[ 5,4]T[ 3,1,2]                                        
      DATA Denom_CLBS(2  ,1,4)/         9/                                    
      DATA (CF_CLBS(i,2  ,1,4),i=1  ,4  ) /  1,  1, -3, -3/             
C               T[ 5,4,2]T[ 3,1]                                        
      DATA Denom_CLBS(3  ,1,4)/         9/                                    
      DATA (CF_CLBS(i,3  ,1,4),i=1  ,4  ) /  6, -3, -8,  1/             
C               T[ 3,1]T[ 5,4,2]                                        
      DATA Denom_CLBS(4  ,1,4)/         9/                                    
      DATA (CF_CLBS(i,4  ,1,4),i=1  ,4  ) / -3, -3,  1,  1/             
C               T[ 5,4]T[ 3,1,2]                                        
C  
      DATA Denom_CLBS(1  ,1,5)/        18/                                    
      DATA (CF_CLBS(i,1  ,1,5),i=1  ,4  ) /  8, -1, -2,  7/             
C               T[ 5,4]T[ 3,1,2]                                        
      DATA Denom_CLBS(2  ,1,5)/        18/                                    
      DATA (CF_CLBS(i,2  ,1,5),i=1  ,4  ) / -1,  8,  7, -2/             
C               T[ 5,4,2]T[ 3,1]                                        
      DATA Denom_CLBS(3  ,1,5)/        18/                                    
      DATA (CF_CLBS(i,3  ,1,5),i=1  ,4  ) / -2,  7, -1,-10/             
C               T[ 3,1]T[ 5,4,2]                                        
      DATA Denom_CLBS(4  ,1,5)/        18/                                    
      DATA (CF_CLBS(i,4  ,1,5),i=1  ,4  ) /  7, -2,-10, -1/             
C               T[ 5,4]T[ 3,1,2]                                        
C  
      DATA Denom_CLBS(1  ,2,3)/         2/                                    
      DATA (CF_CLBS(i,1  ,2,3),i=1  ,4  ) / -8,  1,  2, -7/             
C               T[ 5,4]T[ 3,1,2]                                        
      DATA Denom_CLBS(2  ,2,3)/         2/                                    
      DATA (CF_CLBS(i,2  ,2,3),i=1  ,4  ) /  1,  1,  0,  0/             
C               T[ 5,4,2]T[ 3,1]                                        
      DATA Denom_CLBS(3  ,2,3)/         1/                                    
      DATA (CF_CLBS(i,3  ,2,3),i=1  ,4  ) /  1,  0, -1,  0/             
C               T[ 3,1]T[ 5,4,2]                                        
      DATA Denom_CLBS(4  ,2,3)/         2/                                    
      DATA (CF_CLBS(i,4  ,2,3),i=1  ,4  ) / -7,  0,  0, -7/             
C               T[ 5,4]T[ 3,1,2]                                        
C  
      DATA Denom_CLBS(1  ,2,4)/         2/                                    
      DATA (CF_CLBS(i,1  ,2,4),i=1  ,4  ) /  1,  1,  0,  0/             
C               T[ 5,4]T[ 3,1,2]                                        
      DATA Denom_CLBS(2  ,2,4)/         2/                                    
      DATA (CF_CLBS(i,2  ,2,4),i=1  ,4  ) /  1, -8, -7,  2/             
C               T[ 5,4,2]T[ 3,1]                                        
      DATA Denom_CLBS(3  ,2,4)/         2/                                    
      DATA (CF_CLBS(i,3  ,2,4),i=1  ,4  ) /  0, -7, -7,  0/             
C               T[ 3,1]T[ 5,4,2]                                        
      DATA Denom_CLBS(4  ,2,4)/         1/                                    
      DATA (CF_CLBS(i,4  ,2,4),i=1  ,4  ) /  0,  1,  0, -1/             
C               T[ 5,4]T[ 3,1,2]                                        
C  
      DATA Denom_CLBS(1  ,2,5)/         1/                                    
      DATA (CF_CLBS(i,1  ,2,5),i=1  ,4  ) / -1,  0,  1,  0/             
C               T[ 5,4]T[ 3,1,2]                                        
      DATA Denom_CLBS(2  ,2,5)/         2/                                    
      DATA (CF_CLBS(i,2  ,2,5),i=1  ,4  ) /  0, -7, -7,  0/             
C               T[ 5,4,2]T[ 3,1]                                        
      DATA Denom_CLBS(3  ,2,5)/         2/                                    
      DATA (CF_CLBS(i,3  ,2,5),i=1  ,4  ) /  2, -7, -8,  1/             
C               T[ 3,1]T[ 5,4,2]                                        
      DATA Denom_CLBS(4  ,2,5)/         2/                                    
      DATA (CF_CLBS(i,4  ,2,5),i=1  ,4  ) /  0,  0,  1,  1/             
C               T[ 5,4]T[ 3,1,2]                                        
C  
      DATA Denom_CLBS(1  ,3,4)/        18/                                    
      DATA (CF_CLBS(i,1  ,3,4),i=1  ,4  ) / -1,-10, -2,  7/             
C               T[ 5,4]T[ 3,1,2]                                        
      DATA Denom_CLBS(2  ,3,4)/        18/                                    
      DATA (CF_CLBS(i,2  ,3,4),i=1  ,4  ) /-10, -1,  7, -2/             
C               T[ 5,4,2]T[ 3,1]                                        
      DATA Denom_CLBS(3  ,3,4)/        18/                                    
      DATA (CF_CLBS(i,3  ,3,4),i=1  ,4  ) / -2,  7,  8, -1/             
C               T[ 3,1]T[ 5,4,2]                                        
      DATA Denom_CLBS(4  ,3,4)/        18/                                    
      DATA (CF_CLBS(i,4  ,3,4),i=1  ,4  ) /  7, -2, -1,  8/             
C               T[ 5,4]T[ 3,1,2]                                        
C  
      DATA Denom_CLBS(1  ,3,5)/         9/                                    
      DATA (CF_CLBS(i,1  ,3,5),i=1  ,4  ) /  1,  1, -3, -3/             
C               T[ 5,4]T[ 3,1,2]                                        
      DATA Denom_CLBS(2  ,3,5)/         9/                                    
      DATA (CF_CLBS(i,2  ,3,5),i=1  ,4  ) /  1, -8, -3,  6/             
C               T[ 5,4,2]T[ 3,1]                                        
      DATA Denom_CLBS(3  ,3,5)/         9/                                    
      DATA (CF_CLBS(i,3  ,3,5),i=1  ,4  ) / -3, -3,  1,  1/             
C               T[ 3,1]T[ 5,4,2]                                        
      DATA Denom_CLBS(4  ,3,5)/         9/                                    
      DATA (CF_CLBS(i,4  ,3,5),i=1  ,4  ) / -3,  6,  1, -8/             
C               T[ 5,4]T[ 3,1,2]                                        
C  
      DATA Denom_CLBS(1  ,4,5)/        18/                                    
      DATA (CF_CLBS(i,1  ,4,5),i=1  ,4  ) /-56,  7,  6,-57/             
C               T[ 5,4]T[ 3,1,2]                                        
      DATA Denom_CLBS(2  ,4,5)/        18/                                    
      DATA (CF_CLBS(i,2  ,4,5),i=1  ,4  ) /  7,  7,  6,  6/             
C               T[ 5,4,2]T[ 3,1]                                        
      DATA Denom_CLBS(3  ,4,5)/        18/                                    
      DATA (CF_CLBS(i,3  ,4,5),i=1  ,4  ) /  6,  6,  7,  7/             
C               T[ 3,1]T[ 5,4,2]                                        
      DATA Denom_CLBS(4  ,4,5)/        18/                                    
      DATA (CF_CLBS(i,4  ,4,5),i=1  ,4  ) /-57,  6,  7,-56/             
C               T[ 5,4]T[ 3,1,2]                                        
C  
C  
      DATA Denom(1  )/            3/                                       
      DATA (CF(i,1  ),i=1  ,4  ) /     8,   -1,   -2,    7/                
C               T[ 5, 4]T[ 3, 1, 2]                                        
      DATA Denom(2  )/            3/                                       
      DATA (CF(i,2  ),i=1  ,4  ) /    -1,    8,    7,   -2/                
C               T[ 5, 4, 2]T[ 3, 1]                                        
      DATA Denom(3  )/            3/                                       
      DATA (CF(i,3  ),i=1  ,4  ) /    -2,    7,    8,   -1/                
C               T[ 3, 1]T[ 5, 4, 2]                                        
      DATA Denom(4  )/            3/                                       
      DATA (CF(i,4  ),i=1  ,4  ) /     7,   -2,   -1,    8/                
C               T[ 5, 4]T[ 3, 1, 2]                                        
C ----------
C
C  MODIFIED TO INTERFACE TO POWHEG
C  VARIABLES NEEDED TO EVALUATE COLOR AND 
C  SPIN-CORRELATED BORN AMPLITUDES
C 
      integer II,jjj,kkk
      integer bflav(NEXTERNAL_BORN)
      DATA (bflav(i),i=1,NEXTERNAL_BORN) / 2, 0, 6, -6, 2 /
      complex * 16 ztemp2,amphel(NCOMB,NCOLOR)
      real * 8 amp2jk(NEXTERNAL_BORN,NEXTERNAL_BORN) 
      integer helindex,helcjn
      real * 8 cDENOM(NCOLOR), cCF(NCOLOR,NCOLOR)
      common/to_POWHEG_ug_ttxu/amp2jk,cCF,cDENOM,amphel,helindex
C          
C BEGIN CODE
C ----------
      CALL IXXXXX(P(0,1   ),ZERO ,NHEL(1   ),+1*IC(1   ),W(1,1   ))        
      CALL VXXXXX(P(0,2   ),ZERO ,NHEL(2   ),-1*IC(2   ),W(1,2   ))        
      CALL OXXXXX(P(0,3   ),TMASS ,NHEL(3   ),+1*IC(3   ),W(1,3   ))       
      CALL IXXXXX(P(0,4   ),TMASS ,NHEL(4   ),-1*IC(4   ),W(1,4   ))       
      CALL OXXXXX(P(0,5   ),ZERO ,NHEL(5   ),+1*IC(5   ),W(1,5   ))        
      CALL FVOXXX(W(1,3   ),W(1,2   ),GG ,TMASS   ,TWIDTH  ,W(1,6   ))     
      CALL JIOXXX(W(1,4   ),W(1,6   ),GG ,ZERO    ,ZERO    ,W(1,7   ))     
      CALL IOVXXX(W(1,1   ),W(1,5   ),W(1,7   ),GG ,AMP(1   ))             
      CALL FVIXXX(W(1,4   ),W(1,2   ),GG ,TMASS   ,TWIDTH  ,W(1,8   ))     
      CALL JIOXXX(W(1,1   ),W(1,5   ),GG ,ZERO    ,ZERO    ,W(1,9   ))     
      CALL IOVXXX(W(1,8   ),W(1,3   ),W(1,9   ),GG ,AMP(2   ))             
      CALL JIOXXX(W(1,4   ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,10  ))     
      CALL VVVXXX(W(1,9   ),W(1,2   ),W(1,10  ),G ,AMP(3   ))              
      CALL FVOXXX(W(1,5   ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,11  ))     
      CALL IOVXXX(W(1,1   ),W(1,11  ),W(1,10  ),GG ,AMP(4   ))             
      CALL FVIXXX(W(1,1   ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,12  ))     
      CALL IOVXXX(W(1,12  ),W(1,5   ),W(1,10  ),GG ,AMP(5   ))             
      JAMP(   1) = -AMP(   1)+AMP(   3)
      JAMP(   2) = -AMP(   2)-AMP(   3)
      JAMP(   3) = -AMP(   4)
      JAMP(   4) = -AMP(   5)
      ug_ttxu = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP = (0.D0,0.D0)
          cDENOM(I)=DENOM(I)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
              cCF(J,I)=CF(J,I)
          ENDDO
          ug_ttxu =ug_ttxu+ZTEMP*DCONJG(JAMP(I))/DENOM(I)
C 
         do jjj=1,NEXTERNAL_BORN
            if(abs(bflav(jjj)).le.6) then
            do kkk=jjj+1,NEXTERNAL_BORN
            if(abs(bflav(kkk)).le.6) then
            ZTEMP2 = (0.D0,0.D0)
	    DO II = 1, NCOLOR
            ZTEMP2 = ZTEMP2 - CF_CLBS(II,I,jjj,kkk)*JAMP(II)
            ENDDO
            amp2jk(jjj,kkk)=amp2jk(jjj,kkk)+ 
     $      ZTEMP2*DCONJG(JAMP(I))/DENOM_CLBS(I,jjj,kkk)
            endif
            enddo
            endif
          enddo
          amphel(helindex,I)=jamp(I)
C    
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       
