      subroutine AAtoWW(Q1,Q2,P,aa)
C  
C Generated by MadGraph II Version 3.0. Updated 02/19/04                
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : a a -> e+ ve mu- vm~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=  13) 
      INTEGER    NWAVEFUNCS
      PARAMETER (NWAVEFUNCS=  25) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 Q1(0:3), Q2(0:3), P(0:3,3:6)
      complex*16 aa(0:3,0:3)
C  
C LOCAL VARIABLES 
C  
      INTEGER I,J, MU, NU
      COMPLEX*16 AMP(NGRAPHS)
      COMPLEX*16 W(6,NWAVEFUNCS),WX1(6),WX2(6)
      INTEGER NHEL(3:6), ihel
      DATA (NHEL(IHEL),IHEL=3,6) / +1,-1,-1,+1/
      Save NHEL
c  lepton spinors and W+- polarization vectors
      include 'tensor.inc'

      logical lswitch
      parameter (lswitch=.true.)
C  
C GLOBAL VARIABLES
C  
      include "coupl.inc"
C                                                                          
C ----------
C BEGIN CODE
C ----------
      if (lswitch) then      ! read in lepton an W states from common block
         do i=1,6
            w(i,3) = wep(i)
            w(i,4) = wve(i)
            w(i,5) = wmu(i)
            w(i,6) = wvm(i)
            w(i,19) = wp(i)
            w(i,20) = wm(i)
         enddo
      endif

      DO MU = 0,3
         DO NU = 0,3
      CALL VCARTX(Q1(0),ZERO ,ZERO, MU,-1,W(1,1   ))        
      CALL VCARTX(Q2(0),ZERO ,ZERO, NU,-1,W(1,2   )) 
      if (.not.lswitch) then
         CALL IXXXXX(P(0,4   ),ZERO ,NHEL(3   ),-1,W(1,3   ))        
         CALL OXXXXX(P(0,3   ),ZERO ,NHEL(4   ),+1,W(1,4   ))        
         CALL OXXXXX(P(0,6   ),ZERO ,NHEL(5   ),+1,W(1,5   ))        
         CALL IXXXXX(P(0,5   ),ZERO ,NHEL(6   ),-1,W(1,6   ))  
      endif
      CALL FVIXXX(W(1,3   ),W(1,2   ),GAL ,ZERO    ,ZERO    ,W(1,7   ))    
      CALL JIOXXX(W(1,7   ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,8   ))    
      CALL JVVXXX(W(1,8   ),W(1,1   ),GWWA ,WMASS   ,WWIDTH  ,W(1,9))
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,9   ),GWF ,AMP(1   ))            
      CALL FVOXXX(W(1,5   ),W(1,1   ),GAL ,ZERO    ,ZERO    ,W(1,10  ))    
      CALL IOVXXX(W(1,6   ),W(1,10  ),W(1,8   ),GWF ,AMP(2   ))            
      CALL FVIXXX(W(1,7   ),W(1,1   ),GAL ,ZERO    ,ZERO    ,W(1,11  ))    
      CALL JIOXXX(W(1,11  ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,12  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,12  ),GWF ,AMP(3   ))            
      CALL FVIXXX(W(1,3   ),W(1,1   ),GAL ,ZERO    ,ZERO    ,W(1,13  ))    
      CALL JIOXXX(W(1,13  ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,14  ))    
      CALL JVVXXX(W(1,14  ),W(1,2   ),GWWA ,WMASS   ,WWIDTH  ,W(1,15))
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,15  ),GWF ,AMP(4   ))            
      CALL FVOXXX(W(1,5   ),W(1,2   ),GAL ,ZERO    ,ZERO    ,W(1,16  ))    
      CALL IOVXXX(W(1,6   ),W(1,16  ),W(1,14  ),GWF ,AMP(5   ))            
      CALL FVIXXX(W(1,13  ),W(1,2   ),GAL ,ZERO    ,ZERO    ,W(1,17  ))    
      CALL JIOXXX(W(1,17  ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,18  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,18  ),GWF ,AMP(6   )) 
      if (.not.lswitch) then
         CALL JIOXXX(W(1,3),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,19))    
         CALL JIOXXX(W(1,6),W(1,5   ),GWF ,WMASS   ,WWIDTH  ,W(1,20)) 
      endif
      CALL JVVXXX(W(1,19  ),W(1,2   ),GWWA ,WMASS   ,WWIDTH  ,W(1,21))
      CALL VVVXXX(W(1,20  ),W(1,21  ),W(1,1   ),GWWA ,AMP(7   ))           
      CALL FVIXXX(W(1,6   ),W(1,19  ),GWF ,ZERO    ,ZERO    ,W(1,22  ))    
      CALL IOVXXX(W(1,22  ),W(1,10  ),W(1,2   ),GAL ,AMP(8   ))            
      CALL JIOXXX(W(1,6   ),W(1,10  ),GWF ,WMASS   ,WWIDTH  ,W(1,23  ))    
      CALL VVVXXX(W(1,23  ),W(1,19  ),W(1,2   ),GWWA ,AMP(9   ))           
      CALL JVVXXX(W(1,19  ),W(1,1   ),GWWA ,WMASS   ,WWIDTH  ,W(1,24))
      CALL VVVXXX(W(1,20  ),W(1,24  ),W(1,2   ),GWWA ,AMP(10  ))           
      CALL IOVXXX(W(1,22  ),W(1,16  ),W(1,1   ),GAL ,AMP(11  ))            
      CALL JIOXXX(W(1,6   ),W(1,16  ),GWF ,WMASS   ,WWIDTH  ,W(1,25  ))    
      CALL VVVXXX(W(1,25  ),W(1,19  ),W(1,1   ),GWWA ,AMP(12  ))           
      CALL W3W3XX(W(1,20  ),W(1,1   ),W(1,19  ),W(1,2   ),GWWA ,GWWA ,     
     &     AMP(13  ))                                                      
      AA(MU,NU)  = -AMP(   1)-AMP(   2)-AMP(   3)-AMP(   4)-AMP(   5)
     &             -AMP(   6)-AMP(   7)-AMP(   8)-AMP(   9)-AMP(  10)
     &             -AMP(  11)-AMP(  12)-AMP(  13)
         enddo
      enddo
      END

      subroutine AZtoWW(Q1,Q2,P,az)
C  
C Generated by MadGraph II Version 3.0. Updated 02/19/04                
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : a z -> e+ ve mu- vm~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=  19) 
      INTEGER    NWAVEFUNCS
      PARAMETER (NWAVEFUNCS=  30) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 Q1(0:3), Q2(0:3), P(0:3,3:6)
      complex*16 az(0:3,0:3)
C  
C LOCAL VARIABLES 
C  
      INTEGER I,J, MU, NU
      COMPLEX*16 AMP(NGRAPHS)
      COMPLEX*16 W(6,NWAVEFUNCS),WX1(6),WX2(6)
      INTEGER NHEL(3:6), ihel
      DATA (NHEL(IHEL),IHEL=3,6) / +1,-1,-1,+1/
      Save NHEL
c  lepton spinors and W+- polarization vectors
      include 'tensor.inc'

      logical lswitch
      parameter (lswitch=.true.)
C  
C GLOBAL VARIABLES
C  
      include "coupl.inc"
C                                                                          
C ----------
C BEGIN CODE
C ----------
      if (lswitch) then      ! read in lepton an W states from common block
         do i=1,6
            w(i,3) = wep(i)
            w(i,4) = wve(i)
            w(i,5) = wmu(i)
            w(i,6) = wvm(i)
            w(i,25) = wp(i)
            w(i,14) = wm(i)
         enddo
      endif

      DO MU = 0,3
         DO NU = 0,3
      CALL VCARTX(Q1(0),ZERO ,ZERO, MU,-1,W(1,1   ))        
      CALL VCARTX(Q2(0),ZMASS ,Zwidth, NU,-1,W(1,2   )) 
      if (.not.lswitch) then
         CALL IXXXXX(P(0,4   ),ZERO ,NHEL(3   ),-1,W(1,3   ))        
         CALL OXXXXX(P(0,3   ),ZERO ,NHEL(4   ),+1,W(1,4   ))        
         CALL OXXXXX(P(0,6   ),ZERO ,NHEL(5   ),+1,W(1,5   ))        
         CALL IXXXXX(P(0,5   ),ZERO ,NHEL(6   ),-1,W(1,6   ))        
      endif
      CALL FVIXXX(W(1,3   ),W(1,2   ),GZL ,ZERO    ,ZERO    ,W(1,7   ))    
      CALL JIOXXX(W(1,7   ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,8   ))    
      CALL JVVXXX(W(1,8   ),W(1,1   ),GWWA ,WMASS   ,WWIDTH  ,W(1,         
     &     9   ))                                                          
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,9   ),GWF ,AMP(1   ))            
      CALL FVOXXX(W(1,5   ),W(1,1   ),GAL ,ZERO    ,ZERO    ,W(1,10  ))    
      CALL IOVXXX(W(1,6   ),W(1,10  ),W(1,8   ),GWF ,AMP(2   ))            
      CALL FVIXXX(W(1,7   ),W(1,1   ),GAL ,ZERO    ,ZERO    ,W(1,11  ))    
      CALL JIOXXX(W(1,11  ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,12  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,12  ),GWF ,AMP(3   ))            
      CALL FVOXXX(W(1,4   ),W(1,2   ),GZN ,ZERO    ,ZERO    ,W(1,13  ))    
      if (.not.lswitch)
     &   CALL JIOXXX(W(1,6),W(1,5   ),GWF ,WMASS   ,WWIDTH  ,W(1,14  ))    
      CALL JIOXXX(W(1,3   ),W(1,13  ),GWF ,WMASS   ,WWIDTH  ,W(1,15  ))    
      CALL VVVXXX(W(1,14  ),W(1,15  ),W(1,1   ),GWWA ,AMP(4   ))           
      CALL JIOXXX(W(1,6   ),W(1,10  ),GWF ,WMASS   ,WWIDTH  ,W(1,16  ))    
      CALL IOVXXX(W(1,3   ),W(1,13  ),W(1,16  ),GWF ,AMP(5   ))            
      CALL FVIXXX(W(1,3   ),W(1,1   ),GAL ,ZERO    ,ZERO    ,W(1,17  ))    
      CALL JIOXXX(W(1,17  ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,18  ))    
      CALL FVOXXX(W(1,5   ),W(1,18  ),GWF ,ZERO    ,ZERO    ,W(1,19  ))    
      CALL IOVXXX(W(1,6   ),W(1,19  ),W(1,2   ),GZN ,AMP(6   ))            
      CALL JVVXXX(W(1,18  ),W(1,2   ),GWWZ ,WMASS   ,WWIDTH  ,W(1,         
     &     20  ))                                                          
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,20  ),GWF ,AMP(7   ))            
      CALL FVOXXX(W(1,5   ),W(1,2   ),GZL ,ZERO    ,ZERO    ,W(1,21  ))    
      CALL IOVXXX(W(1,6   ),W(1,21  ),W(1,18  ),GWF ,AMP(8   ))            
      CALL FVIXXX(W(1,17  ),W(1,2   ),GZL ,ZERO    ,ZERO    ,W(1,22  ))    
      CALL JIOXXX(W(1,22  ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,23  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,23  ),GWF ,AMP(9   ))            
      CALL JIOXXX(W(1,17  ),W(1,13  ),GWF ,WMASS   ,WWIDTH  ,W(1,24  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,24  ),GWF ,AMP(10  ))            
      if (.not.lswitch) 
     &   CALL JIOXXX(W(1,3),W(1,4   ),GWF ,WMASS ,WWIDTH  ,W(1,25))    
      CALL FVIXXX(W(1,6   ),W(1,2   ),GZN ,ZERO    ,ZERO    ,W(1,26  ))    
      CALL IOVXXX(W(1,26  ),W(1,10  ),W(1,25  ),GWF ,AMP(11  ))            
      CALL JVVXXX(W(1,25  ),W(1,2   ),GWWZ ,WMASS   ,WWIDTH  ,W(1,         
     &     27  ))                                                          
      CALL VVVXXX(W(1,14  ),W(1,27  ),W(1,1   ),GWWA ,AMP(12  ))           
      CALL FVIXXX(W(1,6   ),W(1,25  ),GWF ,ZERO    ,ZERO    ,W(1,28  ))    
      CALL IOVXXX(W(1,28  ),W(1,10  ),W(1,2   ),GZL ,AMP(13  ))            
      CALL VVVXXX(W(1,16  ),W(1,25  ),W(1,2   ),GWWZ ,AMP(14  ))           
      CALL JVVXXX(W(1,25  ),W(1,1),GWWA ,WMASS   ,WWIDTH  ,W(1,29) )
      CALL IOVXXX(W(1,26  ),W(1,5   ),W(1,29  ),GWF ,AMP(15  ))            
      CALL VVVXXX(W(1,14  ),W(1,29  ),W(1,2   ),GWWZ ,AMP(16  ))           
      CALL IOVXXX(W(1,28  ),W(1,21  ),W(1,1   ),GAL ,AMP(17  ))            
      CALL JIOXXX(W(1,6   ),W(1,21  ),GWF ,WMASS   ,WWIDTH  ,W(1,30  ))    
      CALL VVVXXX(W(1,30  ),W(1,25  ),W(1,1   ),GWWA ,AMP(18  ))           
      CALL W3W3XX(W(1,14  ),W(1,2   ),W(1,25  ),W(1,1   ),GWWZ ,GWWA ,     
     &     AMP(19  ))                                                      
      AZ(MU,NU)  = -AMP(   1)-AMP(   2)-AMP(   3)-AMP(   4)-AMP(   5)
     &             -AMP(   6)-AMP(   7)-AMP(   8)-AMP(   9)-AMP(  10)
     &             -AMP(  11)-AMP(  12)-AMP(  13)-AMP(  14)-AMP(  15)
     &             -AMP(  16)-AMP(  17)-AMP(  18)-AMP(  19)
         ENDDO
      ENDDO

      END
       
      subroutine ZZtoWW(Q1,Q2,P,zz)
C  
C Generated by MadGraph II Version 3.0. Updated 02/19/04                
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : z z -> e+ ve mu- vm~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=  32) 
      INTEGER    NWAVEFUNCS
      PARAMETER (NWAVEFUNCS=  40) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 Q1(0:3), Q2(0:3), P(0:3,3:6)
      complex*16 zz(0:3,0:3)
C  
C LOCAL VARIABLES 
C  
      INTEGER I,J, MU, NU
      COMPLEX*16 AMP(NGRAPHS)
      COMPLEX*16 W(6,NWAVEFUNCS),WX1(6),WX2(6)
      INTEGER NHEL(3:6), ihel
      DATA (NHEL(IHEL),IHEL=3,6) / +1,-1,-1,+1/
      Save NHEL
c  lepton spinors and W+- polarization vectors
      include 'tensor.inc'

      logical lswitch
      parameter (lswitch=.true.)
C  
C GLOBAL VARIABLES
C  
      include "coupl.inc"
C                                                                          
C ----------
C BEGIN CODE
C ----------
      if (lswitch) then      ! read in lepton an W states from common block
         do i=1,6
            w(i,3) = wep(i)
            w(i,4) = wve(i)
            w(i,5) = wmu(i)
            w(i,6) = wvm(i)
            w(i,34) = wp(i)
            w(i,15) = wm(i)
         enddo
      endif

      DO MU = 0,3
c         print*,' mu = ',mu,':',(q1(mu)+q2(mu))/(p(mu,3)+p(mu,4)+p(mu,5)+p(mu,6))
         DO NU = 0,3
      CALL VCARTX(Q1(0),ZMASS ,Zwidth, MU,-1,W(1,1   ))        
      CALL VCARTX(Q2(0),ZMASS ,Zwidth, NU,-1,W(1,2   ))
      if (.not.lswitch) then 
         CALL IXXXXX(P(0,4   ),ZERO ,NHEL(3   ),-1,W(1,3   ))        
         CALL OXXXXX(P(0,3   ),ZERO ,NHEL(4   ),+1,W(1,4   ))        
         CALL OXXXXX(P(0,6   ),ZERO ,NHEL(5   ),+1,W(1,5   ))        
         CALL IXXXXX(P(0,5   ),ZERO ,NHEL(6   ),-1,W(1,6   ))
      endif        
      CALL FVIXXX(W(1,3   ),W(1,2   ),GZL ,ZERO    ,ZERO    ,W(1,7   ))    
      CALL JIOXXX(W(1,7   ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,8   ))    
      CALL FVOXXX(W(1,5   ),W(1,8   ),GWF ,ZERO    ,ZERO    ,W(1,9   ))    
      CALL IOVXXX(W(1,6   ),W(1,9   ),W(1,1   ),GZN ,AMP(1   ))            
      CALL JVVXXX(W(1,8   ),W(1,1   ),GWWZ ,WMASS   ,WWIDTH  ,W(1,10))
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,10  ),GWF ,AMP(2   ))            
      CALL FVOXXX(W(1,5   ),W(1,1   ),GZL ,ZERO    ,ZERO    ,W(1,11  ))    
      CALL IOVXXX(W(1,6   ),W(1,11  ),W(1,8   ),GWF ,AMP(3   ))            
      CALL FVOXXX(W(1,4   ),W(1,1   ),GZN ,ZERO    ,ZERO    ,W(1,12  ))    
      CALL FVIXXX(W(1,6   ),W(1,2   ),GZN ,ZERO    ,ZERO    ,W(1,13  ))    
      CALL JIOXXX(W(1,3   ),W(1,12  ),GWF ,WMASS   ,WWIDTH  ,W(1,14  ))    
      CALL IOVXXX(W(1,13  ),W(1,5   ),W(1,14  ),GWF ,AMP(4   ))            
      if (.not.lswitch) then
         CALL JIOXXX(W(1,6),W(1,5),GWF ,WMASS   ,WWIDTH  ,W(1,15)) 
      endif   
      CALL VVVXXX(W(1,15  ),W(1,14  ),W(1,2   ),GWWZ ,AMP(5   ))           
      CALL FVOXXX(W(1,5   ),W(1,2   ),GZL ,ZERO    ,ZERO    ,W(1,16  ))    
      CALL JIOXXX(W(1,6   ),W(1,16  ),GWF ,WMASS   ,WWIDTH  ,W(1,17  ))    
      CALL IOVXXX(W(1,3   ),W(1,12  ),W(1,17  ),GWF ,AMP(6   ))            
      CALL FVOXXX(W(1,12  ),W(1,2   ),GZN ,ZERO    ,ZERO    ,W(1,18  ))    
      CALL IOVXXX(W(1,3   ),W(1,18  ),W(1,15  ),GWF ,AMP(7   ))            
      CALL FVIXXX(W(1,7   ),W(1,1   ),GZL ,ZERO    ,ZERO    ,W(1,19  ))    
      CALL JIOXXX(W(1,19  ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,20  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,20  ),GWF ,AMP(8   ))            
      CALL JIOXXX(W(1,7   ),W(1,12  ),GWF ,WMASS   ,WWIDTH  ,W(1,21  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,21  ),GWF ,AMP(9   ))            
      CALL FVOXXX(W(1,4   ),W(1,2   ),GZN ,ZERO    ,ZERO    ,W(1,22  ))    
      CALL FVIXXX(W(1,6   ),W(1,1   ),GZN ,ZERO    ,ZERO    ,W(1,23  ))    
      CALL JIOXXX(W(1,3   ),W(1,22  ),GWF ,WMASS   ,WWIDTH  ,W(1,24  ))    
      CALL IOVXXX(W(1,23  ),W(1,5   ),W(1,24  ),GWF ,AMP(10  ))            
      CALL VVVXXX(W(1,15  ),W(1,24  ),W(1,1   ),GWWZ ,AMP(11  ))           
      CALL JIOXXX(W(1,6   ),W(1,11  ),GWF ,WMASS   ,WWIDTH  ,W(1,25  ))    
      CALL IOVXXX(W(1,3   ),W(1,22  ),W(1,25  ),GWF ,AMP(12  ))            
      CALL FVIXXX(W(1,3   ),W(1,1   ),GZL ,ZERO    ,ZERO    ,W(1,26  ))    
      CALL JIOXXX(W(1,26  ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,27  ))    
      CALL FVOXXX(W(1,5   ),W(1,27  ),GWF ,ZERO    ,ZERO    ,W(1,28  ))    
      CALL IOVXXX(W(1,6   ),W(1,28  ),W(1,2   ),GZN ,AMP(13  ))            
      CALL JVVXXX(W(1,27  ),W(1,2   ),GWWZ ,WMASS   ,WWIDTH  ,W(1,29))
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,29  ),GWF ,AMP(14  ))            
      CALL IOVXXX(W(1,6   ),W(1,16  ),W(1,27  ),GWF ,AMP(15  ))            
      CALL FVOXXX(W(1,22  ),W(1,1   ),GZN ,ZERO    ,ZERO    ,W(1,30  ))    
      CALL IOVXXX(W(1,3   ),W(1,30  ),W(1,15  ),GWF ,AMP(16  ))            
      CALL FVIXXX(W(1,26  ),W(1,2   ),GZL ,ZERO    ,ZERO    ,W(1,31  ))    
      CALL JIOXXX(W(1,31  ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,32  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,32  ),GWF ,AMP(17  ))            
      CALL JIOXXX(W(1,26  ),W(1,22  ),GWF ,WMASS   ,WWIDTH  ,W(1,33  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,33  ),GWF ,AMP(18  ))            
      if (.not.lswitch)
     &   CALL JIOXXX(W(1,3),W(1,4),GWF ,WMASS,WWIDTH  ,W(1,34))    
      CALL JVVXXX(W(1,34),W(1,2),GWWZ ,WMASS ,WWIDTH  ,W(1,35))
      CALL IOVXXX(W(1,23  ),W(1,5   ),W(1,35  ),GWF ,AMP(19  ))            
      CALL IOVXXX(W(1,13  ),W(1,11  ),W(1,34  ),GWF ,AMP(20  ))            
      CALL VVVXXX(W(1,15  ),W(1,35  ),W(1,1   ),GWWZ ,AMP(21  ))           
      CALL FVIXXX(W(1,6   ),W(1,34  ),GWF ,ZERO    ,ZERO    ,W(1,36  ))    
      CALL IOVXXX(W(1,36  ),W(1,11  ),W(1,2   ),GZL ,AMP(22  ))            
      CALL VVVXXX(W(1,25  ),W(1,34  ),W(1,2   ),GWWZ ,AMP(23  ))           
      CALL IOVXXX(W(1,23  ),W(1,16  ),W(1,34  ),GWF ,AMP(24  ))            
      CALL JVVXXX(W(1,34  ),W(1,1   ),GWWZ ,WMASS   ,WWIDTH  ,W(1,37))
      CALL IOVXXX(W(1,13  ),W(1,5   ),W(1,37  ),GWF ,AMP(25  ))            
      CALL VVVXXX(W(1,15  ),W(1,37  ),W(1,2   ),GWWZ ,AMP(26  ))           
      CALL IOVXXX(W(1,36  ),W(1,16  ),W(1,1   ),GZL ,AMP(27  ))            
      CALL VVVXXX(W(1,17  ),W(1,34  ),W(1,1   ),GWWZ ,AMP(28  ))           
      CALL FVOXXX(W(1,5   ),W(1,34  ),GWF ,ZERO    ,ZERO    ,W(1,38  ))    
      CALL IOVXXX(W(1,23  ),W(1,38  ),W(1,2   ),GZN ,AMP(29  ))            
      CALL IOVXXX(W(1,13  ),W(1,38  ),W(1,1   ),GZN ,AMP(30  ))            
      CALL HVVXXX(W(1,1   ),W(1,2   ),GZZH ,HMASS   ,HWIDTH  ,W(1,39))
      CALL JVSXXX(W(1,34  ),W(1,39  ),GWWH ,WMASS   ,WWIDTH  ,W(1,40))
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,40  ),GWF ,AMP(31  ))            
      CALL W3W3XX(W(1,15  ),W(1,1   ),W(1,34  ),W(1,2   ),GWWZ ,GWWZ ,     
     &     AMP(32  ))                                                      
      ZZ(MU,NU)  = -AMP(   1)-AMP(   2)-AMP(   3)-AMP(   4)-AMP(   5)
     &             -AMP(   6)-AMP(   7)-AMP(   8)-AMP(   9)-AMP(  10)
     &             -AMP(  11)-AMP(  12)-AMP(  13)-AMP(  14)-AMP(  15)
     &             -AMP(  16)-AMP(  17)-AMP(  18)-AMP(  19)-AMP(  20)
     &             -AMP(  21)-AMP(  22)-AMP(  23)-AMP(  24)-AMP(  25)
     &             -AMP(  26)-AMP(  27)-AMP(  28)-AMP(  29)-AMP(  30)
     &             -AMP(  31)-AMP(  32)
          ENDDO
      ENDDO

      END




      subroutine WWtoWW(Q1,Q2,P,ww)
C  
C Generated by MadGraph II Version 3.0. Updated 02/19/04                
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : w- w+ -> e+ ve mu- vm~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=  28) 
      INTEGER    NWAVEFUNCS
      PARAMETER (NWAVEFUNCS=  40)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 Q1(0:3), Q2(0:3), P(0:3,3:6)
      complex*16 ww(0:3,0:3)
C  
C LOCAL VARIABLES 
C  
      INTEGER I,J, MU, NU
      COMPLEX*16 AMP(NGRAPHS)
      COMPLEX*16 W(6,NWAVEFUNCS)
      INTEGER NHEL(3:6), ihel
      DATA (NHEL(IHEL),IHEL=3,6) / +1,-1,-1,+1/
      Save NHEL
c  lepton spinors and W+- polarization vectors
      include 'tensor.inc'

      logical lswitch
      parameter (lswitch=.true.)
C  
C GLOBAL VARIABLES
C  
      include "coupl.inc"
C                                                                          
C ----------
C BEGIN CODE
C ----------
      if (lswitch) then      ! read in lepton an W states from common block
         do i=1,6
            w(i,3) = wep(i)
            w(i,4) = wve(i)
            w(i,5) = wmu(i)
            w(i,6) = wvm(i)
            w(i,21) = wp(i)
            w(i,18) = wm(i)
         enddo
      endif

      DO MU = 0,3
         DO NU = 0,3
      CALL VCARTX(Q1(0),wMASS ,wwidth, MU,-1,W(1,1   ))        
      CALL VCARTX(Q2(0),wMASS ,wwidth, NU,-1,W(1,2   )) 
      if (.not.lswitch) then
         CALL IXXXXX(P(0,4   ),ZERO ,NHEL(3   ),-1,W(1,3   ))        
         CALL OXXXXX(P(0,3   ),ZERO ,NHEL(4   ),+1,W(1,4   ))        
         CALL OXXXXX(P(0,6   ),ZERO ,NHEL(5   ),+1,W(1,5   ))        
         CALL IXXXXX(P(0,5   ),ZERO ,NHEL(6   ),-1,W(1,6   ))  
      endif      
      CALL FVIXXX(W(1,3   ),W(1,2   ),GWF ,ZERO    ,ZERO    ,W(1,7   ))    
      CALL JIOXXX(W(1,7   ),W(1,4   ),GZN ,ZMASS   ,ZWIDTH  ,W(1,8   ))    
      CALL FVOXXX(W(1,5   ),W(1,8   ),GZL ,ZERO    ,ZERO    ,W(1,9   ))    
      CALL IOVXXX(W(1,6   ),W(1,9   ),W(1,1   ),GWF ,AMP(1   ))            
      CALL JVVXXX(W(1,1   ),W(1,8   ),GWWZ ,WMASS   ,WWIDTH  ,W(1,10))
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,10  ),GWF ,AMP(2   ))            
      CALL FVOXXX(W(1,5   ),W(1,1   ),GWF ,ZERO    ,ZERO    ,W(1,11  ))    
      CALL IOVXXX(W(1,6   ),W(1,11  ),W(1,8   ),GZN ,AMP(3   ))            
      CALL FVIXXX(W(1,7   ),W(1,1   ),GWF ,ZERO    ,ZERO    ,W(1,12  ))    
      CALL JIOXXX(W(1,12  ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,13  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,13  ),GWF ,AMP(4   ))            
      CALL FVOXXX(W(1,4   ),W(1,2   ),GWF ,ZERO    ,ZERO    ,W(1,14  ))    
      CALL FVIXXX(W(1,6   ),W(1,1   ),GWF ,ZERO    ,ZERO    ,W(1,15  ))    
      CALL JIOXXX(W(1,3   ),W(1,14  ),GAL ,ZERO    ,ZERO    ,W(1,16  ))    
      CALL IOVXXX(W(1,15  ),W(1,5   ),W(1,16  ),GAL ,AMP(5   ))            
      CALL JIOXXX(W(1,3   ),W(1,14  ),GZL ,ZMASS   ,ZWIDTH  ,W(1,17  ))    
      CALL IOVXXX(W(1,15  ),W(1,5   ),W(1,17  ),GZL ,AMP(6   ))      
      if (.not.lswitch) 
     &   CALL JIOXXX( W(1,6),W(1,5),GWF ,WMASS,WWIDTH,W(1,18) )    
      CALL VVVXXX(W(1,18  ),W(1,1   ),W(1,16  ),GWWA ,AMP(7   ))           
      CALL VVVXXX(W(1,18  ),W(1,1   ),W(1,17  ),GWWZ ,AMP(8   ))           
      CALL JIOXXX(W(1,6   ),W(1,11  ),GZN ,ZMASS   ,ZWIDTH  ,W(1,19  ))    
      CALL IOVXXX(W(1,3   ),W(1,14  ),W(1,19  ),GZL ,AMP(9   ))            
      CALL FVOXXX(W(1,14  ),W(1,1   ),GWF ,ZERO    ,ZERO    ,W(1,20  ))    
      CALL IOVXXX(W(1,3   ),W(1,20  ),W(1,18  ),GWF ,AMP(10  ))  
      if (.not.lswitch)
     &   CALL JIOXXX(W(1,3),W(1,4),GWF ,WMASS,WWIDTH,W(1,21))    
      CALL JVVXXX(W(1,2   ),W(1,21  ),GWWA ,ZERO    ,ZERO    ,W(1,22))
      CALL IOVXXX(W(1,15  ),W(1,5   ),W(1,22  ),GAL ,AMP(11  ))            
      CALL JVVXXX(W(1,2   ),W(1,21  ),GWWZ ,ZMASS   ,ZWIDTH  ,W(1,23))
      CALL IOVXXX(W(1,15  ),W(1,5   ),W(1,23  ),GZL ,AMP(12  ))            
      CALL VVVXXX(W(1,18  ),W(1,1   ),W(1,22  ),GWWA ,AMP(13  ))           
      CALL VVVXXX(W(1,18  ),W(1,1   ),W(1,23  ),GWWZ ,AMP(14  ))           
      CALL HVVXXX(W(1,2   ),W(1,21  ),GWWH ,HMASS   ,HWIDTH  ,W(1,24))
      CALL VVSXXX(W(1,18  ),W(1,1   ),W(1,24  ),GWWH ,AMP(15  ))           
      CALL FVIXXX(W(1,6   ),W(1,21  ),GWF ,ZERO    ,ZERO    ,W(1,25  ))    
      CALL IOVXXX(W(1,25  ),W(1,11  ),W(1,2   ),GWF ,AMP(16  ))            
      CALL VVVXXX(W(1,2   ),W(1,21  ),W(1,19  ),GWWZ ,AMP(17  ))           
      CALL JVVXXX(W(1,2   ),W(1,1   ),GWWZ ,ZMASS   ,ZWIDTH  ,W(1,26))
      CALL FVOXXX(W(1,4   ),W(1,26  ),GZN ,ZERO    ,ZERO    ,W(1,27  ))    
      CALL JIOXXX(W(1,3   ),W(1,27  ),GWF ,WMASS   ,WWIDTH  ,W(1,28  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,28  ),GWF ,AMP(18  ))            
      CALL JVVXXX(W(1,2   ),W(1,1   ),GWWA ,ZERO    ,ZERO    ,W(1,29))
      CALL FVIXXX(W(1,3   ),W(1,29  ),GAL ,ZERO    ,ZERO    ,W(1,30  ))    
      CALL JIOXXX(W(1,30  ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,31  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,31  ),GWF ,AMP(19  ))            
      CALL FVIXXX(W(1,3   ),W(1,26  ),GZL ,ZERO    ,ZERO    ,W(1,32  ))    
      CALL JIOXXX(W(1,32  ),W(1,4   ),GWF ,WMASS   ,WWIDTH  ,W(1,33  ))    
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,33  ),GWF ,AMP(20  ))            
      CALL FVOXXX(W(1,5   ),W(1,21  ),GWF ,ZERO    ,ZERO    ,W(1,34  ))    
      CALL IOVXXX(W(1,15  ),W(1,34  ),W(1,2   ),GWF ,AMP(21  ))            
      CALL JVVXXX(W(1,21  ),W(1,29  ),GWWA ,WMASS   ,WWIDTH  ,W(1,35))
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,35  ),GWF ,AMP(22  ))            
      CALL JVVXXX(W(1,21  ),W(1,26  ),GWWZ ,WMASS   ,WWIDTH  ,W(1,36))
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,36  ),GWF ,AMP(23  ))            
      CALL HVVXXX(W(1,2   ),W(1,1   ),GWWH ,HMASS   ,HWIDTH  ,W(1,37))
      CALL JVSXXX(W(1,21  ),W(1,37  ),GWWH ,WMASS   ,WWIDTH  ,W(1,38))
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,38  ),GWF ,AMP(24  ))            
      CALL FVOXXX(W(1,5   ),W(1,29  ),GAL ,ZERO    ,ZERO    ,W(1,39  ))    
      CALL IOVXXX(W(1,6   ),W(1,39  ),W(1,21  ),GWF ,AMP(25  ))            
      CALL FVOXXX(W(1,5   ),W(1,26  ),GZL ,ZERO    ,ZERO    ,W(1,40  ))    
      CALL IOVXXX(W(1,6   ),W(1,40  ),W(1,21  ),GWF ,AMP(26  ))            
      CALL IOVXXX(W(1,6   ),W(1,34  ),W(1,26  ),GZN ,AMP(27  ))            
      CALL WWWWXX(W(1,2   ),W(1,1   ),W(1,18  ),W(1,21  ),GWWZ ,GWWA ,     
     &     AMP(28  ))                                                      
      WW(MU,NU)  = -AMP(   1)-AMP(   2)-AMP(   3)-AMP(   4)-AMP(   5)
     &             -AMP(   6)-AMP(   7)-AMP(   8)-AMP(   9)-AMP(  10)
     &             -AMP(  11)-AMP(  12)-AMP(  13)-AMP(  14)-AMP(  15)
     &             -AMP(  16)-AMP(  17)-AMP(  18)-AMP(  19)-AMP(  20)
     &             -AMP(  21)-AMP(  22)-AMP(  23)-AMP(  24)-AMP(  25)
     &             -AMP(  26)-AMP(  27)-AMP(  28)
          ENDDO
      ENDDO

      END

      subroutine ZtoWW(P,z)
C  
C Generated by MadGraph II Version 3.0. Updated 02/19/04                
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : z -> e+ ve mu- vm~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=   5) 
      INTEGER    NWAVEFUNCS
      PARAMETER (NWAVEFUNCS=  13) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 Q(0:3), P(0:3,2:5)
      complex*16 z(0:5)
C  
C LOCAL VARIABLES 
C  
      INTEGER I,J, mu
      COMPLEX*16 AMP(NGRAPHS)
      COMPLEX*16 W(6,NWAVEFUNCS)
      INTEGER NHEL(2:5), ihel
      DATA (NHEL(IHEL),IHEL=2,5) / +1,-1,-1,+1/
      Save NHEL
c  lepton spinors and W+- polarization vectors
      include 'tensor.inc'

      logical lswitch
      parameter (lswitch=.true.)
C  
C GLOBAL VARIABLES
C  
      include "coupl.inc"
C                                                                          
C ----------
C BEGIN CODE
C ----------
      if (lswitch) then      ! read in lepton an W states from common block
         do i=1,6
            w(i,2) = wep(i)
            w(i,3) = wve(i)
            w(i,4) = wmu(i)
            w(i,5) = wvm(i)
            w(i,6) = wp(i)
c            w(i,) = wm(i)
         enddo
      endif

      do mu = 0,3
         q(mu) = p(mu,2)+p(mu,3)+p(mu,4)+p(mu,5)
      enddo
c
      if (.not.lswitch) then
         CALL IXXXXX(P(0,3   ),ZERO ,NHEL(2   ),-1,W(1,2   ))
         CALL OXXXXX(P(0,2   ),ZERO ,NHEL(3   ),+1,W(1,3   ))        
         CALL OXXXXX(P(0,5   ),ZERO ,NHEL(4   ),+1,W(1,4   ))        
         CALL IXXXXX(P(0,4   ),ZERO ,NHEL(5   ),-1,W(1,5   ))        
         CALL JIOXXX(W(1,2   ),W(1,3),GWF ,WMASS,WWIDTH,W(1,6))  
      endif  
      do mu = 0,3
      CALL VCARTX(Q(0     ),ZMASS ,Zwidth,MU,-1,W(1,1   )) 
      CALL FVOXXX(W(1,4   ),W(1,6   ),GWF ,ZERO    ,ZERO    ,W(1,7   ))    
      CALL IOVXXX(W(1,5   ),W(1,7   ),W(1,1   ),GZN ,AMP(1   ))            
      CALL JVVXXX(W(1,6   ),W(1,1   ),GWWZ ,WMASS   ,WWIDTH  ,W(1,8)) 
      CALL IOVXXX(W(1,5   ),W(1,4   ),W(1,8   ),GWF ,AMP(2   ))            
      CALL FVOXXX(W(1,4   ),W(1,1   ),GZL ,ZERO    ,ZERO    ,W(1,9   ))    
      CALL IOVXXX(W(1,5   ),W(1,9   ),W(1,6   ),GWF ,AMP(3   ))            
      CALL FVOXXX(W(1,3   ),W(1,1   ),GZN ,ZERO    ,ZERO    ,W(1,10  ))    
      CALL JIOXXX(W(1,2   ),W(1,10  ),GWF ,WMASS   ,WWIDTH  ,W(1,11  ))    
      CALL IOVXXX(W(1,5   ),W(1,4   ),W(1,11  ),GWF ,AMP(4   ))            
      CALL FVIXXX(W(1,2   ),W(1,1   ),GZL ,ZERO    ,ZERO    ,W(1,12  ))    
      CALL JIOXXX(W(1,12  ),W(1,3   ),GWF ,WMASS   ,WWIDTH  ,W(1,13  ))    
      CALL IOVXXX(W(1,5   ),W(1,4   ),W(1,13  ),GWF ,AMP(5   ))            
      Z(mu) = -AMP(   1)-AMP(   2)-AMP(   3)-AMP(   4)-AMP(   5)
      enddo
      z(4) = dcmplx(q(0),q(3))
      z(5) = dcmplx(q(1),q(2))
c
      return
      END

      subroutine AtoWW(P,a)
C  
C Generated by MadGraph II Version 3.0. Updated 02/19/04                
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : a -> e+ ve mu- vm~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=   3) 
      INTEGER    NWAVEFUNCS
      PARAMETER (NWAVEFUNCS=  10) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 Q(0:3), P(0:3,2:5)
      complex*16 a(0:5)
C  
C LOCAL VARIABLES 
C
      INTEGER I,J, mu
      COMPLEX*16 AMP(NGRAPHS)
      COMPLEX*16 W(6,NWAVEFUNCS)
      INTEGER NHEL(2:5), ihel
      DATA (NHEL(IHEL),IHEL=2,5) /+1,-1,-1,+1/
      Save NHEL
c  lepton spinors and W+- polarization vectors
      include 'tensor.inc'

      logical lswitch
      parameter (lswitch=.true.)
C  
C GLOBAL VARIABLES
C  
      include "coupl.inc"
C                                                                          
C ----------
C BEGIN CODE
C ----------
      if (lswitch) then      ! read in lepton an W states from common block
         do i=1,6
            w(i,2) = wep(i)
            w(i,3) = wve(i)
            w(i,4) = wmu(i)
            w(i,5) = wvm(i)
            w(i,6) = wp(i)
c            w(i,) = wm(i)
         enddo
      endif

      do mu = 0,3
         q(mu) = p(mu,2)+p(mu,3)+p(mu,4)+p(mu,5)
      enddo
c
      if (.not.lswitch) then
         CALL IXXXXX(P(0,3   ),ZERO ,NHEL(2   ),-1,W(1,2   ))        
         CALL OXXXXX(P(0,2   ),ZERO ,NHEL(3   ),+1,W(1,3   ))        
         CALL OXXXXX(P(0,5   ),ZERO ,NHEL(4   ),+1,W(1,4   ))        
         CALL IXXXXX(P(0,4   ),ZERO ,NHEL(5   ),-1,W(1,5   ))        
         CALL JIOXXX(W(1,2   ),W(1,3),GWF ,WMASS,WWIDTH,W(1,6))
      endif
      do mu = 0,3
      CALL VCARTX(Q(0),ZERO ,ZERO, MU,-1,W(1,1   ))
      CALL JVVXXX(W(1,6   ),W(1,1   ),GWWA ,WMASS   ,WWIDTH  ,W(1,7))         
      CALL IOVXXX(W(1,5   ),W(1,4   ),W(1,7   ),GWF ,AMP(1   ))            
      CALL FVOXXX(W(1,4   ),W(1,1   ),GAL ,ZERO    ,ZERO    ,W(1,8   ))    
      CALL IOVXXX(W(1,5   ),W(1,8   ),W(1,6   ),GWF ,AMP(2   ))            
      CALL FVIXXX(W(1,2   ),W(1,1   ),GAL ,ZERO    ,ZERO    ,W(1,9   ))    
      CALL JIOXXX(W(1,9   ),W(1,3   ),GWF ,WMASS   ,WWIDTH  ,W(1,10  ))    
      CALL IOVXXX(W(1,5   ),W(1,4   ),W(1,10  ),GWF ,AMP(3   ))            
      a(mu) = -AMP(   1)-AMP(   2)-AMP(   3)
      enddo
      a(4) = dcmplx(q(0),q(3))
      a(5) = dcmplx(q(1),q(2))
c
      END


      subroutine vcartx(p,vmass,vwidth,ncart,nsv , vc)
c
c This subroutine computes an effective VECTOR wavefunction for an internal
c vector boson line. The propagator is inlcuded in the Feynman gauge.
c
c input:
c       real    p(0:3)         : four-momentum of vector boson
c       real    vmass          : mass          of vector boson
c       integer ncart = 0,1,2,3: cartesian polarization direction
c                                of vector boson
c       integer nsv  = -1 or 1 : +1 for final, -1 for initial
c
c output:
c       complex vc(6)          : vector wavefunction       epsilon^mu(v)
c     
      implicit none
      double complex vc(6), d
      double precision p(0:3),vmass,vwidth,q2
      integer ncart, mu, nsv

      q2 = p(0)**2-p(1)**2-p(2)**2-p(3)**2
      if (vmass.eq.0d0) then
         d = 1d0/q2
      else
         d = 1d0/dcmplx( q2-vmass**2, vmass*vwidth )
      endif
      do mu = 0,3
         vc(mu+1) = dcmplx(0d0,0d0)
      enddo
      if (ncart.eq.0) then
         vc(1) = d
      else
         vc(ncart+1) = -d
      endif
c
      vc(5) = dcmplx(p(0),p(3))*nsv
      vc(6) = dcmplx(p(1),p(2))*nsv
c
      return
      end

      subroutine WVtoWM(id,Q,v,wma,wmz)
C  Input:   Q(*)        one of the two incoming boson momenta 
c           id = 1      Q = q1 = W- momentum
c           id = 2      Q = q2 = gamma/Z momentum
c  OUPTPUT: wma(mu,nu)  lepton tensor for w gamma--> mu- nubar
c           wmz(mu,nu)  lepton tensor for w Z    --> mu- nubar
C  
C Generated by MadGraph II Version 3.0. Updated 02/19/04                
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : w- z -> mu- vm~  
C and           w- a -> mu- vm~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=   3) 
      INTEGER    NWAVEFUNCS
      PARAMETER (NWAVEFUNCS=   7)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      integer id
      real*8 q(0:3), v(0:3,4)
      complex*16 wma(0:3,0:3), wmz(0:3,0:3)
C  
C LOCAL VARIABLES 
C  
      INTEGER I,J, MU, NU
      real*8 q2sq, Q1(0:3), Q2(0:3)
      COMPLEX*16 AMP(NGRAPHS)
      COMPLEX*16 W(6,NWAVEFUNCS), denz
c  lepton spinors and W+- polarization vectors
      include 'tensor.inc'

      logical ldebug
      parameter (ldebug=.false.)
C  
C GLOBAL VARIABLES
C  
      include "coupl.inc"
C                                                                          
C ----------
C BEGIN CODE
C ----------
      if (id.eq.1) then
         q2(0) = dreal(wm(5))
         q2(1) = dreal(wm(6))
         q2(2) = dimag(wm(6))
         q2(3) = dimag(wm(5))
         do mu = 0,3
            q1(mu) = q(mu)
            q2(mu) = q2(mu)-q(mu)
         enddo
      elseif (id.eq.2) then
         q1(0) = dreal(wm(5))
         q1(1) = dreal(wm(6))
         q1(2) = dimag(wm(6))
         q1(3) = dimag(wm(5))
         do mu = 0,3
            q2(mu) = q(mu)
            q1(mu) = q1(mu)-q(mu)
         enddo
      endif

      do i=1,6                  ! read in lepton states from common block
         w(i,3) = wmu(i)
         w(i,4) = wvm(i)
      enddo

      q2sq = q2(0)**2-q2(1)**2-q2(2)**2-q2(3)**2
      denz = q2sq/dcmplx(q2sq-zmass**2,zmass*zwidth)
      DO MU = 0,3
         CALL VCARTX(Q1(0),WMASS ,Wwidth, MU,-1,W(1,1))        
         CALL FVOXXX(WMU,W(1,1),GWF ,ZERO,ZERO,W(1,6))    
         DO NU = 0,3
            CALL VCARTX(Q2(0),ZERO  ,ZERO  , NU,-1,W(1,2))
            CALL FVOXXX(WMU,W(1,2),GZL ,ZERO,ZERO,W(1,5))    
            CALL IOVXXX(WVM,W(1,5),W(1,1),GWF ,AMP(1))            
            CALL IOVXXX(WVM,W(1,6),W(1,2),GZN ,AMP(2))            
            CALL JVVXXX(W(1,1),W(1,2),GWWZ ,WMASS,WWIDTH,W(1,7))
            CALL IOVXXX(WVM,WMU,W(1,7),GWF ,AMP(3))            
            wmz(mu,nu) = (-AMP(   1)-AMP(   2)-AMP(   3))*denz
            wma(mu,nu) = -(amp(1)*gal(1)/gzl(1)+amp(3)*gwwa/gwwz)
         enddo
      enddo

      end

      subroutine WVtoWP(id,Q,v,wpa,wpz)
C  Input:   Q(*)        one of the two incoming boson momenta 
c           id = 1      Q = q1 = W+ momentum
c           id = 2      Q = q2 = gamma/Z momentum
c  OUPTPUT: wpa(mu,nu)  lepton tensor for w gamma--> e+nu
c           wpz(mu,nu)  lepton tensor for w Z    --> e+nu
C  
C Generated by MadGraph II Version 3.0. Updated 02/19/04                
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : w+(q1) z(q2) -> e+ ve  
C FOR PROCESS : w+(q1) a(q2) -> e+ ve  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=   3) 
      INTEGER    NWAVEFUNCS
      PARAMETER (NWAVEFUNCS=   7) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS
C
      integer id
      real*8 q(0:3), v(0:3,4)
      complex*16 wpa(0:3,0:3), wpz(0:3,0:3)
C  
C LOCAL VARIABLES 
C  
      INTEGER I,J, MU, NU
      real*8 q2sq, Q1(0:3), Q2(0:3)
      COMPLEX*16 AMP(NGRAPHS)
      COMPLEX*16 W(6,NWAVEFUNCS), denz
c  lepton spinors and W+- polarization vectors
      include 'tensor.inc'

      logical ldebug
      parameter (ldebug=.false.)
C  
C GLOBAL VARIABLES
C  
      include "coupl.inc"
C                                                                          
C ----------
C BEGIN CODE
C ----------
      
      if (id.eq.1) then
         q2(0) = dreal(wp(5))
         q2(1) = dreal(wp(6))
         q2(2) = dimag(wp(6))
         q2(3) = dimag(wp(5))
         do mu = 0,3
            q1(mu) = q(mu)
            q2(mu) = q2(mu)-q(mu)
         enddo
      elseif (id.eq.2) then
         q1(0) = dreal(wp(5))
         q1(1) = dreal(wp(6))
         q1(2) = dimag(wp(6))
         q1(3) = dimag(wp(5))
         do mu = 0,3
            q2(mu) = q(mu)
            q1(mu) = q1(mu)-q(mu)
         enddo
      endif

      do i=1,6                  ! read in lepton states from common block
         w(i,3) = wep(i)
         w(i,4) = wve(i)
      enddo
      q2sq = q2(0)**2-q2(1)**2-q2(2)**2-q2(3)**2
      denz = q2sq/dcmplx(q2sq-zmass**2,zmass*zwidth)
      DO MU = 0,3
         DO NU = 0,3
            CALL VCARTX(Q1(0),WMASS ,Wwidth, MU,-1,W(1,1))        
            CALL VCARTX(Q2(0),ZERO  ,ZERO  , NU,-1,W(1,2))
            CALL FVIXXX(W(1,3),W(1,2),GZL ,ZERO,ZERO,W(1,5))    
            CALL IOVXXX(W(1,5),W(1,4),W(1,1),GWF ,AMP(1))            
            CALL FVIXXX(W(1,3),W(1,1),GWF ,ZERO,ZERO,W(1,6))    
            CALL IOVXXX(W(1,6),W(1,4),W(1,2),GZN ,AMP(2))            
            CALL JVVXXX(W(1,2),W(1,1),GWWZ ,WMASS,WWIDTH,W(1,7) )
            CALL IOVXXX(W(1,3),W(1,4),W(1,7),GWF ,AMP(3))            
            wpz(mu,nu) = (-AMP(   1)-AMP(   2)-AMP(   3))*denz
            wpa(mu,nu) = -(amp(1)*gal(1)/gzl(1)+amp(3)*gwwa/gwwz)
         enddo
      enddo

      end
