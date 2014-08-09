c  The next subroutines, open some histograms and prepare them 
c      to receive data 
c  You can substitute these  with your favourite ones
c  init   :  opens the histograms
c  topout :  closes them
c  pwhgfill  :  fills the histograms with data

      subroutine init_hist
      implicit none
      include  'LesHouches.h'
      include 'pwhg_math.h'
      integer j,k,i
      real * 8 dy,dpt,dr
      character * 1 cnum(9)
      data cnum/'1','2','3','4','5','6','7','8','9'/
      integer maxjet
      parameter (maxjet=4)
      integer nptmin
      parameter (nptmin=5)
      character * 4 cptmin(nptmin)
      real * 8 ptminarr(nptmin)
      data cptmin/  '-020','-040','-060','-080','-100'/
      data ptminarr/   20d0,  40d0,  60d0,  80d0, 100d0/
      common/infohist/ptminarr,cnum,cptmin
      save /infohist/
      real * 8 powheginput
      external powheginput
      integer nptbins1
      parameter (nptbins1 = 20)
      integer nptbins2
      parameter (nptbins2 = 20)
      integer netabins
      parameter (netabins = 20)
      real*8 ptbins1(nptbins1 + 1)
      real*8 ptbins2(nptbins2 + 1)
      real*8 etabins(netabins + 1)
      data ptbins1/60d0, 86.8d0, 117.8d0,153.1d0,192.6d0,236.3d0,
     1             284.3d0,336.5d0,392.9d0,453.5d0,518.4d0,587.5d0,
     2             660.9d0,738.5d0,820.3d0,906.3d0,996.6d0,1091.1d0,
     3             1189.8d0,1292.8d0,1400d0/
      data ptbins2/60d0,72.8d0,87.6d0,104.5d0,123.3d0,144.2d0,
     1             167.1d0,192d0,219d0,248d0,278.9d0,312d0,347d0,
     2             384d0,423.1d0,464.2d0,507.3d0,552.5d0,599.6d0,
     3             648.8d0,700d0/
      data etabins/-2.8d0,-2.52d0,-2.24d0,-1.96d0,-1.68d0,-1.4d0,
     1     -1.12d0,-0.84d0,-0.56d0,-0.28d0,0.0d0,0.28d0,0.56d0,
     2     0.84d0,1.12d0,1.4d0,1.68d0,1.96d0,2.24d0,2.52d0,2.8d0/
c
      integer ndphibins1
      parameter (ndphibins1 = 15)
      integer ndphibins2
      parameter (ndphibins2 = 10)
      integer ndphibins3
      parameter (ndphibins3 = 9)
      integer ndphibins4
      parameter (ndphibins4 = 4)
      integer nptmaxbins
      parameter (nptmaxbins = 9)
      real*8 dphibins1(ndphibins1 + 1)
      real*8 dphibins2(ndphibins2 + 1)
      real*8 dphibins3(ndphibins3 + 1)
      real*8 dphibins4(ndphibins4 + 1)
      real*8 dptmaxbins(nptmaxbins + 1)
      data dphibins1/0.50d0,0.59d0,0.66d0,0.71d0,0.75d0,0.78d0,0.81d0,
     1               0.84d0,0.86d0,0.88d0,0.90d0,0.92d0,0.94d0,0.96d0,
     2               0.98d0,1.00d0/
      data dphibins2/0.50d0,0.66d0,0.75d0,0.81d0,0.86d0,0.90d0,0.92d0,
     1               0.94d0,0.96d0,0.98d0,1.00d0/
      data dphibins3/0.50d0,0.66d0,0.75d0,0.81d0,0.86d0,0.90d0,0.94d0,
     1               0.96d0,0.98d0,1.00d0/
      data dphibins4/0.75d0,0.86d0,0.94d0,0.98d0,1.00d0/
      data dptmaxbins/110d0,160d0,210d0,260d0,310d0,400d0,500d0,600d0,
     1                800d0,10000d0/
      character*3 cptmax(9)
      data cptmax/'110','160','210','260','310','400','500',
     1            '600','800'/
c
      integer nptrangebins
      parameter (nptrangebins = 11)
      real*8 ptrangebins(nptrangebins + 1)
      data ptrangebins/30d0,40d0,60d0,80d0,110d0,160d0,210d0,260d0,
     1                 310d0,400d0,500d0,600d0/
      character*3 cptrange(nptrangebins + 1)
      data cptrange/' 30',' 40',' 60',' 80','110','160','210','260',
     1              '310','400','500','600'/
      integer nyrangebins
      parameter (nyrangebins = 5)
      real*8 yrangebins(nyrangebins + 1)
      data yrangebins/0.0d0,0.3d0,0.8d0,1.2d0,2.1d0,2.8d0/
      character*3 cyrange(nyrangebins + 1)
      data cyrange/'0.0','0.3','0.8','1.2','2.1','2.8'/
      real*8 deltar
      parameter (deltar = 0.1d0)
      real*8 Rparshapes
      parameter (Rparshapes = 0.6d0)
      real*8 MaxJetRapshapes
      parameter (MaxJetRapshapes = 2.8d0)
c
      integer nptbins
      parameter (nptbins = 4)
      real*8 ptbins(nptbins + 1)
      data ptbins/200d0,300d0,400d0,500d0,600d0/
      character*3 cptrangesub(nptbins + 1)
      data cptrangesub/'200','300','400','500','600'/
      real*8 jetmassmaxsub(nptbins)
      data jetmassmaxsub/220d0,220d0,310d0,310d0/
c
      call inihists

      dy=0.5d0
      dpt=10d0
      dr=0.2d0
      
      do i=1,nptmin
      call bookupeqbins('sigtot'//cptmin(i),1d0,0.5d0,1.5d0)      
      call bookupeqbins('Njet'//cptmin(i),1d0,-0.5d0,5.5d0)

      do j=1,maxjet
         call bookupeqbins('j'//cnum(j)//'-y'//cptmin(i),dy,-5d0,5d0)
         call bookupeqbins('j'//cnum(j)//'-eta'//cptmin(i),dy,-5d0,5d0)
         call bookupeqbins('j'//cnum(j)//'-pt'//cptmin(i),dpt,0d0,400d0)
         call bookupeqbins('j'//cnum(j)//'-ptzoom'//cptmin(i),
     $        2d0,1d0,151d0)
         call bookupeqbins('j'//cnum(j)//'-m'//cptmin(i),dpt,0d0,400d0) 
         call bookupeqbins('j'//cnum(j)//'-ptzoom2'//cptmin(i),
     $        0.5d0,0d0,20d0)
      enddo

      goto 300

      do j=1,maxjet-1
         do k=j+1,maxjet
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-y'//cptmin(i),dy,-5d0,5d0)  
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-eta'//cptmin(i),dy,-5d0,5d0)
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-pt'//cptmin(i),dpt,0d0,400d0)
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-m'//cptmin(i),dpt,0d0,400d0)  
         enddo
      enddo
   
      do j=1,maxjet-1
         do k=j+1,maxjet
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-dy'//cptmin(i),dy,-5d0,5d0)  
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-deta'//cptmin(i),dy,-5d0,5d0)
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-delphi'//cptmin(i),pi/20,0d0,pi)
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-dr'//cptmin(i),dr,0d0,10d0)  
         enddo
      enddo
  
      do j=1,maxjet
         call bookupeqbins('ptrel'//cnum(j)//cptmin(i),0.5d0,0d0,20d0)
      enddo   

 300  continue

      enddo
      
      call bookupeqbins('sigma',1.0d0,0.0d0,3.0d0)
      call bookupeqbins('njet',1d0,-0.5d0,5.5d0)
      call bookup('J pt 1st',nptbins1,ptbins1)
      call bookup('J pt 2nd',nptbins1,ptbins1)
      call bookup('J pt 3rd',nptbins2,ptbins2)
      call bookup('y 1st',netabins,etabins)
      call bookup('y 2nd',netabins,etabins)
      call bookup('y 3rd',netabins,etabins)
      call bookup('eta 1st',netabins,etabins)
      call bookup('eta 2nd',netabins,etabins)
      call bookup('eta 3rd',netabins,etabins)

c vvvvvvvvvvvv This part concerns the azimuthal decorrelation vvvvvvvv
c **** arXiv:1102.2696 ****
c for normalization purposes we store the cross sections:
      do j=1,nptmaxbins
        if (j.le.5) then
          call bookupeqbins('sigma '//cptmax(j)//' < ptmax < '//
     >                         cptmax(j+1),1d0,0.5d0,1.5d0)
        else if ((j.gt.5).and.(j.le.7)) then
          call bookupeqbins('sigma '//cptmax(j)//' < ptmax < '//
     >                         cptmax(j+1),1d0,0.5d0,1.5d0)
        else if (j.eq.8) then
          call bookupeqbins('sigma '//cptmax(j)//' < ptmax < '//
     >                         cptmax(j+1),1d0,0.5d0,1.5d0)
        else if (j.eq.9) then
          call bookupeqbins('sigma '//cptmax(j)//' < ptmax',
     >                1d0,0.5d0,1.5d0)
        end if
      end do
      do j=1,nptmaxbins
        if (j.le.5) then
          call bookup('dphi '//cptmax(j)//' < ptmax < '//
     >                         cptmax(j+1),ndphibins1,dphibins1)
        else if ((j.gt.5).and.(j.le.7)) then
          call bookup('dphi '//cptmax(j)//' < ptmax < '//
     >                         cptmax(j+1),ndphibins2,dphibins2)
        else if (j.eq.8) then
          call bookup('dphi '//cptmax(j)//' < ptmax < '//
     >                         cptmax(j+1),ndphibins3,dphibins3)
        else if (j.eq.9) then
          call bookup('dphi '//cptmax(j)//' < ptmax',
     >                ndphibins4,dphibins4)
        end if
      end do
c ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

c vvvvvvvvvvvv This part concerns the jet shapes vvvvvvvvvvvvvvvvvvvvv
c **** arXiv:1101.0070 ****
c Differential jet shapes:
      do j=1,nptrangebins
        call bookupeqbins('rho(r) '//cptrange(j)//' < pt < '//
     >                    cptrange(j+1),deltar,0d0,Rparshapes)
      end do
      do j=1,nptrangebins
        do k=1,nyrangebins
          call bookupeqbins('rho(r) '//cptrange(j)//' < pt < '//
     >                      cptrange(j+1)//cyrange(k)//' < |y| < '//
     >                      cyrange(k+1),deltar,0d0,Rparshapes)
        end do
      end do
c Integrated jet shapes:
      call bookup('1 - psi(r=0.3) ',nptrangebins,ptrangebins)
      do j=1,nptrangebins
        call bookup('1 - psi(r=0.3) '//cptrange(j)//' < pt < '//
     >              cptrange(j+1),nyrangebins,yrangebins)
      end do
      do j=1,nptrangebins
        do k=1,nyrangebins
          call bookup('psi(r) '//cptrange(j)//' < pt < '//
     >                cptrange(j+1)//cyrange(k)//' < |y| < '//
     >                cyrange(k+1),deltar,0d0,Rparshapes)
        end do
      end do
c ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

c vvvvvvvvvvvv This part concerns the jet substructures vvvvvvvvvvvvvv
c **** arXiv:1203.4606 ****
c Jet pt, eta mass and substructure related observables:
      call bookupeqbins('pt, CA, R=1.2, |y| < 2, 200 < pt',
     >                  100d0,200d0,1000d0)
      call bookupeqbins('pt, antikt, R=1.0, |y| < 2, 200 < pt',
     >                  100d0,200d0,1000d0)
      call bookupeqbins('pt, CA, R=1.2, |y| < 2, 200 < pt, '//
     >                  'splitfilt',100d0,200d0,1000d0)
      call bookupeqbins('eta, CA, R=1.2, 200 < pt',
     >                  0.5d0,-2.5d0,2.5d0)
      call bookupeqbins('eta, antikt, R=1.0, 200 < pt',
     >                  0.5d0,-2.5d0,2.5d0)
      call bookupeqbins('eta, CA, R=1.2, 200 < pt, '//
     >                  'splitfilt',0.5d0,-2.5d0,2.5d0)
      call bookupeqbins('mass, CA, R=1.2, |y| < 2, 300 < pt < 400',
     >                  10d0,0d0,250d0)
      call bookupeqbins('mass, antikt, R=1.0, |y| < 2, 300 < pt < 400',
     >                  10d0,0d0,250d0)
      call bookupeqbins('mass, CA, R=1.2, |y| < 2, 300 < pt < 400, '//
     >                  'splitfilt',10d0,0d0,250d0)
c \sqrt{d_{12}}:
      call bookupeqbins('sd12, antikt, R=1.0, |y| < 2, 300 < pt < 400',
     >                  5d0,0d0,100d0)
c \sqrt{d_{23}}:
      call bookupeqbins('sd23, antikt, R=1.0, |y| < 2, 300 < pt < 400',
     >                  2d0,0d0,40d0)
c \tau_{21}:
      call bookupeqbins('tau21, antikt, R=1.0, |y| < 2, 300 < pt < 400',
     >                  0.08d0,0d0,1.2d0)
c \tau_{32}:
      call bookupeqbins('tau32, antikt, R=1.0, |y| < 2, 300 < pt < 400',
     >                  0.08d0,0d0,1.2d0)
c Jet Mass part:
c jet mass with CA-algorithm without filtering:
      do j=1,nptbins
        call bookupeqbins('jetmass, CA, R=1.2, |y| < 2, '//
     >                    cptrangesub(j)//' < pt < '//cptrangesub(j+1),
     >                    10d0,0d0,jetmassmaxsub(j))
      end do
c jet mass with CA-algorithm with filtering:
      do j=1,nptbins
        call bookupeqbins('jetmass, CA splitfilt, R=1.2, |y| < 2, '//
     >                    cptrangesub(j)//' < pt < '//cptrangesub(j+1),
     >                    10d0,0d0,jetmassmaxsub(j))
      end do
c jet mass with antikt-algorithm without filtering:
      do j=1,nptbins
        call bookupeqbins('jetmass, antikt, R=1.0, |y| < 2, '//
     >                    cptrangesub(j)//' < pt < '//cptrangesub(j+1),
     >                    10d0,0d0,jetmassmaxsub(j))
      end do
c \sqrt{d_{12}} part:
c \sqrt{d_{12}} with CA-algorithm without filtering:
      do j=1,nptbins
        call bookupeqbins('sqrt(d12), CA, R=1.2, |y| < 2, '//
     >                    cptrangesub(j)//' < pt < '//cptrangesub(j+1),
     >                    5d0,0d0,100d0)
      end do
c \sqrt{d_{12}} with CA-algorithm with filtering:
      do j=1,nptbins
        call bookupeqbins('sqrt(d12), CA splitfilt, R=1.2, |y| < 2, '//
     >                    cptrangesub(j)//' < pt < '//cptrangesub(j+1),
     >                    5d0,0d0,100d0)
      end do
c \sqrt{d_{12}} with antikt-algorithm without filtering:
      do j=1,nptbins
        call bookupeqbins('sqrt(d12), antikt, R=1.0, |y| < 2, '//
     >                    cptrangesub(j)//' < pt < '//cptrangesub(j+1),
     >                    5d0,0d0,100d0)
      end do
c ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      end
     

      subroutine analysis(dsig0)
      implicit none
      real * 8 dsig0
      include 'hepevt.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_math.h' 
      include 'pwhg_rad.h' 
      include 'pwhg_weights.h'
c      include 'pwhg_flg.h'
c      include 'LesHouches.h'
      integer   maxjet,mjets,njets,numjets,ntracks
      parameter (maxjet=2048)
      real * 8  ktj(maxjet),etaj(maxjet),rapj(maxjet),
     1    phij(maxjet),pj(4,maxjet),rr,ptrel(4),ptot(4)
      integer maxtrack
      parameter (maxtrack=2048)
      real * 8  ptrack(4,maxtrack)
      integer   jetvec(maxtrack),itrackhep(maxtrack)
      character * 1 cnum(9)
      integer nptmin
      parameter (nptmin=5)
      character * 4 cptmin(nptmin)
      real * 8 ptminarr(nptmin)      
      common/infohist/ptminarr,cnum,cptmin
      save /infohist/
      integer j,k,i,jj,ii
c     we need to tell to this analysis file which program is running it
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/
      integer ih,il,inu
      real * 8 psum(4)
      real * 8 httot,y,eta,pt,m
      real * 8 dy,deta,delphi,dr
      integer ihep
      real * 8 powheginput,dotp
      external powheginput,dotp
      real * 8 ptmin,mininvmass
      integer maxnumlep
      parameter (maxnumlep=10)
      real * 8 pvl(4,maxnumlep),plep(4,maxnumlep)
      integer mu,ilep,ivl,nlep,nvl
      logical is_W
      real * 8 mV2,ptvb,mvb,ptlep,ptminfastjet,ptvl,R,ylep,yvb,yvl
      real * 8 Wmass,Wwidth,Wmasslow,Wmasshigh
      integer jpart, jjet
      real * 8 palg
c      real * 8 rescfac1,rescfac2
c      common /crescfac/rescfac1,rescfac2
      real * 8 dsig(7)
      integer nweights
      logical ini
      data ini/.true./
      logical inimulti
      data inimulti/.true./
      integer  minlo
      data minlo/0/
      save inimulti,minlo,ini
      integer minnumjets,maxnumjets
      logical pwhg_isfinite
      external pwhg_isfinite
      character * 4 prefix
c
      real*8 dy12,deta12,dr12,dphi12
c
      real*8 MinJetPt
      parameter (MinJetPt = 60d0)
      real*8 Min1stJetPt
      parameter (Min1stJetPt = 80d0)
      real*8 MaxEtaJet
      parameter (MaxEtaJet = 2.8d0)
      real*8 MaxRapJet
      parameter (MaxRapJet = 2.8d0)
      real*8 Rpar
      parameter (Rpar = 0.4d0)
      real*8 pj1(4),pj2(4),pj3(4)
      real*8 ptj1,ptj2,ptj3
      real*8 etaj1,etaj2,etaj3
      real*8 yj1,yj2,yj3
      real*8 mj1,mj2,mj3
c
      integer l
      real*8 rho,psi
c
      real*8 calcrho,calcpsi
      external calcrho,calcpsi
c
c These declarations are needed for azimuthal decorrelations:
      real*8 MinJetPtdphi
      parameter (MinJetPtdphi = 100d0)
      real*8 MaxRapJetdphi
      parameter (MaxRapJetdphi = 0.8d0)
      real*8 Rpardphi
      parameter (Rpardphi = 0.6d0)
c
      integer nptmaxbins
      parameter (nptmaxbins = 9)
      real*8 dptmaxbins(nptmaxbins + 1)
      data dptmaxbins/110d0,160d0,210d0,260d0,310d0,400d0,500d0,600d0,
     1                800d0,10000d0/
      character*3 cptmax(9)
      data cptmax/'110','160','210','260','310','400','500',
     1            '600','800'/
c
c These declarations for the jet shapes:
      integer nptrangebins
      parameter (nptrangebins = 11)
      real*8 ptrangebins(nptrangebins + 1)
      data ptrangebins/30d0,40d0,60d0,80d0,110d0,160d0,210d0,260d0,
     1                 310d0,400d0,500d0,600d0/
      character*3 cptrange(nptrangebins + 1)
      data cptrange/' 30',' 40',' 60',' 80','110','160','210','260',
     1              '310','400','500','600'/
      integer nyrangebins
      parameter (nyrangebins = 5)
      real*8 yrangebins(nyrangebins + 1)
      data yrangebins/0.0d0,0.3d0,0.8d0,1.2d0,2.1d0,2.8d0/
      character*3 cyrange(nyrangebins + 1)
      data cyrange/'0.0','0.3','0.8','1.2','2.1','2.8'/
      real*8 deltar
      parameter (deltar = 0.1d0)
      real*8 Rparshapes
      parameter (Rparshapes = 0.6d0)
      real*8 MaxJetRapshapes
      parameter (MaxJetRapshapes = 2.8d0)
c

      if (.not.pwhg_isfinite(dsig0)) then
         write(*,*) '***PROBLEMS***'
         return
      endif

c      write(*,*) 'nhep ',nhep

      maxnumjets=4

c      call reweightifneeded(dsig0,dsig)

      if(inimulti) then
         if(weights_num.eq.0) then
            call setupmulti(1)
         else
            call setupmulti(weights_num)
         endif
         inimulti=.false.
      endif

      dsig=0
      if(weights_num.eq.0) then
         dsig(1)=dsig0
         nweights=1
      else
         dsig(1:weights_num)=weights_val(1:weights_num)
          nweights=weights_num
      endif

      if(sum(abs(dsig)).eq.0) return

      if (ini) then
         minlo=powheginput('#minlo')
         if (minlo.eq.1) then
            minnumjets = 2
         else
            minnumjets = 3
         endif
         ini=.false.
      endif

c     set up arrays for jet finding
c      do jpart=1,maxtrack
c         do mu=1,4
c            ptrack(mu,jpart)=0d0
c         enddo
c         jetvec(jpart)=0
c      enddo      
c      do jjet=1,maxjet
c         do mu=1,4
c            pj(mu,jjet)=0d0
c         enddo
c      enddo

      ntracks=0
      mjets=0
c     Loop over final state particles to find jets 
      do ihep=1,nhep
         if (isthep(ihep).eq.1) then
           if (ntracks.eq.maxtrack) then
              write(*,*) 'Too many particles. Increase maxtrack.'//
     #             ' PROGRAM ABORTS'
              call pwhg_exit(-1)
           endif
c     copy momenta to construct jets 
           ntracks=ntracks+1
           do mu=1,4
              ptrack(mu,ntracks)=phep(mu,ihep)
           enddo
        endif
      enddo



      goto 111


      if (ntracks.eq.0) then
         numjets=0
      else
c     palg=1 is standard kt, -1 is antikt
         palg = -1d0
         R = 0.5d0              ! radius parameter
c         ptminfastjet = 1d0
         ptminfastjet = ptminarr(1)
         call fastjetppgenkt(ptrack,ntracks,R,palg,ptminfastjet,
     $        pj,numjets,jetvec)
c         call fastjetktwhich(ptrack,ntracks,ptminfastjet,R,
c     $        pj,mjets,jetvec) 
      endif

c      write(*,*) 'numjets ',numjets


      do i=1,nptmin        
         njets=0    
         do j=1,min(maxnumjets,numjets)
            ktj(j) = sqrt(pj(1,j)**2 + pj(2,j)**2 )            
            if (ktj(j).gt.ptminarr(i)) then
               njets=njets+1
            endif
         enddo


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         if (njets.lt.minnumjets) exit
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


         mininvmass=1d30
         do ii=1,njets
            do j=ii+1,njets
               ptot(:)=pj(:,ii)+pj(:,j)
               call pwhg_getinvmass(ptot(:),m)
               mininvmass=min(mininvmass,m)
            enddo
         enddo
         

c********************************************************
         if (mininvmass.lt.100d0) exit
c********************************************************



c         write(*,*) 'njets ',njets


         call filld('sigtot'//cptmin(i),1d0,dsig)
                  
         if(njets.eq.0) then
            call filld('Njet'//cptmin(i),0d0,dsig)
         elseif(njets.eq.1) then
            call filld('Njet'//cptmin(i),1d0,dsig)
         elseif(njets.eq.2) then
            call filld('Njet'//cptmin(i),2d0,dsig)
         elseif(njets.eq.3) then
            call filld('Njet'//cptmin(i),3d0,dsig)
         elseif(njets.eq.4) then
            call filld('Njet'//cptmin(i),4d0,dsig)
         elseif(njets.eq.5) then
            call filld('Njet'//cptmin(i),5d0,dsig)
         else
c     write(*,*) ' Njet?',mjets
         endif
         
c     jets
         mjets=min(njets,maxnumjets)
         
         do j=1,mjets
            call getyetaptmass(pj(:,j),y,eta,pt,m)
            call filld('j'//cnum(j)//'-y'//cptmin(i),     y, dsig)
            call filld('j'//cnum(j)//'-eta'//cptmin(i), eta, dsig)
            call filld('j'//cnum(j)//'-pt'//cptmin(i),   pt, dsig)
            call filld('j'//cnum(j)//'-ptzoom'//cptmin(i),   pt, dsig)
            call filld('j'//cnum(j)//'-ptzoom2'//cptmin(i),   pt, dsig)
            call filld('j'//cnum(j)//'-m'//cptmin(i),     m, dsig)
c     call filld('ptrel'//cnum(j)//cptmin(i),ptrel(j), dsig)         
         enddo
         


c$$$         do j=1,mjets
c$$$            do k=j+1,mjets
c$$$               call getyetaptmass(pj(:,j)+pj(:,k),y,eta,pt,m)
c$$$               call filld('j'//cnum(j)//'j'//cnum(k)//'-y'//cptmin(i),
c$$$     $              y, dsig)
c$$$               call filld('j'//cnum(j)//'j'//cnum(k)//'-eta'//cptmin(i),
c$$$     $              eta, dsig)
c$$$               call filld('j'//cnum(j)//'j'//cnum(k)//'-pt'//cptmin(i),
c$$$     $              pt, dsig)
c$$$               call filld('j'//cnum(j)//'j'//cnum(k)//'-m'//cptmin(i), 
c$$$     $              m, dsig)
c$$$            enddo
c$$$         enddo
c$$$         
c$$$         do j=1,mjets
c$$$            do k=j+1,mjets
c$$$               prefix = 'j'//cnum(j)//'j'//cnum(k)               
c$$$               call getdydetadphidr(pj(:,j),pj(:,k),dy,deta,delphi,dr)
c$$$               call filld(prefix//'-dy'//cptmin(i),dy,dsig)
c$$$               call filld(prefix//'-deta'//cptmin(i),deta,dsig)
c$$$               call filld(prefix//'-delphi'//cptmin(i),delphi,dsig)
c$$$               call filld(prefix//'-dr'//cptmin(i),dr,dsig)
c$$$            enddo
c$$$         enddo
c$$$
         
      enddo

c      call fastjetppgenkt_BBUY(ptrack,ntracks,Rpar,-1d0,MinJetPt,
c     $     MaxRapJet,pj,njets,jetvec)




 111  continue

      palg = -1d0
      R = 0.4d0                 ! radius parameter
c     ptminfastjet = 1d0
      ptminfastjet = 60d0
      call fastjetppgenkt(ptrack,ntracks,R,palg,ptminfastjet,
     $     pj,njets,jetvec)
      
c We need at least 3 jets:
      if (njets.lt.3) goto 222
c
c We calculate the pt's, eta's and rapidities for the three leading jets:
      pj1 = pj(:,1)
      pj2 = pj(:,2)
      pj3 = pj(:,3)
c
      call getyetaptmass(pj1,yj1,etaj1,ptj1,mj1)
      call getyetaptmass(pj2,yj2,etaj2,ptj2,mj2)
      call getyetaptmass(pj3,yj3,etaj3,ptj3,mj3)

c     we require the first to be harder
      if (ptj1.lt.80d0) return
      if (abs(yj1).gt.2.8.or.abs(yj2).gt.2.8.or.abs(yj3).gt.2.8) 
     >  goto 222

      call filld('sigma',1.5d0,dsig)

      if(njets.eq.0) then
         call filld('njet',0d0,dsig)
      elseif(njets.eq.1) then
         call filld('njet',1d0,dsig)
      elseif(njets.eq.2) then
         call filld('njet',2d0,dsig)
      elseif(njets.eq.3) then
         call filld('njet',3d0,dsig)
      elseif(njets.eq.4) then
         call filld('njet',4d0,dsig)
      elseif(njets.eq.5) then
         call filld('njet',5d0,dsig)
      endif
      call filld('J pt 1st',ptj1,dsig)
      call filld('J pt 2nd',ptj2,dsig)
      call filld('J pt 3rd',ptj3,dsig)
      call filld('y 1st',yj1,dsig)
      call filld('y 2nd',yj2,dsig)
      call filld('y 3rd',yj3,dsig)
      call filld('eta 1st',etaj1,dsig)
      call filld('eta 2nd',etaj2,dsig)
      call filld('eta 3rd',etaj3,dsig)

 222  continue


c vvvvvvvvvvvv This part concerns the azimuthal decorrelation vvvvvvvv
c We can only do this analysis if minlo is activated:
      if (minlo.eq.1) then !<minlo>
      R = Rpardphi
      palg = -1d0
      call fastjetppgenkt_pty(ptrack,ntracks,R,palg,
     $                        MinJetPtdphi,MaxRapJetdphi,
     $                        pj,njets,jetvec)
c
      if (njets.lt.2) goto 333
c
      pj1 = pj(:,1)
      pj2 = pj(:,2)
c
      call getyetaptmass(pj1,yj1,etaj1,ptj1,mj1)
      call getyetaptmass(pj2,yj2,etaj2,ptj2,mj2)
      call getdydetadphidr(pj1,pj2,dy12,deta12,dphi12,dr12)
c
      dphi12 = dphi12/pi
c
      do j=1,nptmaxbins
        if ((ptj1.gt.dptmaxbins(j)).and.(ptj1.lt.dptmaxbins(j+1))) then
          if (j.ne.nptmaxbins) then
            if ((j.lt.3).and.
     >          (dphi12.lt.23d0/24d0).and.
     >          (dphi12.gt.0.5d0)
     >         ) then
              call filld('sigma '//cptmax(j)//' < ptmax < '//
     >                 cptmax(j+1),1.0d0,dsig)
              call filld('dphi '//cptmax(j)//' < ptmax < '//
     >                   cptmax(j+1),dphi12,dsig)
            end if
            if ((j.gt.2).and.
     >          (dphi12.lt.47d0/48d0).and.
     >          (dphi12.gt.0.5d0)
     >         ) then
              call filld('sigma '//cptmax(j)//' < ptmax < '//
     >                 cptmax(j+1),1.0d0,dsig)
              call filld('dphi '//cptmax(j)//' < ptmax < '//
     >                   cptmax(j+1),dphi12,dsig)
            end if
          else if (j.eq.nptmaxbins) then
            if ((dphi12.lt.47d0/48d0).and.
     >          (dphi12.gt.0.75d0)
     >         ) then
              call filld('dphi '//cptmax(j)//' < ptmax',dphi12,dsig)
              call filld('sigma '//cptmax(j)//' < ptmax',1.0d0,dsig)
            end if
          end if
        end if
      end do
c
      end if !<minlo>
c ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

c *+*+*+*+*

c vvvvvvvvvvvv This part concerns the jet shapes vvvvvvvvvvvvvvvvvvvvv
c We can only do this analysis if minlo is activated:
      if (minlo.eq.1) then !<minlo>
c
c Differential in pt:
      do j=1,nptrangebins
        do l=0,5
          rho = calcrho(ntracks,njets,ptrack,pj,jetvec,
     >                  ptrangebins(j),ptrangebins(j+1),
     >                  0d0,yrangebins(nyrangebins+1),
     >                  Rparshapes,deltar,deltar/2d0 + deltar*l)
          call filld('rho(r) '//cptrange(j)//' < pt < '//
     >               cptrange(j+1),deltar/2d0 + deltar*l,rho)
        end do
      end do
c
c Differential in pt and y:
c Loop over the whole pt range:
      do j=1,nptrangebins
c Loop over the total rapidity range:
        do k=1,nyrangebins
          do l=0,5
            rho = calcrho(ntracks,njets,ptrack,pj,jetvec,
     >                    ptrangebins(j),ptrangebins(j+1),
     >                    yrangebins(k),yrangebins(k+1),
     >                    Rparshapes,deltar,deltar/2d0 + deltar*l)
            call filld('rho(r) '//cptrange(j)//' < pt < '//
     >                 cptrange(j+1)//cyrange(k)//' < |y| < '//
     >                 cyrange(k+1),deltar/2d0 + deltar*l,rho)
          end do
        end do
      end do
c
c We calculate psi(r = 0.3) for different pt regions:
      do j=1,nptrangebins
        psi = calcpsi(ntracks,njets,ptrack,pj,jetvec,
     >                ptrangebins(1),ptrangebins(nptrangebins + 1),
     >                ptrangebins(j),ptrangebins(j + 1),
     >                yrangebins(1),yrangebins(nyrangebins + 1),
     >                yrangebins(1),yrangebins(nyrangebins + 1),
     >                Rparshapes,0.3d0)
        call filld('1 - psi(r=0.3) ',(ptrangebins(j+1) + 
     >             ptrangebins(j))/2d0,1d0 - psi)
      end do
      do j=1,nptrangebins
        do k=1,nyrangebins
          psi = calcpsi(ntracks,njets,ptrack,pj,jetvec,
     >                  ptrangebins(j),ptrangebins(j + 1),
     >                  ptrangebins(j),ptrangebins(j + 1),
     >                  yrangebins(1),yrangebins(nyrangebins + 1),
     >                  yrangebins(k),yrangebins(k + 1),
     >                  Rparshapes,0.3d0)
          call filld('1 - psi(r=0.3) '//cptrange(j)//' < pt < '//
     >               cptrange(j+1),(yrangebins(k) + 
     >               yrangebins(k+1))/2d0,1d0 - psi)
c
          do l=0,5
            psi = calcpsi(ntracks,njets,ptrack,pj,jetvec,
     >                    ptrangebins(j),ptrangebins(j + 1),
     >                    ptrangebins(j),ptrangebins(j + 1),
     >                    yrangebins(k),yrangebins(k + 1),
     >                    yrangebins(k),yrangebins(k + 1),
     >                    Rparshapes,deltar + l*deltar)
            call filld('psi(r) '//cptrange(j)//' < pt < '//
     >                 cptrange(j+1)//cyrange(k)//' < |y| < '//
     >                 cyrange(k+1),deltar/2d0 + l*deltar,psi)
          end do
        end do
      end do
c
      end if !<minlo>
c ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

 333  continue

      end




c      subroutine yetaptmassplot(p,dsig,prefix)
c      implicit none
c      real * 8 p(4),dsig
c      character *(*) prefix
c      real * 8 y,eta,pt,m
c      call getyetaptmass(p,y,eta,pt,m)
c      call filld(prefix//'-y',y,dsig)
c      call filld(prefix//'-eta',eta,dsig)
c      call filld(prefix//'-pt',pt,dsig)
c      call filld(prefix//'-m',m,dsig)
c      end

      subroutine deltaplot(p1,p2,dsig,prefix,postfix)
      implicit none
      real * 8 p1(4),p2(4),dsig(7)
      character *(*) prefix,postfix
      real * 8 dy,deta,delphi,dr
      call getdydetadphidr(p1,p2,dy,deta,delphi,dr)
      call filld(prefix//'-dy'//postfix,dy,dsig)
      call filld(prefix//'-deta'//postfix,deta,dsig)
      call filld(prefix//'-delphi'//postfix,delphi,dsig)
      call filld(prefix//'-dr'//postfix,dr,dsig)
      end

      subroutine getyetaptmass(p,y,eta,pt,mass)
      implicit none
      real * 8 p(4),y,eta,pt,mass
      call pwhg_getrapidity(p,y)      
      pt=sqrt(p(1)**2+p(2)**2)
      call pwhg_getpseudorapidity(p,eta)
      call pwhg_getinvmass(p,mass)
      end

      subroutine getdydetadphidr(p1,p2,dy,deta,dphi,dr)
      implicit none
      include 'pwhg_math.h' 
      real * 8 p1(*),p2(*),dy,deta,dphi,dr
      real * 8 y1,eta1,pt1,mass1,phi1
      real * 8 y2,eta2,pt2,mass2,phi2
      call getyetaptmass(p1,y1,eta1,pt1,mass1)
      call getyetaptmass(p2,y2,eta2,pt2,mass2)
      dy=y1-y2
      deta=eta1-eta2
      phi1=atan2(p1(1),p1(2))
      phi2=atan2(p2(1),p2(2))
      dphi=abs(phi1-phi2)
      dphi=min(dphi,2d0*pi-dphi)
      dr=sqrt(deta**2+dphi**2)
      end


      subroutine sortbypt(n,iarr)
      implicit none
      integer n,iarr(n)
      include 'hepevt.h'
      integer j,k
      real * 8 tmp,pt(nmxhep)
      logical touched
      do j=1,n
         pt(j)=sqrt(phep(1,iarr(j))**2+phep(2,iarr(j))**2)
      enddo
c bubble sort
      touched=.true.
      do while(touched)
         touched=.false.
         do j=1,n-1
            if(pt(j).lt.pt(j+1)) then
               k=iarr(j)
               iarr(j)=iarr(j+1)
               iarr(j+1)=k
               tmp=pt(j)
               pt(j)=pt(j+1)
               pt(j+1)=tmp
               touched=.true.
            endif
         enddo
      enddo
      end

      function islept(j)
      implicit none
      logical islept
      integer j
      if(abs(j).ge.11.and.abs(j).le.15) then
         islept = .true.
      else
         islept = .false.
      endif
      end

      function get_ptrel(pin,pjet)
      implicit none
      real * 8 get_ptrel,pin(0:3),pjet(0:3)
      real * 8 pin2,pjet2,cth2,scalprod
      pin2  = pin(1)**2 + pin(2)**2 + pin(3)**2
      pjet2 = pjet(1)**2 + pjet(2)**2 + pjet(3)**2
      scalprod = pin(1)*pjet(1) + pin(2)*pjet(2) + pin(3)*pjet(3)
      cth2 = scalprod**2/pin2/pjet2
      get_ptrel = sqrt(pin2*abs(1d0 - cth2))
      end

      subroutine pwhgfinalopshist
      end

      function calcrho(ntrack,njet,ptrack,pjet,jetvec,
     >                 ptmin,ptmax,ymin,ymax,Rpar,dr,r)
      implicit none
c
      real*8 calcrho
c
      integer maxtrack,maxjet
      parameter (maxtrack=2048)
      parameter (maxjet=2048)
c
      integer ntrack,njet
      real*8 ptrack(4,maxtrack),pjet(4,maxjet)
      integer jetvec(maxtrack)
      real*8 ptmin,ptmax
      real*8 ymin,ymax
      real*8 Rpar,dr,r
c
      integer ijet
      integer numjets
      real*8 p_j(4)
      real*8 pt_j,y_j
      real*8 pt_dr
c
      real*8 calcptdr
      external calcptdr
c
      calcrho = 0d0
      numjets = 0
c
c we loop over all jets:
      do ijet=1,njet
        p_j = pjet(:,ijet)
        call pwhg_getpt(p_j,pt_j)
        call pwhg_getrapidity(p_j,y_j)
c We only consider those jets for which the pt is in a given range:
        if ((pt_j.lt.ptmin).or.(pt_j.gt.ptmax)) cycle
c We even include the possibility to make cuts on rapidity:
        if ((abs(y_j).lt.ymin).or.(abs(y_j).gt.ymax)) cycle
c We have an extra cut on the jet pt, hence njet is not the number of
c jets considered for rho:
        numjets = numjets + 1
c we calculate the pt contribution of an annulus between r - \Delta r/2
c and r + \Delta r/2
        pt_dr = calcptdr(ijet,p_j,ntrack,ptrack,jetvec,r,dr)
        calcrho = calcrho + pt_dr/pt_j
      end do
c normalization:
      if (numjets.gt.0) calcrho = calcrho/dr/numjets
c
      end function calcrho
c
c This function calculates the pt contribution coming from an annulus
c between r - dr/2 and r + dr/2. The jet orientation is
c given by the jet momentum (pj). 
      function calcptdr(ijet,pj,ntrack,ptrack,jetvec,r,dr)
      implicit none
c
      real*8 calcptdr
c
      integer maxtrack
      parameter (maxtrack = 2048)
c
      integer ijet
      real*8 pj(4)
      integer ntrack
      real*8 ptrack(4,maxtrack)
      integer jetvec(maxtrack)
      real*8 r,dr
c
      integer itrack
      real*8 dr2
      real*8 deltar,pt_tmp
c
c
      calcptdr = 0d0
c
      dr2 = dr/2d0
c
c We loop over all tracks and select only those which are part
c of the ijet-th jet:
      do itrack=1,ntrack
        if (jetvec(itrack).ne.ijet) cycle
c If we are here the itrack-th track is part of the ijet-th jet:
        call pwhg_getR_phiy(pj,ptrack(:,itrack),deltar)
c is it in the annulus?
        if ((deltar.lt.(r - dr2)).or.(deltar.gt.(r + dr2))) cycle
        call pwhg_getpt(ptrack(:,itrack),pt_tmp)
        calcptdr = calcptdr + pt_tmp
      end do
c
      end function calcptdr
c
c This routine calculates the integrated jet shape. 
c ptmin, ptmax are the cuts on the pt of the considered jets,
c ymin and ymax have the same role but with the rapidities,
c while ptw,yw variables are used to define the pt and y
c window.
      function calcpsi(ntrack,njet,ptrack,pjet,jetvec,
     >                 ptmin,ptmax,ymin,ymax,
     >                 ptwmin,ptwmax,ywmin,ywmax,
     >                 Rpar,r)
      implicit none
c
      real*8 calcpsi
c
      integer maxtrack,maxjet
      parameter (maxtrack=2048)
      parameter (maxjet=2048)
c
      integer ntrack,njet
      real*8 ptrack(4,maxtrack),pjet(4,maxjet)
      integer jetvec(maxtrack)
      real*8 ptmin,ptmax
      real*8 ymin,ymax
      real*8 ptwmin,ptwmax
      real*8 ywmin,ywmax
      real*8 Rpar,r
c
      integer ijet
      integer numjets
      real*8 p_j(4)
      real*8 pt_j,y_j
      real*8 pt_r
c
      real*8 calcptdr
      external calcptdr
c
      calcpsi = 0d0
      numjets = 0
c
c we loop over all jets:
      do ijet=1,njet
        p_j = pjet(:,ijet)
        call pwhg_getpt(p_j,pt_j)
        call pwhg_getrapidity(p_j,y_j)
c We only consider those jets for which the pt is in a given range:
        if ((pt_j.lt.ptmin).or.(pt_j.gt.ptmax)) cycle
c We even include the possibility to make cuts on rapidity:
        if ((abs(y_j).lt.ymin).or.(abs(y_j).gt.ymax)) cycle
        numjets = numjets + 1
        if ((pt_j.lt.ptwmin).or.(pt_j.gt.ptwmax)) cycle
        if ((abs(y_j).lt.ywmin).or.(abs(y_j).gt.ywmax)) cycle
c The next few lines are needed for the window:
c we calculate the pt contribution of between 0 and r:
c we can use the previously implemented calcptdr function 
c with parameters: r = -r/2 and dr = r/2, with this
c parameter choice the function calculates the pt 
c contribution from tracks situating in an annulus between 
c zero and r
        pt_r = calcptdr(ijet,p_j,ntrack,ptrack,jetvec,-r/2d0,r/2d0)
        calcpsi = calcpsi + pt_r/pt_j
      end do
c normalization:
      if (numjets.gt.0) calcpsi = calcpsi/numjets
c
      end function calcpsi
