      subroutine setborn(p,bflav,born,bornjk,bmunu)
c Wrapper subroutine to call the MadGraph Borns
c and set the event-by-event couplings constant
      implicit none
      include 'nlegborn.h'
      real * 8 p(0:3,nlegborn),bornjk(nlegborn,nlegborn)
      integer bflav(nlegborn)
      real * 8 bmunu(0:3,0:3,nlegborn),born

      real * 8 born_nlojet,ratio
      real * 8 powheginput
      external powheginput
      logical ini,bornonly
      data ini/.true./
      save bornonly,ini

      if (ini) then
         ini=.false.
         bornonly=(powheginput("#bornonly").eq.1d0)
      endif

c     if bornonly, it's faster to use NLOJET to compute the Born contributions.
c     bornjk and bmunu are NOT needed
      if (bornonly) then
         call setborn_nlojet(p,bflav,born_nlojet)
         born_nlojet=abs(born_nlojet)
         born=born_nlojet
         return
      endif


      call set_ebe_couplings
      call sborn_proc(p,bflav,born,bornjk,bmunu)

c      write(*,*) 'Born ',born


      
      end

      subroutine borncolour_lh
c Wrapper subroutine to call the MadGraph code to associate
c a (leading) color structure to an event.
      implicit none
      include 'nlegborn.h'
      include 'LesHouches.h'
      integer bflav(nlegborn),color(2,nlegborn)
      integer i,j
      do i=1,nlegborn
         bflav(i)=idup(i)
         if (bflav(i).eq.21) bflav(i)=0
      enddo
      call born_color(bflav,color)
      do i=1,2
         do j=1,nlegborn
            icolup(i,j)=color(i,j)
         enddo
      enddo
      end

      subroutine finalize_lh
c Specify here if resonances need be written in the event file.
      implicit none
      end




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCC            NLOJET++         CCCCCCCCCC      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine setborn_nlojet(p,bflav,born)
      implicit none
      include 'nlegborn.h'
      include 'PhysPars.h'
      include 'pwhg_st.h'
      include 'pwhg_kn.h'
      include 'pwhg_flst.h'
      include 'pwhg_math.h'
      real * 8 p(0:3,nlegborn)
      integer bflav(nlegborn)
      real * 8 born
      
      logical initnlojet
      data initnlojet/.true./
      save initnlojet
      integer nsubproc
      parameter (nsubproc=4)
      integer virtptrn(nlegborn,nsubproc)
      real * 8 prefactor(nsubproc)
      save virtptrn,prefactor
      integer iptrn,i
      real * 8 pmom(0:4,nlegborn)
      real * 8 prefact

      if (initnlojet) then
c     g g g g g:
         virtptrn(1,1) =  0
         virtptrn(2,1) =  0
         virtptrn(3,1) =  0
         virtptrn(4,1) =  0
         virtptrn(5,1) =  0
         prefactor(1)  =  384d0
c     q q~ g g g:
         virtptrn(1,2) =  1
         virtptrn(2,2) = -1
         virtptrn(3,2) =  0
         virtptrn(4,2) =  0
         virtptrn(5,2) =  0
         prefactor(2)  =  54d0
c     q q~ q q~ g:
         virtptrn(1,3) =  1
         virtptrn(2,3) = -1
         virtptrn(3,3) =  1
         virtptrn(4,3) = -1
         virtptrn(5,3) =  0
         prefactor(3)  =  9d0
c     q q~ Q Q~ g:
         virtptrn(1,4) =  1
         virtptrn(2,4) = -1
         virtptrn(3,4) =  2
         virtptrn(4,4) = -2
         virtptrn(5,4) =  0
         prefactor(4)  =  9d0        
c     print out the patterns
         goto 100
         write(*,*) "**************************************************"
         write(*,*) "*                                                *"
         write(*,*) "*       Initializing the born patterns...     *"
         write(*,*) "*                                                *"
         write(*,*) "**************************************************"
         print *,"The patterns used for the born part: "
         call PrintPatterns(nlegborn,nsubproc,virtptrn)
 100     continue
         initnlojet = .false.         
      endif
      
      born = 0d0

c     fill up the pmom array
      do i=1,nlegborn
         pmom(0:3,i) = p(0:3,i)
         pmom(4,i)   = kn_masses(i)
      enddo
      
c     reshuffle the momenta and select the nlojet subprocess for the
c     current flavour configuration
      call reshufflemom(nlegborn,bflav,pmom,nsubproc,virtptrn,
     $     iptrn,prefact)
      

c     call the nlojet wrapper
      call nlojet3jetborn(iptrn-1,nlegborn,pmom,born)
      
c     fix all missing factors
      born = born*prefactor(iptrn)/prefact
      born = born*(4.d0*pi*st_alpha)**3

      end


