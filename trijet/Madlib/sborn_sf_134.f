      subroutine sborn_sf_134(p_born,wgtjk)
      implicit none
      include "nexternal.inc"
      double precision p_born(0:3,nexternal-1),wgt,
     &    wgtjk(nexternal-1,nexternal-1)
      integer m,n
 
      do m=1,nexternal-2
      do n=m+1,nexternal-1
 
      if ( m.eq.           1  .and. n.eq.           2  ) then
         call sb_sf_134_006(p_born,wgt)
      elseif ( m.eq.           1  .and. n.eq.           3  ) then
         call sb_sf_134_007(p_born,wgt)
      elseif ( m.eq.           1  .and. n.eq.           4  ) then
         call sb_sf_134_008(p_born,wgt)
      elseif ( m.eq.           1  .and. n.eq.           5  ) then
         call sb_sf_134_009(p_born,wgt)
      elseif ( m.eq.           2  .and. n.eq.           1  ) then
         call sb_sf_134_010(p_born,wgt)
      elseif ( m.eq.           2  .and. n.eq.           3  ) then
         call sb_sf_134_011(p_born,wgt)
      elseif ( m.eq.           2  .and. n.eq.           4  ) then
         call sb_sf_134_012(p_born,wgt)
      elseif ( m.eq.           2  .and. n.eq.           5  ) then
         call sb_sf_134_013(p_born,wgt)
      elseif ( m.eq.           3  .and. n.eq.           1  ) then
         call sb_sf_134_014(p_born,wgt)
      elseif ( m.eq.           3  .and. n.eq.           2  ) then
         call sb_sf_134_015(p_born,wgt)
      elseif ( m.eq.           3  .and. n.eq.           4  ) then
         call sb_sf_134_016(p_born,wgt)
      elseif ( m.eq.           3  .and. n.eq.           5  ) then
         call sb_sf_134_017(p_born,wgt)
      elseif ( m.eq.           4  .and. n.eq.           1  ) then
         call sb_sf_134_018(p_born,wgt)
      elseif ( m.eq.           4  .and. n.eq.           2  ) then
         call sb_sf_134_019(p_born,wgt)
      elseif ( m.eq.           4  .and. n.eq.           3  ) then
         call sb_sf_134_020(p_born,wgt)
      elseif ( m.eq.           4  .and. n.eq.           5  ) then
         call sb_sf_134_021(p_born,wgt)
      elseif ( m.eq.           5  .and. n.eq.           1  ) then
         call sb_sf_134_022(p_born,wgt)
      elseif ( m.eq.           5  .and. n.eq.           2  ) then
         call sb_sf_134_023(p_born,wgt)
      elseif ( m.eq.           5  .and. n.eq.           3  ) then
         call sb_sf_134_024(p_born,wgt)
      elseif ( m.eq.           5  .and. n.eq.           4  ) then
         call sb_sf_134_025(p_born,wgt)
      else
         wgt=0d0
c         write (*,*)
c     &      "No corresponding color linked Born found in sborn_sf"
      endif
 
      wgtjk(m,n)=-wgt
      wgtjk(n,m)=wgtjk(m,n)
      enddo ! loop over color-links
      enddo ! loop over color-links
 
      do m=1,nexternal-1
         wgtjk(m,m)=0d0
         do n=1,nexternal-1
            if (n.ne.m) wgtjk(m,m)=wgtjk(m,m)-wgtjk(n,m)
         enddo
      enddo
 
 
      return
      end
