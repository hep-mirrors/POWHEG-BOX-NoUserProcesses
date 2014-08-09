      subroutine sborn_proc(p_born,legs,wgt,wgtjk,wgtmunu)
      implicit none
      include "nexternal.inc"
      include "coupl.inc"
      double precision wgt
      double precision p_born(0:3,nexternal-1),wgt2(nexternal-1),
     &   wgtmunu(0:3,0:3,nexternal-1),wgtjk(nexternal-1,nexternal-1)
      double precision p_born1(0:3,nexternal-1),wgt1(0:nexternal-1),
     &   wgtmunu1(0:3,0:3,nexternal-1),wgtjk1(nexternal-1,nexternal-1)
      integer legs(nexternal-1),lstr,i
      character*140 str
      integer ic(nexternal-1),legs1(nexternal-1)
      logical mtc,even
      logical calculatedBorn
      integer skip
      common/cBorn/calculatedBorn,skip
 
      calculatedBorn=.false.
      
      do i=1,nexternal-1
         ic(i)=i
      enddo
      mtc=.false.
 10   call nexper(nexternal-1- 2,ic( 2+1),mtc,even)
      do i= 2+1,nexternal-1
         ic(i)=ic(i)+ 2
      enddo
      CALL SWITCHMOM (P_born,P_born1,IC,NEXTERNAL-1)
      CALL SWITCHLEGS(legs,legs1,IC,NEXTERNAL-1)
      
      call convert_to_string(nexternal-1,legs1,str,lstr)
      
      if (str.eq."-1-1-1-10") then
         call sborn_cl_001(p_born1,wgtmunu1,wgt1)
         call sborn_sf_001(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-111-10") then
         call sborn_cl_002(p_born1,wgtmunu1,wgt1)
         call sborn_sf_002(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-112-20") then
         call sborn_cl_003(p_born1,wgtmunu1,wgt1)
         call sborn_sf_003(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-114-40") then
         call sborn_cl_004(p_born1,wgtmunu1,wgt1)
         call sborn_sf_004(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-113-30") then
         call sborn_cl_005(p_born1,wgtmunu1,wgt1)
         call sborn_sf_005(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-115-50") then
         call sborn_cl_006(p_born1,wgtmunu1,wgt1)
         call sborn_sf_006(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-11000") then
         call sborn_cl_007(p_born1,wgtmunu1,wgt1)
         call sborn_sf_007(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-1-2-1-20") then
         call sborn_cl_008(p_born1,wgtmunu1,wgt1)
         call sborn_sf_008(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-12-120") then
         call sborn_cl_009(p_born1,wgtmunu1,wgt1)
         call sborn_sf_009(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-1-4-1-40") then
         call sborn_cl_010(p_born1,wgtmunu1,wgt1)
         call sborn_sf_010(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-14-140") then
         call sborn_cl_011(p_born1,wgtmunu1,wgt1)
         call sborn_sf_011(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-1-3-1-30") then
         call sborn_cl_012(p_born1,wgtmunu1,wgt1)
         call sborn_sf_012(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-13-130") then
         call sborn_cl_013(p_born1,wgtmunu1,wgt1)
         call sborn_sf_013(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-1-5-1-50") then
         call sborn_cl_014(p_born1,wgtmunu1,wgt1)
         call sborn_sf_014(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-15-150") then
         call sborn_cl_015(p_born1,wgtmunu1,wgt1)
         call sborn_sf_015(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-101-1-1") then
         call sborn_cl_016(p_born1,wgtmunu1,wgt1)
         call sborn_sf_016(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-10-12-2") then
         call sborn_cl_017(p_born1,wgtmunu1,wgt1)
         call sborn_sf_017(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-10-14-4") then
         call sborn_cl_018(p_born1,wgtmunu1,wgt1)
         call sborn_sf_018(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-10-13-3") then
         call sborn_cl_019(p_born1,wgtmunu1,wgt1)
         call sborn_sf_019(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-10-15-5") then
         call sborn_cl_020(p_born1,wgtmunu1,wgt1)
         call sborn_sf_020(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-10-100") then
         call sborn_cl_021(p_born1,wgtmunu1,wgt1)
         call sborn_sf_021(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1-11-10") then
         call sborn_cl_022(p_born1,wgtmunu1,wgt1)
         call sborn_sf_022(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1-12-20") then
         call sborn_cl_023(p_born1,wgtmunu1,wgt1)
         call sborn_sf_023(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1-14-40") then
         call sborn_cl_024(p_born1,wgtmunu1,wgt1)
         call sborn_sf_024(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1-13-30") then
         call sborn_cl_025(p_born1,wgtmunu1,wgt1)
         call sborn_sf_025(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1-15-50") then
         call sborn_cl_026(p_born1,wgtmunu1,wgt1)
         call sborn_sf_026(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1-1000") then
         call sborn_cl_027(p_born1,wgtmunu1,wgt1)
         call sborn_sf_027(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."11110") then
         call sborn_cl_028(p_born1,wgtmunu1,wgt1)
         call sborn_sf_028(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1-21-20") then
         call sborn_cl_029(p_born1,wgtmunu1,wgt1)
         call sborn_sf_029(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."12120") then
         call sborn_cl_030(p_born1,wgtmunu1,wgt1)
         call sborn_sf_030(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1-41-40") then
         call sborn_cl_031(p_born1,wgtmunu1,wgt1)
         call sborn_sf_031(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."14140") then
         call sborn_cl_032(p_born1,wgtmunu1,wgt1)
         call sborn_sf_032(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1-31-30") then
         call sborn_cl_033(p_born1,wgtmunu1,wgt1)
         call sborn_sf_033(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."13130") then
         call sborn_cl_034(p_born1,wgtmunu1,wgt1)
         call sborn_sf_034(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1-51-50") then
         call sborn_cl_035(p_born1,wgtmunu1,wgt1)
         call sborn_sf_035(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."15150") then
         call sborn_cl_036(p_born1,wgtmunu1,wgt1)
         call sborn_sf_036(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1011-1") then
         call sborn_cl_037(p_born1,wgtmunu1,wgt1)
         call sborn_sf_037(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1012-2") then
         call sborn_cl_038(p_born1,wgtmunu1,wgt1)
         call sborn_sf_038(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1014-4") then
         call sborn_cl_039(p_born1,wgtmunu1,wgt1)
         call sborn_sf_039(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1013-3") then
         call sborn_cl_040(p_born1,wgtmunu1,wgt1)
         call sborn_sf_040(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."1015-5") then
         call sborn_cl_041(p_born1,wgtmunu1,wgt1)
         call sborn_sf_041(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."10100") then
         call sborn_cl_042(p_born1,wgtmunu1,wgt1)
         call sborn_sf_042(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-2-1-1-20") then
         call sborn_cl_043(p_born1,wgtmunu1,wgt1)
         call sborn_sf_043(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-211-20") then
         call sborn_cl_044(p_born1,wgtmunu1,wgt1)
         call sborn_sf_044(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-2-2-2-20") then
         call sborn_cl_045(p_born1,wgtmunu1,wgt1)
         call sborn_sf_045(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-221-10") then
         call sborn_cl_046(p_born1,wgtmunu1,wgt1)
         call sborn_sf_046(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-222-20") then
         call sborn_cl_047(p_born1,wgtmunu1,wgt1)
         call sborn_sf_047(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-224-40") then
         call sborn_cl_048(p_born1,wgtmunu1,wgt1)
         call sborn_sf_048(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-223-30") then
         call sborn_cl_049(p_born1,wgtmunu1,wgt1)
         call sborn_sf_049(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-225-50") then
         call sborn_cl_050(p_born1,wgtmunu1,wgt1)
         call sborn_sf_050(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-22000") then
         call sborn_cl_051(p_born1,wgtmunu1,wgt1)
         call sborn_sf_051(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-2-4-2-40") then
         call sborn_cl_052(p_born1,wgtmunu1,wgt1)
         call sborn_sf_052(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-24-240") then
         call sborn_cl_053(p_born1,wgtmunu1,wgt1)
         call sborn_sf_053(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-2-3-2-30") then
         call sborn_cl_054(p_born1,wgtmunu1,wgt1)
         call sborn_sf_054(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-23-230") then
         call sborn_cl_055(p_born1,wgtmunu1,wgt1)
         call sborn_sf_055(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-2-5-2-50") then
         call sborn_cl_056(p_born1,wgtmunu1,wgt1)
         call sborn_sf_056(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-25-250") then
         call sborn_cl_057(p_born1,wgtmunu1,wgt1)
         call sborn_sf_057(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-201-1-2") then
         call sborn_cl_058(p_born1,wgtmunu1,wgt1)
         call sborn_sf_058(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-202-2-2") then
         call sborn_cl_059(p_born1,wgtmunu1,wgt1)
         call sborn_sf_059(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-20-24-4") then
         call sborn_cl_060(p_born1,wgtmunu1,wgt1)
         call sborn_sf_060(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-20-23-3") then
         call sborn_cl_061(p_born1,wgtmunu1,wgt1)
         call sborn_sf_061(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-20-25-5") then
         call sborn_cl_062(p_born1,wgtmunu1,wgt1)
         call sborn_sf_062(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-20-200") then
         call sborn_cl_063(p_born1,wgtmunu1,wgt1)
         call sborn_sf_063(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2-1-120") then
         call sborn_cl_064(p_born1,wgtmunu1,wgt1)
         call sborn_sf_064(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."21120") then
         call sborn_cl_065(p_born1,wgtmunu1,wgt1)
         call sborn_sf_065(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2-21-10") then
         call sborn_cl_066(p_born1,wgtmunu1,wgt1)
         call sborn_sf_066(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2-22-20") then
         call sborn_cl_067(p_born1,wgtmunu1,wgt1)
         call sborn_sf_067(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2-24-40") then
         call sborn_cl_068(p_born1,wgtmunu1,wgt1)
         call sborn_sf_068(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2-23-30") then
         call sborn_cl_069(p_born1,wgtmunu1,wgt1)
         call sborn_sf_069(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2-25-50") then
         call sborn_cl_070(p_born1,wgtmunu1,wgt1)
         call sborn_sf_070(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2-2000") then
         call sborn_cl_071(p_born1,wgtmunu1,wgt1)
         call sborn_sf_071(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."22220") then
         call sborn_cl_072(p_born1,wgtmunu1,wgt1)
         call sborn_sf_072(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2-42-40") then
         call sborn_cl_073(p_born1,wgtmunu1,wgt1)
         call sborn_sf_073(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."24240") then
         call sborn_cl_074(p_born1,wgtmunu1,wgt1)
         call sborn_sf_074(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2-32-30") then
         call sborn_cl_075(p_born1,wgtmunu1,wgt1)
         call sborn_sf_075(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."23230") then
         call sborn_cl_076(p_born1,wgtmunu1,wgt1)
         call sborn_sf_076(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2-52-50") then
         call sborn_cl_077(p_born1,wgtmunu1,wgt1)
         call sborn_sf_077(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."25250") then
         call sborn_cl_078(p_born1,wgtmunu1,wgt1)
         call sborn_sf_078(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."201-12") then
         call sborn_cl_079(p_born1,wgtmunu1,wgt1)
         call sborn_sf_079(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2022-2") then
         call sborn_cl_080(p_born1,wgtmunu1,wgt1)
         call sborn_sf_080(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2024-4") then
         call sborn_cl_081(p_born1,wgtmunu1,wgt1)
         call sborn_sf_081(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2023-3") then
         call sborn_cl_082(p_born1,wgtmunu1,wgt1)
         call sborn_sf_082(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."2025-5") then
         call sborn_cl_083(p_born1,wgtmunu1,wgt1)
         call sborn_sf_083(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."20200") then
         call sborn_cl_084(p_born1,wgtmunu1,wgt1)
         call sborn_sf_084(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-4-1-1-40") then
         call sborn_cl_085(p_born1,wgtmunu1,wgt1)
         call sborn_sf_085(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-411-40") then
         call sborn_cl_086(p_born1,wgtmunu1,wgt1)
         call sborn_sf_086(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-4-2-2-40") then
         call sborn_cl_087(p_born1,wgtmunu1,wgt1)
         call sborn_sf_087(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-422-40") then
         call sborn_cl_088(p_born1,wgtmunu1,wgt1)
         call sborn_sf_088(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-4-4-4-40") then
         call sborn_cl_089(p_born1,wgtmunu1,wgt1)
         call sborn_sf_089(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-441-10") then
         call sborn_cl_090(p_born1,wgtmunu1,wgt1)
         call sborn_sf_090(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-442-20") then
         call sborn_cl_091(p_born1,wgtmunu1,wgt1)
         call sborn_sf_091(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-444-40") then
         call sborn_cl_092(p_born1,wgtmunu1,wgt1)
         call sborn_sf_092(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-443-30") then
         call sborn_cl_093(p_born1,wgtmunu1,wgt1)
         call sborn_sf_093(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-445-50") then
         call sborn_cl_094(p_born1,wgtmunu1,wgt1)
         call sborn_sf_094(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-44000") then
         call sborn_cl_095(p_born1,wgtmunu1,wgt1)
         call sborn_sf_095(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-4-3-4-30") then
         call sborn_cl_096(p_born1,wgtmunu1,wgt1)
         call sborn_sf_096(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-43-430") then
         call sborn_cl_097(p_born1,wgtmunu1,wgt1)
         call sborn_sf_097(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-4-5-4-50") then
         call sborn_cl_098(p_born1,wgtmunu1,wgt1)
         call sborn_sf_098(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-45-450") then
         call sborn_cl_099(p_born1,wgtmunu1,wgt1)
         call sborn_sf_099(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-401-1-4") then
         call sborn_cl_100(p_born1,wgtmunu1,wgt1)
         call sborn_sf_100(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-402-2-4") then
         call sborn_cl_101(p_born1,wgtmunu1,wgt1)
         call sborn_sf_101(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-404-4-4") then
         call sborn_cl_102(p_born1,wgtmunu1,wgt1)
         call sborn_sf_102(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-40-43-3") then
         call sborn_cl_103(p_born1,wgtmunu1,wgt1)
         call sborn_sf_103(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-40-45-5") then
         call sborn_cl_104(p_born1,wgtmunu1,wgt1)
         call sborn_sf_104(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-40-400") then
         call sborn_cl_105(p_born1,wgtmunu1,wgt1)
         call sborn_sf_105(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4-1-140") then
         call sborn_cl_106(p_born1,wgtmunu1,wgt1)
         call sborn_sf_106(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."41140") then
         call sborn_cl_107(p_born1,wgtmunu1,wgt1)
         call sborn_sf_107(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4-2-240") then
         call sborn_cl_108(p_born1,wgtmunu1,wgt1)
         call sborn_sf_108(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."42240") then
         call sborn_cl_109(p_born1,wgtmunu1,wgt1)
         call sborn_sf_109(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4-41-10") then
         call sborn_cl_110(p_born1,wgtmunu1,wgt1)
         call sborn_sf_110(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4-42-20") then
         call sborn_cl_111(p_born1,wgtmunu1,wgt1)
         call sborn_sf_111(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4-44-40") then
         call sborn_cl_112(p_born1,wgtmunu1,wgt1)
         call sborn_sf_112(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4-43-30") then
         call sborn_cl_113(p_born1,wgtmunu1,wgt1)
         call sborn_sf_113(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4-45-50") then
         call sborn_cl_114(p_born1,wgtmunu1,wgt1)
         call sborn_sf_114(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4-4000") then
         call sborn_cl_115(p_born1,wgtmunu1,wgt1)
         call sborn_sf_115(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."44440") then
         call sborn_cl_116(p_born1,wgtmunu1,wgt1)
         call sborn_sf_116(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4-34-30") then
         call sborn_cl_117(p_born1,wgtmunu1,wgt1)
         call sborn_sf_117(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."43430") then
         call sborn_cl_118(p_born1,wgtmunu1,wgt1)
         call sborn_sf_118(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4-54-50") then
         call sborn_cl_119(p_born1,wgtmunu1,wgt1)
         call sborn_sf_119(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."45450") then
         call sborn_cl_120(p_born1,wgtmunu1,wgt1)
         call sborn_sf_120(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."401-14") then
         call sborn_cl_121(p_born1,wgtmunu1,wgt1)
         call sborn_sf_121(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."402-24") then
         call sborn_cl_122(p_born1,wgtmunu1,wgt1)
         call sborn_sf_122(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4044-4") then
         call sborn_cl_123(p_born1,wgtmunu1,wgt1)
         call sborn_sf_123(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4043-3") then
         call sborn_cl_124(p_born1,wgtmunu1,wgt1)
         call sborn_sf_124(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."4045-5") then
         call sborn_cl_125(p_born1,wgtmunu1,wgt1)
         call sborn_sf_125(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."40400") then
         call sborn_cl_126(p_born1,wgtmunu1,wgt1)
         call sborn_sf_126(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-3-1-1-30") then
         call sborn_cl_127(p_born1,wgtmunu1,wgt1)
         call sborn_sf_127(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-311-30") then
         call sborn_cl_128(p_born1,wgtmunu1,wgt1)
         call sborn_sf_128(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-3-2-2-30") then
         call sborn_cl_129(p_born1,wgtmunu1,wgt1)
         call sborn_sf_129(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-322-30") then
         call sborn_cl_130(p_born1,wgtmunu1,wgt1)
         call sborn_sf_130(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-3-4-4-30") then
         call sborn_cl_131(p_born1,wgtmunu1,wgt1)
         call sborn_sf_131(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-344-30") then
         call sborn_cl_132(p_born1,wgtmunu1,wgt1)
         call sborn_sf_132(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-3-3-3-30") then
         call sborn_cl_133(p_born1,wgtmunu1,wgt1)
         call sborn_sf_133(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-331-10") then
         call sborn_cl_134(p_born1,wgtmunu1,wgt1)
         call sborn_sf_134(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-332-20") then
         call sborn_cl_135(p_born1,wgtmunu1,wgt1)
         call sborn_sf_135(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-334-40") then
         call sborn_cl_136(p_born1,wgtmunu1,wgt1)
         call sborn_sf_136(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-333-30") then
         call sborn_cl_137(p_born1,wgtmunu1,wgt1)
         call sborn_sf_137(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-335-50") then
         call sborn_cl_138(p_born1,wgtmunu1,wgt1)
         call sborn_sf_138(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-33000") then
         call sborn_cl_139(p_born1,wgtmunu1,wgt1)
         call sborn_sf_139(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-3-5-3-50") then
         call sborn_cl_140(p_born1,wgtmunu1,wgt1)
         call sborn_sf_140(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-35-350") then
         call sborn_cl_141(p_born1,wgtmunu1,wgt1)
         call sborn_sf_141(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-301-1-3") then
         call sborn_cl_142(p_born1,wgtmunu1,wgt1)
         call sborn_sf_142(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-302-2-3") then
         call sborn_cl_143(p_born1,wgtmunu1,wgt1)
         call sborn_sf_143(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-304-4-3") then
         call sborn_cl_144(p_born1,wgtmunu1,wgt1)
         call sborn_sf_144(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-303-3-3") then
         call sborn_cl_145(p_born1,wgtmunu1,wgt1)
         call sborn_sf_145(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-30-35-5") then
         call sborn_cl_146(p_born1,wgtmunu1,wgt1)
         call sborn_sf_146(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-30-300") then
         call sborn_cl_147(p_born1,wgtmunu1,wgt1)
         call sborn_sf_147(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."3-1-130") then
         call sborn_cl_148(p_born1,wgtmunu1,wgt1)
         call sborn_sf_148(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."31130") then
         call sborn_cl_149(p_born1,wgtmunu1,wgt1)
         call sborn_sf_149(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."3-2-230") then
         call sborn_cl_150(p_born1,wgtmunu1,wgt1)
         call sborn_sf_150(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."32230") then
         call sborn_cl_151(p_born1,wgtmunu1,wgt1)
         call sborn_sf_151(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."3-4-430") then
         call sborn_cl_152(p_born1,wgtmunu1,wgt1)
         call sborn_sf_152(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."34430") then
         call sborn_cl_153(p_born1,wgtmunu1,wgt1)
         call sborn_sf_153(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."3-31-10") then
         call sborn_cl_154(p_born1,wgtmunu1,wgt1)
         call sborn_sf_154(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."3-32-20") then
         call sborn_cl_155(p_born1,wgtmunu1,wgt1)
         call sborn_sf_155(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."3-34-40") then
         call sborn_cl_156(p_born1,wgtmunu1,wgt1)
         call sborn_sf_156(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."3-33-30") then
         call sborn_cl_157(p_born1,wgtmunu1,wgt1)
         call sborn_sf_157(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."3-35-50") then
         call sborn_cl_158(p_born1,wgtmunu1,wgt1)
         call sborn_sf_158(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."3-3000") then
         call sborn_cl_159(p_born1,wgtmunu1,wgt1)
         call sborn_sf_159(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."33330") then
         call sborn_cl_160(p_born1,wgtmunu1,wgt1)
         call sborn_sf_160(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."3-53-50") then
         call sborn_cl_161(p_born1,wgtmunu1,wgt1)
         call sborn_sf_161(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."35350") then
         call sborn_cl_162(p_born1,wgtmunu1,wgt1)
         call sborn_sf_162(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."301-13") then
         call sborn_cl_163(p_born1,wgtmunu1,wgt1)
         call sborn_sf_163(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."302-23") then
         call sborn_cl_164(p_born1,wgtmunu1,wgt1)
         call sborn_sf_164(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."304-43") then
         call sborn_cl_165(p_born1,wgtmunu1,wgt1)
         call sborn_sf_165(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."3033-3") then
         call sborn_cl_166(p_born1,wgtmunu1,wgt1)
         call sborn_sf_166(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."3035-5") then
         call sborn_cl_167(p_born1,wgtmunu1,wgt1)
         call sborn_sf_167(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."30300") then
         call sborn_cl_168(p_born1,wgtmunu1,wgt1)
         call sborn_sf_168(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-5-1-1-50") then
         call sborn_cl_169(p_born1,wgtmunu1,wgt1)
         call sborn_sf_169(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-511-50") then
         call sborn_cl_170(p_born1,wgtmunu1,wgt1)
         call sborn_sf_170(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-5-2-2-50") then
         call sborn_cl_171(p_born1,wgtmunu1,wgt1)
         call sborn_sf_171(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-522-50") then
         call sborn_cl_172(p_born1,wgtmunu1,wgt1)
         call sborn_sf_172(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-5-4-4-50") then
         call sborn_cl_173(p_born1,wgtmunu1,wgt1)
         call sborn_sf_173(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-544-50") then
         call sborn_cl_174(p_born1,wgtmunu1,wgt1)
         call sborn_sf_174(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-5-3-3-50") then
         call sborn_cl_175(p_born1,wgtmunu1,wgt1)
         call sborn_sf_175(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-533-50") then
         call sborn_cl_176(p_born1,wgtmunu1,wgt1)
         call sborn_sf_176(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-5-5-5-50") then
         call sborn_cl_177(p_born1,wgtmunu1,wgt1)
         call sborn_sf_177(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-551-10") then
         call sborn_cl_178(p_born1,wgtmunu1,wgt1)
         call sborn_sf_178(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-552-20") then
         call sborn_cl_179(p_born1,wgtmunu1,wgt1)
         call sborn_sf_179(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-554-40") then
         call sborn_cl_180(p_born1,wgtmunu1,wgt1)
         call sborn_sf_180(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-553-30") then
         call sborn_cl_181(p_born1,wgtmunu1,wgt1)
         call sborn_sf_181(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-555-50") then
         call sborn_cl_182(p_born1,wgtmunu1,wgt1)
         call sborn_sf_182(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-55000") then
         call sborn_cl_183(p_born1,wgtmunu1,wgt1)
         call sborn_sf_183(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-501-1-5") then
         call sborn_cl_184(p_born1,wgtmunu1,wgt1)
         call sborn_sf_184(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-502-2-5") then
         call sborn_cl_185(p_born1,wgtmunu1,wgt1)
         call sborn_sf_185(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-504-4-5") then
         call sborn_cl_186(p_born1,wgtmunu1,wgt1)
         call sborn_sf_186(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-503-3-5") then
         call sborn_cl_187(p_born1,wgtmunu1,wgt1)
         call sborn_sf_187(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-505-5-5") then
         call sborn_cl_188(p_born1,wgtmunu1,wgt1)
         call sborn_sf_188(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."-50-500") then
         call sborn_cl_189(p_born1,wgtmunu1,wgt1)
         call sborn_sf_189(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."5-1-150") then
         call sborn_cl_190(p_born1,wgtmunu1,wgt1)
         call sborn_sf_190(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."51150") then
         call sborn_cl_191(p_born1,wgtmunu1,wgt1)
         call sborn_sf_191(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."5-2-250") then
         call sborn_cl_192(p_born1,wgtmunu1,wgt1)
         call sborn_sf_192(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."52250") then
         call sborn_cl_193(p_born1,wgtmunu1,wgt1)
         call sborn_sf_193(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."5-4-450") then
         call sborn_cl_194(p_born1,wgtmunu1,wgt1)
         call sborn_sf_194(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."54450") then
         call sborn_cl_195(p_born1,wgtmunu1,wgt1)
         call sborn_sf_195(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."5-3-350") then
         call sborn_cl_196(p_born1,wgtmunu1,wgt1)
         call sborn_sf_196(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."53350") then
         call sborn_cl_197(p_born1,wgtmunu1,wgt1)
         call sborn_sf_197(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."5-51-10") then
         call sborn_cl_198(p_born1,wgtmunu1,wgt1)
         call sborn_sf_198(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."5-52-20") then
         call sborn_cl_199(p_born1,wgtmunu1,wgt1)
         call sborn_sf_199(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."5-54-40") then
         call sborn_cl_200(p_born1,wgtmunu1,wgt1)
         call sborn_sf_200(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."5-53-30") then
         call sborn_cl_201(p_born1,wgtmunu1,wgt1)
         call sborn_sf_201(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."5-55-50") then
         call sborn_cl_202(p_born1,wgtmunu1,wgt1)
         call sborn_sf_202(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."5-5000") then
         call sborn_cl_203(p_born1,wgtmunu1,wgt1)
         call sborn_sf_203(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."55550") then
         call sborn_cl_204(p_born1,wgtmunu1,wgt1)
         call sborn_sf_204(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."501-15") then
         call sborn_cl_205(p_born1,wgtmunu1,wgt1)
         call sborn_sf_205(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."502-25") then
         call sborn_cl_206(p_born1,wgtmunu1,wgt1)
         call sborn_sf_206(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."504-45") then
         call sborn_cl_207(p_born1,wgtmunu1,wgt1)
         call sborn_sf_207(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."503-35") then
         call sborn_cl_208(p_born1,wgtmunu1,wgt1)
         call sborn_sf_208(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."5055-5") then
         call sborn_cl_209(p_born1,wgtmunu1,wgt1)
         call sborn_sf_209(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."50500") then
         call sborn_cl_210(p_born1,wgtmunu1,wgt1)
         call sborn_sf_210(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-11-1-1") then
         call sborn_cl_211(p_born1,wgtmunu1,wgt1)
         call sborn_sf_211(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-1-12-2") then
         call sborn_cl_212(p_born1,wgtmunu1,wgt1)
         call sborn_sf_212(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-1-14-4") then
         call sborn_cl_213(p_born1,wgtmunu1,wgt1)
         call sborn_sf_213(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-1-13-3") then
         call sborn_cl_214(p_born1,wgtmunu1,wgt1)
         call sborn_sf_214(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-1-15-5") then
         call sborn_cl_215(p_born1,wgtmunu1,wgt1)
         call sborn_sf_215(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-1-100") then
         call sborn_cl_216(p_born1,wgtmunu1,wgt1)
         call sborn_sf_216(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0111-1") then
         call sborn_cl_217(p_born1,wgtmunu1,wgt1)
         call sborn_sf_217(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0112-2") then
         call sborn_cl_218(p_born1,wgtmunu1,wgt1)
         call sborn_sf_218(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0114-4") then
         call sborn_cl_219(p_born1,wgtmunu1,wgt1)
         call sborn_sf_219(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0113-3") then
         call sborn_cl_220(p_born1,wgtmunu1,wgt1)
         call sborn_sf_220(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0115-5") then
         call sborn_cl_221(p_born1,wgtmunu1,wgt1)
         call sborn_sf_221(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."01100") then
         call sborn_cl_222(p_born1,wgtmunu1,wgt1)
         call sborn_sf_222(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-21-1-2") then
         call sborn_cl_223(p_born1,wgtmunu1,wgt1)
         call sborn_sf_223(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-22-2-2") then
         call sborn_cl_224(p_born1,wgtmunu1,wgt1)
         call sborn_sf_224(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-2-24-4") then
         call sborn_cl_225(p_born1,wgtmunu1,wgt1)
         call sborn_sf_225(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-2-23-3") then
         call sborn_cl_226(p_born1,wgtmunu1,wgt1)
         call sborn_sf_226(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-2-25-5") then
         call sborn_cl_227(p_born1,wgtmunu1,wgt1)
         call sborn_sf_227(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-2-200") then
         call sborn_cl_228(p_born1,wgtmunu1,wgt1)
         call sborn_sf_228(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."021-12") then
         call sborn_cl_229(p_born1,wgtmunu1,wgt1)
         call sborn_sf_229(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0222-2") then
         call sborn_cl_230(p_born1,wgtmunu1,wgt1)
         call sborn_sf_230(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0224-4") then
         call sborn_cl_231(p_born1,wgtmunu1,wgt1)
         call sborn_sf_231(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0223-3") then
         call sborn_cl_232(p_born1,wgtmunu1,wgt1)
         call sborn_sf_232(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0225-5") then
         call sborn_cl_233(p_born1,wgtmunu1,wgt1)
         call sborn_sf_233(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."02200") then
         call sborn_cl_234(p_born1,wgtmunu1,wgt1)
         call sborn_sf_234(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-41-1-4") then
         call sborn_cl_235(p_born1,wgtmunu1,wgt1)
         call sborn_sf_235(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-42-2-4") then
         call sborn_cl_236(p_born1,wgtmunu1,wgt1)
         call sborn_sf_236(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-44-4-4") then
         call sborn_cl_237(p_born1,wgtmunu1,wgt1)
         call sborn_sf_237(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-4-43-3") then
         call sborn_cl_238(p_born1,wgtmunu1,wgt1)
         call sborn_sf_238(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-4-45-5") then
         call sborn_cl_239(p_born1,wgtmunu1,wgt1)
         call sborn_sf_239(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-4-400") then
         call sborn_cl_240(p_born1,wgtmunu1,wgt1)
         call sborn_sf_240(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."041-14") then
         call sborn_cl_241(p_born1,wgtmunu1,wgt1)
         call sborn_sf_241(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."042-24") then
         call sborn_cl_242(p_born1,wgtmunu1,wgt1)
         call sborn_sf_242(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0444-4") then
         call sborn_cl_243(p_born1,wgtmunu1,wgt1)
         call sborn_sf_243(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0443-3") then
         call sborn_cl_244(p_born1,wgtmunu1,wgt1)
         call sborn_sf_244(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0445-5") then
         call sborn_cl_245(p_born1,wgtmunu1,wgt1)
         call sborn_sf_245(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."04400") then
         call sborn_cl_246(p_born1,wgtmunu1,wgt1)
         call sborn_sf_246(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-31-1-3") then
         call sborn_cl_247(p_born1,wgtmunu1,wgt1)
         call sborn_sf_247(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-32-2-3") then
         call sborn_cl_248(p_born1,wgtmunu1,wgt1)
         call sborn_sf_248(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-34-4-3") then
         call sborn_cl_249(p_born1,wgtmunu1,wgt1)
         call sborn_sf_249(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-33-3-3") then
         call sborn_cl_250(p_born1,wgtmunu1,wgt1)
         call sborn_sf_250(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-3-35-5") then
         call sborn_cl_251(p_born1,wgtmunu1,wgt1)
         call sborn_sf_251(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-3-300") then
         call sborn_cl_252(p_born1,wgtmunu1,wgt1)
         call sborn_sf_252(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."031-13") then
         call sborn_cl_253(p_born1,wgtmunu1,wgt1)
         call sborn_sf_253(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."032-23") then
         call sborn_cl_254(p_born1,wgtmunu1,wgt1)
         call sborn_sf_254(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."034-43") then
         call sborn_cl_255(p_born1,wgtmunu1,wgt1)
         call sborn_sf_255(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0333-3") then
         call sborn_cl_256(p_born1,wgtmunu1,wgt1)
         call sborn_sf_256(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0335-5") then
         call sborn_cl_257(p_born1,wgtmunu1,wgt1)
         call sborn_sf_257(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."03300") then
         call sborn_cl_258(p_born1,wgtmunu1,wgt1)
         call sborn_sf_258(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-51-1-5") then
         call sborn_cl_259(p_born1,wgtmunu1,wgt1)
         call sborn_sf_259(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-52-2-5") then
         call sborn_cl_260(p_born1,wgtmunu1,wgt1)
         call sborn_sf_260(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-54-4-5") then
         call sborn_cl_261(p_born1,wgtmunu1,wgt1)
         call sborn_sf_261(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-53-3-5") then
         call sborn_cl_262(p_born1,wgtmunu1,wgt1)
         call sborn_sf_262(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-55-5-5") then
         call sborn_cl_263(p_born1,wgtmunu1,wgt1)
         call sborn_sf_263(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0-5-500") then
         call sborn_cl_264(p_born1,wgtmunu1,wgt1)
         call sborn_sf_264(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."051-15") then
         call sborn_cl_265(p_born1,wgtmunu1,wgt1)
         call sborn_sf_265(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."052-25") then
         call sborn_cl_266(p_born1,wgtmunu1,wgt1)
         call sborn_sf_266(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."054-45") then
         call sborn_cl_267(p_born1,wgtmunu1,wgt1)
         call sborn_sf_267(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."053-35") then
         call sborn_cl_268(p_born1,wgtmunu1,wgt1)
         call sborn_sf_268(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."0555-5") then
         call sborn_cl_269(p_born1,wgtmunu1,wgt1)
         call sborn_sf_269(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."05500") then
         call sborn_cl_270(p_born1,wgtmunu1,wgt1)
         call sborn_sf_270(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."001-10") then
         call sborn_cl_271(p_born1,wgtmunu1,wgt1)
         call sborn_sf_271(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."002-20") then
         call sborn_cl_272(p_born1,wgtmunu1,wgt1)
         call sborn_sf_272(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."004-40") then
         call sborn_cl_273(p_born1,wgtmunu1,wgt1)
         call sborn_sf_273(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."003-30") then
         call sborn_cl_274(p_born1,wgtmunu1,wgt1)
         call sborn_sf_274(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."005-50") then
         call sborn_cl_275(p_born1,wgtmunu1,wgt1)
         call sborn_sf_275(p_born1,wgtjk1)
         goto 20
      elseif (str.eq."00000") then
         call sborn_cl_276(p_born1,wgtmunu1,wgt1)
         call sborn_sf_276(p_born1,wgtjk1)
         goto 20
      endif
      
      do while(mtc)
         do i= 2+1,nexternal-1
            ic(i)=ic(i)- 2
         enddo
         goto 10
      enddo
      if (.not.mtc) then
         write (*,*) "Error #1, in sborn_proc.f"
         stop
      endif
      
 20   wgt=0d0
      call switchborns(wgt1(1),wgt2,wgtjk1,wgtjk,wgtmunu1,wgtmunu,
     &     ic,nexternal-1)
      do i=1,nexternal-1
         if(wgt.eq.0d0 .and. wgt2(i).ne.0d0) then
            wgt=wgt2(i)
         elseif (wgt.ne.0d0 .and. wgt2(i).ne.0d0 .and.
     &           abs((wgt-wgt2(i))/wgt).gt.1d-7 ) then
            write (*,*) "Error #2 in sborn_proc ",i,wgt2
            stop
         endif
      enddo
      
      end
      
      
      
      
      
      subroutine born_color(legs,color)
      implicit none
      include "nexternal.inc"
      integer maxamps
      parameter (maxamps=6000)
      Double Precision amp2001(maxamps), jamp2001(0:maxamps)
      common/to_amps_001/amp2001,jamp2001
      Double Precision amp2002(maxamps), jamp2002(0:maxamps)
      common/to_amps_002/amp2002,jamp2002
      Double Precision amp2003(maxamps), jamp2003(0:maxamps)
      common/to_amps_003/amp2003,jamp2003
      Double Precision amp2004(maxamps), jamp2004(0:maxamps)
      common/to_amps_004/amp2004,jamp2004
      Double Precision amp2005(maxamps), jamp2005(0:maxamps)
      common/to_amps_005/amp2005,jamp2005
      Double Precision amp2006(maxamps), jamp2006(0:maxamps)
      common/to_amps_006/amp2006,jamp2006
      Double Precision amp2007(maxamps), jamp2007(0:maxamps)
      common/to_amps_007/amp2007,jamp2007
      Double Precision amp2008(maxamps), jamp2008(0:maxamps)
      common/to_amps_008/amp2008,jamp2008
      Double Precision amp2009(maxamps), jamp2009(0:maxamps)
      common/to_amps_009/amp2009,jamp2009
      Double Precision amp2010(maxamps), jamp2010(0:maxamps)
      common/to_amps_010/amp2010,jamp2010
      Double Precision amp2011(maxamps), jamp2011(0:maxamps)
      common/to_amps_011/amp2011,jamp2011
      Double Precision amp2012(maxamps), jamp2012(0:maxamps)
      common/to_amps_012/amp2012,jamp2012
      Double Precision amp2013(maxamps), jamp2013(0:maxamps)
      common/to_amps_013/amp2013,jamp2013
      Double Precision amp2014(maxamps), jamp2014(0:maxamps)
      common/to_amps_014/amp2014,jamp2014
      Double Precision amp2015(maxamps), jamp2015(0:maxamps)
      common/to_amps_015/amp2015,jamp2015
      Double Precision amp2016(maxamps), jamp2016(0:maxamps)
      common/to_amps_016/amp2016,jamp2016
      Double Precision amp2017(maxamps), jamp2017(0:maxamps)
      common/to_amps_017/amp2017,jamp2017
      Double Precision amp2018(maxamps), jamp2018(0:maxamps)
      common/to_amps_018/amp2018,jamp2018
      Double Precision amp2019(maxamps), jamp2019(0:maxamps)
      common/to_amps_019/amp2019,jamp2019
      Double Precision amp2020(maxamps), jamp2020(0:maxamps)
      common/to_amps_020/amp2020,jamp2020
      Double Precision amp2021(maxamps), jamp2021(0:maxamps)
      common/to_amps_021/amp2021,jamp2021
      Double Precision amp2022(maxamps), jamp2022(0:maxamps)
      common/to_amps_022/amp2022,jamp2022
      Double Precision amp2023(maxamps), jamp2023(0:maxamps)
      common/to_amps_023/amp2023,jamp2023
      Double Precision amp2024(maxamps), jamp2024(0:maxamps)
      common/to_amps_024/amp2024,jamp2024
      Double Precision amp2025(maxamps), jamp2025(0:maxamps)
      common/to_amps_025/amp2025,jamp2025
      Double Precision amp2026(maxamps), jamp2026(0:maxamps)
      common/to_amps_026/amp2026,jamp2026
      Double Precision amp2027(maxamps), jamp2027(0:maxamps)
      common/to_amps_027/amp2027,jamp2027
      Double Precision amp2028(maxamps), jamp2028(0:maxamps)
      common/to_amps_028/amp2028,jamp2028
      Double Precision amp2029(maxamps), jamp2029(0:maxamps)
      common/to_amps_029/amp2029,jamp2029
      Double Precision amp2030(maxamps), jamp2030(0:maxamps)
      common/to_amps_030/amp2030,jamp2030
      Double Precision amp2031(maxamps), jamp2031(0:maxamps)
      common/to_amps_031/amp2031,jamp2031
      Double Precision amp2032(maxamps), jamp2032(0:maxamps)
      common/to_amps_032/amp2032,jamp2032
      Double Precision amp2033(maxamps), jamp2033(0:maxamps)
      common/to_amps_033/amp2033,jamp2033
      Double Precision amp2034(maxamps), jamp2034(0:maxamps)
      common/to_amps_034/amp2034,jamp2034
      Double Precision amp2035(maxamps), jamp2035(0:maxamps)
      common/to_amps_035/amp2035,jamp2035
      Double Precision amp2036(maxamps), jamp2036(0:maxamps)
      common/to_amps_036/amp2036,jamp2036
      Double Precision amp2037(maxamps), jamp2037(0:maxamps)
      common/to_amps_037/amp2037,jamp2037
      Double Precision amp2038(maxamps), jamp2038(0:maxamps)
      common/to_amps_038/amp2038,jamp2038
      Double Precision amp2039(maxamps), jamp2039(0:maxamps)
      common/to_amps_039/amp2039,jamp2039
      Double Precision amp2040(maxamps), jamp2040(0:maxamps)
      common/to_amps_040/amp2040,jamp2040
      Double Precision amp2041(maxamps), jamp2041(0:maxamps)
      common/to_amps_041/amp2041,jamp2041
      Double Precision amp2042(maxamps), jamp2042(0:maxamps)
      common/to_amps_042/amp2042,jamp2042
      Double Precision amp2043(maxamps), jamp2043(0:maxamps)
      common/to_amps_043/amp2043,jamp2043
      Double Precision amp2044(maxamps), jamp2044(0:maxamps)
      common/to_amps_044/amp2044,jamp2044
      Double Precision amp2045(maxamps), jamp2045(0:maxamps)
      common/to_amps_045/amp2045,jamp2045
      Double Precision amp2046(maxamps), jamp2046(0:maxamps)
      common/to_amps_046/amp2046,jamp2046
      Double Precision amp2047(maxamps), jamp2047(0:maxamps)
      common/to_amps_047/amp2047,jamp2047
      Double Precision amp2048(maxamps), jamp2048(0:maxamps)
      common/to_amps_048/amp2048,jamp2048
      Double Precision amp2049(maxamps), jamp2049(0:maxamps)
      common/to_amps_049/amp2049,jamp2049
      Double Precision amp2050(maxamps), jamp2050(0:maxamps)
      common/to_amps_050/amp2050,jamp2050
      Double Precision amp2051(maxamps), jamp2051(0:maxamps)
      common/to_amps_051/amp2051,jamp2051
      Double Precision amp2052(maxamps), jamp2052(0:maxamps)
      common/to_amps_052/amp2052,jamp2052
      Double Precision amp2053(maxamps), jamp2053(0:maxamps)
      common/to_amps_053/amp2053,jamp2053
      Double Precision amp2054(maxamps), jamp2054(0:maxamps)
      common/to_amps_054/amp2054,jamp2054
      Double Precision amp2055(maxamps), jamp2055(0:maxamps)
      common/to_amps_055/amp2055,jamp2055
      Double Precision amp2056(maxamps), jamp2056(0:maxamps)
      common/to_amps_056/amp2056,jamp2056
      Double Precision amp2057(maxamps), jamp2057(0:maxamps)
      common/to_amps_057/amp2057,jamp2057
      Double Precision amp2058(maxamps), jamp2058(0:maxamps)
      common/to_amps_058/amp2058,jamp2058
      Double Precision amp2059(maxamps), jamp2059(0:maxamps)
      common/to_amps_059/amp2059,jamp2059
      Double Precision amp2060(maxamps), jamp2060(0:maxamps)
      common/to_amps_060/amp2060,jamp2060
      Double Precision amp2061(maxamps), jamp2061(0:maxamps)
      common/to_amps_061/amp2061,jamp2061
      Double Precision amp2062(maxamps), jamp2062(0:maxamps)
      common/to_amps_062/amp2062,jamp2062
      Double Precision amp2063(maxamps), jamp2063(0:maxamps)
      common/to_amps_063/amp2063,jamp2063
      Double Precision amp2064(maxamps), jamp2064(0:maxamps)
      common/to_amps_064/amp2064,jamp2064
      Double Precision amp2065(maxamps), jamp2065(0:maxamps)
      common/to_amps_065/amp2065,jamp2065
      Double Precision amp2066(maxamps), jamp2066(0:maxamps)
      common/to_amps_066/amp2066,jamp2066
      Double Precision amp2067(maxamps), jamp2067(0:maxamps)
      common/to_amps_067/amp2067,jamp2067
      Double Precision amp2068(maxamps), jamp2068(0:maxamps)
      common/to_amps_068/amp2068,jamp2068
      Double Precision amp2069(maxamps), jamp2069(0:maxamps)
      common/to_amps_069/amp2069,jamp2069
      Double Precision amp2070(maxamps), jamp2070(0:maxamps)
      common/to_amps_070/amp2070,jamp2070
      Double Precision amp2071(maxamps), jamp2071(0:maxamps)
      common/to_amps_071/amp2071,jamp2071
      Double Precision amp2072(maxamps), jamp2072(0:maxamps)
      common/to_amps_072/amp2072,jamp2072
      Double Precision amp2073(maxamps), jamp2073(0:maxamps)
      common/to_amps_073/amp2073,jamp2073
      Double Precision amp2074(maxamps), jamp2074(0:maxamps)
      common/to_amps_074/amp2074,jamp2074
      Double Precision amp2075(maxamps), jamp2075(0:maxamps)
      common/to_amps_075/amp2075,jamp2075
      Double Precision amp2076(maxamps), jamp2076(0:maxamps)
      common/to_amps_076/amp2076,jamp2076
      Double Precision amp2077(maxamps), jamp2077(0:maxamps)
      common/to_amps_077/amp2077,jamp2077
      Double Precision amp2078(maxamps), jamp2078(0:maxamps)
      common/to_amps_078/amp2078,jamp2078
      Double Precision amp2079(maxamps), jamp2079(0:maxamps)
      common/to_amps_079/amp2079,jamp2079
      Double Precision amp2080(maxamps), jamp2080(0:maxamps)
      common/to_amps_080/amp2080,jamp2080
      Double Precision amp2081(maxamps), jamp2081(0:maxamps)
      common/to_amps_081/amp2081,jamp2081
      Double Precision amp2082(maxamps), jamp2082(0:maxamps)
      common/to_amps_082/amp2082,jamp2082
      Double Precision amp2083(maxamps), jamp2083(0:maxamps)
      common/to_amps_083/amp2083,jamp2083
      Double Precision amp2084(maxamps), jamp2084(0:maxamps)
      common/to_amps_084/amp2084,jamp2084
      Double Precision amp2085(maxamps), jamp2085(0:maxamps)
      common/to_amps_085/amp2085,jamp2085
      Double Precision amp2086(maxamps), jamp2086(0:maxamps)
      common/to_amps_086/amp2086,jamp2086
      Double Precision amp2087(maxamps), jamp2087(0:maxamps)
      common/to_amps_087/amp2087,jamp2087
      Double Precision amp2088(maxamps), jamp2088(0:maxamps)
      common/to_amps_088/amp2088,jamp2088
      Double Precision amp2089(maxamps), jamp2089(0:maxamps)
      common/to_amps_089/amp2089,jamp2089
      Double Precision amp2090(maxamps), jamp2090(0:maxamps)
      common/to_amps_090/amp2090,jamp2090
      Double Precision amp2091(maxamps), jamp2091(0:maxamps)
      common/to_amps_091/amp2091,jamp2091
      Double Precision amp2092(maxamps), jamp2092(0:maxamps)
      common/to_amps_092/amp2092,jamp2092
      Double Precision amp2093(maxamps), jamp2093(0:maxamps)
      common/to_amps_093/amp2093,jamp2093
      Double Precision amp2094(maxamps), jamp2094(0:maxamps)
      common/to_amps_094/amp2094,jamp2094
      Double Precision amp2095(maxamps), jamp2095(0:maxamps)
      common/to_amps_095/amp2095,jamp2095
      Double Precision amp2096(maxamps), jamp2096(0:maxamps)
      common/to_amps_096/amp2096,jamp2096
      Double Precision amp2097(maxamps), jamp2097(0:maxamps)
      common/to_amps_097/amp2097,jamp2097
      Double Precision amp2098(maxamps), jamp2098(0:maxamps)
      common/to_amps_098/amp2098,jamp2098
      Double Precision amp2099(maxamps), jamp2099(0:maxamps)
      common/to_amps_099/amp2099,jamp2099
      Double Precision amp2100(maxamps), jamp2100(0:maxamps)
      common/to_amps_100/amp2100,jamp2100
      Double Precision amp2101(maxamps), jamp2101(0:maxamps)
      common/to_amps_101/amp2101,jamp2101
      Double Precision amp2102(maxamps), jamp2102(0:maxamps)
      common/to_amps_102/amp2102,jamp2102
      Double Precision amp2103(maxamps), jamp2103(0:maxamps)
      common/to_amps_103/amp2103,jamp2103
      Double Precision amp2104(maxamps), jamp2104(0:maxamps)
      common/to_amps_104/amp2104,jamp2104
      Double Precision amp2105(maxamps), jamp2105(0:maxamps)
      common/to_amps_105/amp2105,jamp2105
      Double Precision amp2106(maxamps), jamp2106(0:maxamps)
      common/to_amps_106/amp2106,jamp2106
      Double Precision amp2107(maxamps), jamp2107(0:maxamps)
      common/to_amps_107/amp2107,jamp2107
      Double Precision amp2108(maxamps), jamp2108(0:maxamps)
      common/to_amps_108/amp2108,jamp2108
      Double Precision amp2109(maxamps), jamp2109(0:maxamps)
      common/to_amps_109/amp2109,jamp2109
      Double Precision amp2110(maxamps), jamp2110(0:maxamps)
      common/to_amps_110/amp2110,jamp2110
      Double Precision amp2111(maxamps), jamp2111(0:maxamps)
      common/to_amps_111/amp2111,jamp2111
      Double Precision amp2112(maxamps), jamp2112(0:maxamps)
      common/to_amps_112/amp2112,jamp2112
      Double Precision amp2113(maxamps), jamp2113(0:maxamps)
      common/to_amps_113/amp2113,jamp2113
      Double Precision amp2114(maxamps), jamp2114(0:maxamps)
      common/to_amps_114/amp2114,jamp2114
      Double Precision amp2115(maxamps), jamp2115(0:maxamps)
      common/to_amps_115/amp2115,jamp2115
      Double Precision amp2116(maxamps), jamp2116(0:maxamps)
      common/to_amps_116/amp2116,jamp2116
      Double Precision amp2117(maxamps), jamp2117(0:maxamps)
      common/to_amps_117/amp2117,jamp2117
      Double Precision amp2118(maxamps), jamp2118(0:maxamps)
      common/to_amps_118/amp2118,jamp2118
      Double Precision amp2119(maxamps), jamp2119(0:maxamps)
      common/to_amps_119/amp2119,jamp2119
      Double Precision amp2120(maxamps), jamp2120(0:maxamps)
      common/to_amps_120/amp2120,jamp2120
      Double Precision amp2121(maxamps), jamp2121(0:maxamps)
      common/to_amps_121/amp2121,jamp2121
      Double Precision amp2122(maxamps), jamp2122(0:maxamps)
      common/to_amps_122/amp2122,jamp2122
      Double Precision amp2123(maxamps), jamp2123(0:maxamps)
      common/to_amps_123/amp2123,jamp2123
      Double Precision amp2124(maxamps), jamp2124(0:maxamps)
      common/to_amps_124/amp2124,jamp2124
      Double Precision amp2125(maxamps), jamp2125(0:maxamps)
      common/to_amps_125/amp2125,jamp2125
      Double Precision amp2126(maxamps), jamp2126(0:maxamps)
      common/to_amps_126/amp2126,jamp2126
      Double Precision amp2127(maxamps), jamp2127(0:maxamps)
      common/to_amps_127/amp2127,jamp2127
      Double Precision amp2128(maxamps), jamp2128(0:maxamps)
      common/to_amps_128/amp2128,jamp2128
      Double Precision amp2129(maxamps), jamp2129(0:maxamps)
      common/to_amps_129/amp2129,jamp2129
      Double Precision amp2130(maxamps), jamp2130(0:maxamps)
      common/to_amps_130/amp2130,jamp2130
      Double Precision amp2131(maxamps), jamp2131(0:maxamps)
      common/to_amps_131/amp2131,jamp2131
      Double Precision amp2132(maxamps), jamp2132(0:maxamps)
      common/to_amps_132/amp2132,jamp2132
      Double Precision amp2133(maxamps), jamp2133(0:maxamps)
      common/to_amps_133/amp2133,jamp2133
      Double Precision amp2134(maxamps), jamp2134(0:maxamps)
      common/to_amps_134/amp2134,jamp2134
      Double Precision amp2135(maxamps), jamp2135(0:maxamps)
      common/to_amps_135/amp2135,jamp2135
      Double Precision amp2136(maxamps), jamp2136(0:maxamps)
      common/to_amps_136/amp2136,jamp2136
      Double Precision amp2137(maxamps), jamp2137(0:maxamps)
      common/to_amps_137/amp2137,jamp2137
      Double Precision amp2138(maxamps), jamp2138(0:maxamps)
      common/to_amps_138/amp2138,jamp2138
      Double Precision amp2139(maxamps), jamp2139(0:maxamps)
      common/to_amps_139/amp2139,jamp2139
      Double Precision amp2140(maxamps), jamp2140(0:maxamps)
      common/to_amps_140/amp2140,jamp2140
      Double Precision amp2141(maxamps), jamp2141(0:maxamps)
      common/to_amps_141/amp2141,jamp2141
      Double Precision amp2142(maxamps), jamp2142(0:maxamps)
      common/to_amps_142/amp2142,jamp2142
      Double Precision amp2143(maxamps), jamp2143(0:maxamps)
      common/to_amps_143/amp2143,jamp2143
      Double Precision amp2144(maxamps), jamp2144(0:maxamps)
      common/to_amps_144/amp2144,jamp2144
      Double Precision amp2145(maxamps), jamp2145(0:maxamps)
      common/to_amps_145/amp2145,jamp2145
      Double Precision amp2146(maxamps), jamp2146(0:maxamps)
      common/to_amps_146/amp2146,jamp2146
      Double Precision amp2147(maxamps), jamp2147(0:maxamps)
      common/to_amps_147/amp2147,jamp2147
      Double Precision amp2148(maxamps), jamp2148(0:maxamps)
      common/to_amps_148/amp2148,jamp2148
      Double Precision amp2149(maxamps), jamp2149(0:maxamps)
      common/to_amps_149/amp2149,jamp2149
      Double Precision amp2150(maxamps), jamp2150(0:maxamps)
      common/to_amps_150/amp2150,jamp2150
      Double Precision amp2151(maxamps), jamp2151(0:maxamps)
      common/to_amps_151/amp2151,jamp2151
      Double Precision amp2152(maxamps), jamp2152(0:maxamps)
      common/to_amps_152/amp2152,jamp2152
      Double Precision amp2153(maxamps), jamp2153(0:maxamps)
      common/to_amps_153/amp2153,jamp2153
      Double Precision amp2154(maxamps), jamp2154(0:maxamps)
      common/to_amps_154/amp2154,jamp2154
      Double Precision amp2155(maxamps), jamp2155(0:maxamps)
      common/to_amps_155/amp2155,jamp2155
      Double Precision amp2156(maxamps), jamp2156(0:maxamps)
      common/to_amps_156/amp2156,jamp2156
      Double Precision amp2157(maxamps), jamp2157(0:maxamps)
      common/to_amps_157/amp2157,jamp2157
      Double Precision amp2158(maxamps), jamp2158(0:maxamps)
      common/to_amps_158/amp2158,jamp2158
      Double Precision amp2159(maxamps), jamp2159(0:maxamps)
      common/to_amps_159/amp2159,jamp2159
      Double Precision amp2160(maxamps), jamp2160(0:maxamps)
      common/to_amps_160/amp2160,jamp2160
      Double Precision amp2161(maxamps), jamp2161(0:maxamps)
      common/to_amps_161/amp2161,jamp2161
      Double Precision amp2162(maxamps), jamp2162(0:maxamps)
      common/to_amps_162/amp2162,jamp2162
      Double Precision amp2163(maxamps), jamp2163(0:maxamps)
      common/to_amps_163/amp2163,jamp2163
      Double Precision amp2164(maxamps), jamp2164(0:maxamps)
      common/to_amps_164/amp2164,jamp2164
      Double Precision amp2165(maxamps), jamp2165(0:maxamps)
      common/to_amps_165/amp2165,jamp2165
      Double Precision amp2166(maxamps), jamp2166(0:maxamps)
      common/to_amps_166/amp2166,jamp2166
      Double Precision amp2167(maxamps), jamp2167(0:maxamps)
      common/to_amps_167/amp2167,jamp2167
      Double Precision amp2168(maxamps), jamp2168(0:maxamps)
      common/to_amps_168/amp2168,jamp2168
      Double Precision amp2169(maxamps), jamp2169(0:maxamps)
      common/to_amps_169/amp2169,jamp2169
      Double Precision amp2170(maxamps), jamp2170(0:maxamps)
      common/to_amps_170/amp2170,jamp2170
      Double Precision amp2171(maxamps), jamp2171(0:maxamps)
      common/to_amps_171/amp2171,jamp2171
      Double Precision amp2172(maxamps), jamp2172(0:maxamps)
      common/to_amps_172/amp2172,jamp2172
      Double Precision amp2173(maxamps), jamp2173(0:maxamps)
      common/to_amps_173/amp2173,jamp2173
      Double Precision amp2174(maxamps), jamp2174(0:maxamps)
      common/to_amps_174/amp2174,jamp2174
      Double Precision amp2175(maxamps), jamp2175(0:maxamps)
      common/to_amps_175/amp2175,jamp2175
      Double Precision amp2176(maxamps), jamp2176(0:maxamps)
      common/to_amps_176/amp2176,jamp2176
      Double Precision amp2177(maxamps), jamp2177(0:maxamps)
      common/to_amps_177/amp2177,jamp2177
      Double Precision amp2178(maxamps), jamp2178(0:maxamps)
      common/to_amps_178/amp2178,jamp2178
      Double Precision amp2179(maxamps), jamp2179(0:maxamps)
      common/to_amps_179/amp2179,jamp2179
      Double Precision amp2180(maxamps), jamp2180(0:maxamps)
      common/to_amps_180/amp2180,jamp2180
      Double Precision amp2181(maxamps), jamp2181(0:maxamps)
      common/to_amps_181/amp2181,jamp2181
      Double Precision amp2182(maxamps), jamp2182(0:maxamps)
      common/to_amps_182/amp2182,jamp2182
      Double Precision amp2183(maxamps), jamp2183(0:maxamps)
      common/to_amps_183/amp2183,jamp2183
      Double Precision amp2184(maxamps), jamp2184(0:maxamps)
      common/to_amps_184/amp2184,jamp2184
      Double Precision amp2185(maxamps), jamp2185(0:maxamps)
      common/to_amps_185/amp2185,jamp2185
      Double Precision amp2186(maxamps), jamp2186(0:maxamps)
      common/to_amps_186/amp2186,jamp2186
      Double Precision amp2187(maxamps), jamp2187(0:maxamps)
      common/to_amps_187/amp2187,jamp2187
      Double Precision amp2188(maxamps), jamp2188(0:maxamps)
      common/to_amps_188/amp2188,jamp2188
      Double Precision amp2189(maxamps), jamp2189(0:maxamps)
      common/to_amps_189/amp2189,jamp2189
      Double Precision amp2190(maxamps), jamp2190(0:maxamps)
      common/to_amps_190/amp2190,jamp2190
      Double Precision amp2191(maxamps), jamp2191(0:maxamps)
      common/to_amps_191/amp2191,jamp2191
      Double Precision amp2192(maxamps), jamp2192(0:maxamps)
      common/to_amps_192/amp2192,jamp2192
      Double Precision amp2193(maxamps), jamp2193(0:maxamps)
      common/to_amps_193/amp2193,jamp2193
      Double Precision amp2194(maxamps), jamp2194(0:maxamps)
      common/to_amps_194/amp2194,jamp2194
      Double Precision amp2195(maxamps), jamp2195(0:maxamps)
      common/to_amps_195/amp2195,jamp2195
      Double Precision amp2196(maxamps), jamp2196(0:maxamps)
      common/to_amps_196/amp2196,jamp2196
      Double Precision amp2197(maxamps), jamp2197(0:maxamps)
      common/to_amps_197/amp2197,jamp2197
      Double Precision amp2198(maxamps), jamp2198(0:maxamps)
      common/to_amps_198/amp2198,jamp2198
      Double Precision amp2199(maxamps), jamp2199(0:maxamps)
      common/to_amps_199/amp2199,jamp2199
      Double Precision amp2200(maxamps), jamp2200(0:maxamps)
      common/to_amps_200/amp2200,jamp2200
      Double Precision amp2201(maxamps), jamp2201(0:maxamps)
      common/to_amps_201/amp2201,jamp2201
      Double Precision amp2202(maxamps), jamp2202(0:maxamps)
      common/to_amps_202/amp2202,jamp2202
      Double Precision amp2203(maxamps), jamp2203(0:maxamps)
      common/to_amps_203/amp2203,jamp2203
      Double Precision amp2204(maxamps), jamp2204(0:maxamps)
      common/to_amps_204/amp2204,jamp2204
      Double Precision amp2205(maxamps), jamp2205(0:maxamps)
      common/to_amps_205/amp2205,jamp2205
      Double Precision amp2206(maxamps), jamp2206(0:maxamps)
      common/to_amps_206/amp2206,jamp2206
      Double Precision amp2207(maxamps), jamp2207(0:maxamps)
      common/to_amps_207/amp2207,jamp2207
      Double Precision amp2208(maxamps), jamp2208(0:maxamps)
      common/to_amps_208/amp2208,jamp2208
      Double Precision amp2209(maxamps), jamp2209(0:maxamps)
      common/to_amps_209/amp2209,jamp2209
      Double Precision amp2210(maxamps), jamp2210(0:maxamps)
      common/to_amps_210/amp2210,jamp2210
      Double Precision amp2211(maxamps), jamp2211(0:maxamps)
      common/to_amps_211/amp2211,jamp2211
      Double Precision amp2212(maxamps), jamp2212(0:maxamps)
      common/to_amps_212/amp2212,jamp2212
      Double Precision amp2213(maxamps), jamp2213(0:maxamps)
      common/to_amps_213/amp2213,jamp2213
      Double Precision amp2214(maxamps), jamp2214(0:maxamps)
      common/to_amps_214/amp2214,jamp2214
      Double Precision amp2215(maxamps), jamp2215(0:maxamps)
      common/to_amps_215/amp2215,jamp2215
      Double Precision amp2216(maxamps), jamp2216(0:maxamps)
      common/to_amps_216/amp2216,jamp2216
      Double Precision amp2217(maxamps), jamp2217(0:maxamps)
      common/to_amps_217/amp2217,jamp2217
      Double Precision amp2218(maxamps), jamp2218(0:maxamps)
      common/to_amps_218/amp2218,jamp2218
      Double Precision amp2219(maxamps), jamp2219(0:maxamps)
      common/to_amps_219/amp2219,jamp2219
      Double Precision amp2220(maxamps), jamp2220(0:maxamps)
      common/to_amps_220/amp2220,jamp2220
      Double Precision amp2221(maxamps), jamp2221(0:maxamps)
      common/to_amps_221/amp2221,jamp2221
      Double Precision amp2222(maxamps), jamp2222(0:maxamps)
      common/to_amps_222/amp2222,jamp2222
      Double Precision amp2223(maxamps), jamp2223(0:maxamps)
      common/to_amps_223/amp2223,jamp2223
      Double Precision amp2224(maxamps), jamp2224(0:maxamps)
      common/to_amps_224/amp2224,jamp2224
      Double Precision amp2225(maxamps), jamp2225(0:maxamps)
      common/to_amps_225/amp2225,jamp2225
      Double Precision amp2226(maxamps), jamp2226(0:maxamps)
      common/to_amps_226/amp2226,jamp2226
      Double Precision amp2227(maxamps), jamp2227(0:maxamps)
      common/to_amps_227/amp2227,jamp2227
      Double Precision amp2228(maxamps), jamp2228(0:maxamps)
      common/to_amps_228/amp2228,jamp2228
      Double Precision amp2229(maxamps), jamp2229(0:maxamps)
      common/to_amps_229/amp2229,jamp2229
      Double Precision amp2230(maxamps), jamp2230(0:maxamps)
      common/to_amps_230/amp2230,jamp2230
      Double Precision amp2231(maxamps), jamp2231(0:maxamps)
      common/to_amps_231/amp2231,jamp2231
      Double Precision amp2232(maxamps), jamp2232(0:maxamps)
      common/to_amps_232/amp2232,jamp2232
      Double Precision amp2233(maxamps), jamp2233(0:maxamps)
      common/to_amps_233/amp2233,jamp2233
      Double Precision amp2234(maxamps), jamp2234(0:maxamps)
      common/to_amps_234/amp2234,jamp2234
      Double Precision amp2235(maxamps), jamp2235(0:maxamps)
      common/to_amps_235/amp2235,jamp2235
      Double Precision amp2236(maxamps), jamp2236(0:maxamps)
      common/to_amps_236/amp2236,jamp2236
      Double Precision amp2237(maxamps), jamp2237(0:maxamps)
      common/to_amps_237/amp2237,jamp2237
      Double Precision amp2238(maxamps), jamp2238(0:maxamps)
      common/to_amps_238/amp2238,jamp2238
      Double Precision amp2239(maxamps), jamp2239(0:maxamps)
      common/to_amps_239/amp2239,jamp2239
      Double Precision amp2240(maxamps), jamp2240(0:maxamps)
      common/to_amps_240/amp2240,jamp2240
      Double Precision amp2241(maxamps), jamp2241(0:maxamps)
      common/to_amps_241/amp2241,jamp2241
      Double Precision amp2242(maxamps), jamp2242(0:maxamps)
      common/to_amps_242/amp2242,jamp2242
      Double Precision amp2243(maxamps), jamp2243(0:maxamps)
      common/to_amps_243/amp2243,jamp2243
      Double Precision amp2244(maxamps), jamp2244(0:maxamps)
      common/to_amps_244/amp2244,jamp2244
      Double Precision amp2245(maxamps), jamp2245(0:maxamps)
      common/to_amps_245/amp2245,jamp2245
      Double Precision amp2246(maxamps), jamp2246(0:maxamps)
      common/to_amps_246/amp2246,jamp2246
      Double Precision amp2247(maxamps), jamp2247(0:maxamps)
      common/to_amps_247/amp2247,jamp2247
      Double Precision amp2248(maxamps), jamp2248(0:maxamps)
      common/to_amps_248/amp2248,jamp2248
      Double Precision amp2249(maxamps), jamp2249(0:maxamps)
      common/to_amps_249/amp2249,jamp2249
      Double Precision amp2250(maxamps), jamp2250(0:maxamps)
      common/to_amps_250/amp2250,jamp2250
      Double Precision amp2251(maxamps), jamp2251(0:maxamps)
      common/to_amps_251/amp2251,jamp2251
      Double Precision amp2252(maxamps), jamp2252(0:maxamps)
      common/to_amps_252/amp2252,jamp2252
      Double Precision amp2253(maxamps), jamp2253(0:maxamps)
      common/to_amps_253/amp2253,jamp2253
      Double Precision amp2254(maxamps), jamp2254(0:maxamps)
      common/to_amps_254/amp2254,jamp2254
      Double Precision amp2255(maxamps), jamp2255(0:maxamps)
      common/to_amps_255/amp2255,jamp2255
      Double Precision amp2256(maxamps), jamp2256(0:maxamps)
      common/to_amps_256/amp2256,jamp2256
      Double Precision amp2257(maxamps), jamp2257(0:maxamps)
      common/to_amps_257/amp2257,jamp2257
      Double Precision amp2258(maxamps), jamp2258(0:maxamps)
      common/to_amps_258/amp2258,jamp2258
      Double Precision amp2259(maxamps), jamp2259(0:maxamps)
      common/to_amps_259/amp2259,jamp2259
      Double Precision amp2260(maxamps), jamp2260(0:maxamps)
      common/to_amps_260/amp2260,jamp2260
      Double Precision amp2261(maxamps), jamp2261(0:maxamps)
      common/to_amps_261/amp2261,jamp2261
      Double Precision amp2262(maxamps), jamp2262(0:maxamps)
      common/to_amps_262/amp2262,jamp2262
      Double Precision amp2263(maxamps), jamp2263(0:maxamps)
      common/to_amps_263/amp2263,jamp2263
      Double Precision amp2264(maxamps), jamp2264(0:maxamps)
      common/to_amps_264/amp2264,jamp2264
      Double Precision amp2265(maxamps), jamp2265(0:maxamps)
      common/to_amps_265/amp2265,jamp2265
      Double Precision amp2266(maxamps), jamp2266(0:maxamps)
      common/to_amps_266/amp2266,jamp2266
      Double Precision amp2267(maxamps), jamp2267(0:maxamps)
      common/to_amps_267/amp2267,jamp2267
      Double Precision amp2268(maxamps), jamp2268(0:maxamps)
      common/to_amps_268/amp2268,jamp2268
      Double Precision amp2269(maxamps), jamp2269(0:maxamps)
      common/to_amps_269/amp2269,jamp2269
      Double Precision amp2270(maxamps), jamp2270(0:maxamps)
      common/to_amps_270/amp2270,jamp2270
      Double Precision amp2271(maxamps), jamp2271(0:maxamps)
      common/to_amps_271/amp2271,jamp2271
      Double Precision amp2272(maxamps), jamp2272(0:maxamps)
      common/to_amps_272/amp2272,jamp2272
      Double Precision amp2273(maxamps), jamp2273(0:maxamps)
      common/to_amps_273/amp2273,jamp2273
      Double Precision amp2274(maxamps), jamp2274(0:maxamps)
      common/to_amps_274/amp2274,jamp2274
      Double Precision amp2275(maxamps), jamp2275(0:maxamps)
      common/to_amps_275/amp2275,jamp2275
      Double Precision amp2276(maxamps), jamp2276(0:maxamps)
      common/to_amps_276/amp2276,jamp2276
      double precision jamp2cum(0:maxamps)
      integer ICOLUP(2,nexternal-1,maxamps)
      integer color(2,nexternal-1),color1(2,nexternal-1)
      double precision random,xtarget
      external random
      integer legs(nexternal-1),lstr,i,j
      character*140 str
      integer ic(nexternal-1),legs1(nexternal-1)
      integer iflow,ifl
      logical mtc,even
      
      do i=1,nexternal-1
         ic(i)=i
      enddo
      mtc=.false.
 10   call nexper(nexternal-1- 2,ic( 2+1),mtc,even)
      do i= 2+1,nexternal-1
         ic(i)=ic(i)+ 2
      enddo
      CALL SWITCHLEGS(legs,legs1,IC,NEXTERNAL-1)
      
      call convert_to_string(nexternal-1,legs1,str,lstr)
      
      if (str.eq."-1-1-1-10") then
         include "leshouches_001.inc"
         iflow=nint(jamp2001(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2001(i)
         enddo
         goto 20
      elseif (str.eq."-111-10") then
         include "leshouches_002.inc"
         iflow=nint(jamp2002(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2002(i)
         enddo
         goto 20
      elseif (str.eq."-112-20") then
         include "leshouches_003.inc"
         iflow=nint(jamp2003(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2003(i)
         enddo
         goto 20
      elseif (str.eq."-114-40") then
         include "leshouches_004.inc"
         iflow=nint(jamp2004(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2004(i)
         enddo
         goto 20
      elseif (str.eq."-113-30") then
         include "leshouches_005.inc"
         iflow=nint(jamp2005(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2005(i)
         enddo
         goto 20
      elseif (str.eq."-115-50") then
         include "leshouches_006.inc"
         iflow=nint(jamp2006(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2006(i)
         enddo
         goto 20
      elseif (str.eq."-11000") then
         include "leshouches_007.inc"
         iflow=nint(jamp2007(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2007(i)
         enddo
         goto 20
      elseif (str.eq."-1-2-1-20") then
         include "leshouches_008.inc"
         iflow=nint(jamp2008(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2008(i)
         enddo
         goto 20
      elseif (str.eq."-12-120") then
         include "leshouches_009.inc"
         iflow=nint(jamp2009(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2009(i)
         enddo
         goto 20
      elseif (str.eq."-1-4-1-40") then
         include "leshouches_010.inc"
         iflow=nint(jamp2010(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2010(i)
         enddo
         goto 20
      elseif (str.eq."-14-140") then
         include "leshouches_011.inc"
         iflow=nint(jamp2011(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2011(i)
         enddo
         goto 20
      elseif (str.eq."-1-3-1-30") then
         include "leshouches_012.inc"
         iflow=nint(jamp2012(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2012(i)
         enddo
         goto 20
      elseif (str.eq."-13-130") then
         include "leshouches_013.inc"
         iflow=nint(jamp2013(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2013(i)
         enddo
         goto 20
      elseif (str.eq."-1-5-1-50") then
         include "leshouches_014.inc"
         iflow=nint(jamp2014(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2014(i)
         enddo
         goto 20
      elseif (str.eq."-15-150") then
         include "leshouches_015.inc"
         iflow=nint(jamp2015(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2015(i)
         enddo
         goto 20
      elseif (str.eq."-101-1-1") then
         include "leshouches_016.inc"
         iflow=nint(jamp2016(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2016(i)
         enddo
         goto 20
      elseif (str.eq."-10-12-2") then
         include "leshouches_017.inc"
         iflow=nint(jamp2017(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2017(i)
         enddo
         goto 20
      elseif (str.eq."-10-14-4") then
         include "leshouches_018.inc"
         iflow=nint(jamp2018(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2018(i)
         enddo
         goto 20
      elseif (str.eq."-10-13-3") then
         include "leshouches_019.inc"
         iflow=nint(jamp2019(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2019(i)
         enddo
         goto 20
      elseif (str.eq."-10-15-5") then
         include "leshouches_020.inc"
         iflow=nint(jamp2020(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2020(i)
         enddo
         goto 20
      elseif (str.eq."-10-100") then
         include "leshouches_021.inc"
         iflow=nint(jamp2021(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2021(i)
         enddo
         goto 20
      elseif (str.eq."1-11-10") then
         include "leshouches_022.inc"
         iflow=nint(jamp2022(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2022(i)
         enddo
         goto 20
      elseif (str.eq."1-12-20") then
         include "leshouches_023.inc"
         iflow=nint(jamp2023(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2023(i)
         enddo
         goto 20
      elseif (str.eq."1-14-40") then
         include "leshouches_024.inc"
         iflow=nint(jamp2024(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2024(i)
         enddo
         goto 20
      elseif (str.eq."1-13-30") then
         include "leshouches_025.inc"
         iflow=nint(jamp2025(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2025(i)
         enddo
         goto 20
      elseif (str.eq."1-15-50") then
         include "leshouches_026.inc"
         iflow=nint(jamp2026(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2026(i)
         enddo
         goto 20
      elseif (str.eq."1-1000") then
         include "leshouches_027.inc"
         iflow=nint(jamp2027(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2027(i)
         enddo
         goto 20
      elseif (str.eq."11110") then
         include "leshouches_028.inc"
         iflow=nint(jamp2028(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2028(i)
         enddo
         goto 20
      elseif (str.eq."1-21-20") then
         include "leshouches_029.inc"
         iflow=nint(jamp2029(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2029(i)
         enddo
         goto 20
      elseif (str.eq."12120") then
         include "leshouches_030.inc"
         iflow=nint(jamp2030(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2030(i)
         enddo
         goto 20
      elseif (str.eq."1-41-40") then
         include "leshouches_031.inc"
         iflow=nint(jamp2031(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2031(i)
         enddo
         goto 20
      elseif (str.eq."14140") then
         include "leshouches_032.inc"
         iflow=nint(jamp2032(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2032(i)
         enddo
         goto 20
      elseif (str.eq."1-31-30") then
         include "leshouches_033.inc"
         iflow=nint(jamp2033(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2033(i)
         enddo
         goto 20
      elseif (str.eq."13130") then
         include "leshouches_034.inc"
         iflow=nint(jamp2034(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2034(i)
         enddo
         goto 20
      elseif (str.eq."1-51-50") then
         include "leshouches_035.inc"
         iflow=nint(jamp2035(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2035(i)
         enddo
         goto 20
      elseif (str.eq."15150") then
         include "leshouches_036.inc"
         iflow=nint(jamp2036(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2036(i)
         enddo
         goto 20
      elseif (str.eq."1011-1") then
         include "leshouches_037.inc"
         iflow=nint(jamp2037(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2037(i)
         enddo
         goto 20
      elseif (str.eq."1012-2") then
         include "leshouches_038.inc"
         iflow=nint(jamp2038(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2038(i)
         enddo
         goto 20
      elseif (str.eq."1014-4") then
         include "leshouches_039.inc"
         iflow=nint(jamp2039(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2039(i)
         enddo
         goto 20
      elseif (str.eq."1013-3") then
         include "leshouches_040.inc"
         iflow=nint(jamp2040(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2040(i)
         enddo
         goto 20
      elseif (str.eq."1015-5") then
         include "leshouches_041.inc"
         iflow=nint(jamp2041(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2041(i)
         enddo
         goto 20
      elseif (str.eq."10100") then
         include "leshouches_042.inc"
         iflow=nint(jamp2042(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2042(i)
         enddo
         goto 20
      elseif (str.eq."-2-1-1-20") then
         include "leshouches_043.inc"
         iflow=nint(jamp2043(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2043(i)
         enddo
         goto 20
      elseif (str.eq."-211-20") then
         include "leshouches_044.inc"
         iflow=nint(jamp2044(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2044(i)
         enddo
         goto 20
      elseif (str.eq."-2-2-2-20") then
         include "leshouches_045.inc"
         iflow=nint(jamp2045(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2045(i)
         enddo
         goto 20
      elseif (str.eq."-221-10") then
         include "leshouches_046.inc"
         iflow=nint(jamp2046(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2046(i)
         enddo
         goto 20
      elseif (str.eq."-222-20") then
         include "leshouches_047.inc"
         iflow=nint(jamp2047(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2047(i)
         enddo
         goto 20
      elseif (str.eq."-224-40") then
         include "leshouches_048.inc"
         iflow=nint(jamp2048(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2048(i)
         enddo
         goto 20
      elseif (str.eq."-223-30") then
         include "leshouches_049.inc"
         iflow=nint(jamp2049(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2049(i)
         enddo
         goto 20
      elseif (str.eq."-225-50") then
         include "leshouches_050.inc"
         iflow=nint(jamp2050(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2050(i)
         enddo
         goto 20
      elseif (str.eq."-22000") then
         include "leshouches_051.inc"
         iflow=nint(jamp2051(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2051(i)
         enddo
         goto 20
      elseif (str.eq."-2-4-2-40") then
         include "leshouches_052.inc"
         iflow=nint(jamp2052(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2052(i)
         enddo
         goto 20
      elseif (str.eq."-24-240") then
         include "leshouches_053.inc"
         iflow=nint(jamp2053(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2053(i)
         enddo
         goto 20
      elseif (str.eq."-2-3-2-30") then
         include "leshouches_054.inc"
         iflow=nint(jamp2054(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2054(i)
         enddo
         goto 20
      elseif (str.eq."-23-230") then
         include "leshouches_055.inc"
         iflow=nint(jamp2055(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2055(i)
         enddo
         goto 20
      elseif (str.eq."-2-5-2-50") then
         include "leshouches_056.inc"
         iflow=nint(jamp2056(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2056(i)
         enddo
         goto 20
      elseif (str.eq."-25-250") then
         include "leshouches_057.inc"
         iflow=nint(jamp2057(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2057(i)
         enddo
         goto 20
      elseif (str.eq."-201-1-2") then
         include "leshouches_058.inc"
         iflow=nint(jamp2058(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2058(i)
         enddo
         goto 20
      elseif (str.eq."-202-2-2") then
         include "leshouches_059.inc"
         iflow=nint(jamp2059(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2059(i)
         enddo
         goto 20
      elseif (str.eq."-20-24-4") then
         include "leshouches_060.inc"
         iflow=nint(jamp2060(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2060(i)
         enddo
         goto 20
      elseif (str.eq."-20-23-3") then
         include "leshouches_061.inc"
         iflow=nint(jamp2061(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2061(i)
         enddo
         goto 20
      elseif (str.eq."-20-25-5") then
         include "leshouches_062.inc"
         iflow=nint(jamp2062(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2062(i)
         enddo
         goto 20
      elseif (str.eq."-20-200") then
         include "leshouches_063.inc"
         iflow=nint(jamp2063(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2063(i)
         enddo
         goto 20
      elseif (str.eq."2-1-120") then
         include "leshouches_064.inc"
         iflow=nint(jamp2064(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2064(i)
         enddo
         goto 20
      elseif (str.eq."21120") then
         include "leshouches_065.inc"
         iflow=nint(jamp2065(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2065(i)
         enddo
         goto 20
      elseif (str.eq."2-21-10") then
         include "leshouches_066.inc"
         iflow=nint(jamp2066(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2066(i)
         enddo
         goto 20
      elseif (str.eq."2-22-20") then
         include "leshouches_067.inc"
         iflow=nint(jamp2067(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2067(i)
         enddo
         goto 20
      elseif (str.eq."2-24-40") then
         include "leshouches_068.inc"
         iflow=nint(jamp2068(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2068(i)
         enddo
         goto 20
      elseif (str.eq."2-23-30") then
         include "leshouches_069.inc"
         iflow=nint(jamp2069(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2069(i)
         enddo
         goto 20
      elseif (str.eq."2-25-50") then
         include "leshouches_070.inc"
         iflow=nint(jamp2070(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2070(i)
         enddo
         goto 20
      elseif (str.eq."2-2000") then
         include "leshouches_071.inc"
         iflow=nint(jamp2071(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2071(i)
         enddo
         goto 20
      elseif (str.eq."22220") then
         include "leshouches_072.inc"
         iflow=nint(jamp2072(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2072(i)
         enddo
         goto 20
      elseif (str.eq."2-42-40") then
         include "leshouches_073.inc"
         iflow=nint(jamp2073(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2073(i)
         enddo
         goto 20
      elseif (str.eq."24240") then
         include "leshouches_074.inc"
         iflow=nint(jamp2074(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2074(i)
         enddo
         goto 20
      elseif (str.eq."2-32-30") then
         include "leshouches_075.inc"
         iflow=nint(jamp2075(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2075(i)
         enddo
         goto 20
      elseif (str.eq."23230") then
         include "leshouches_076.inc"
         iflow=nint(jamp2076(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2076(i)
         enddo
         goto 20
      elseif (str.eq."2-52-50") then
         include "leshouches_077.inc"
         iflow=nint(jamp2077(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2077(i)
         enddo
         goto 20
      elseif (str.eq."25250") then
         include "leshouches_078.inc"
         iflow=nint(jamp2078(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2078(i)
         enddo
         goto 20
      elseif (str.eq."201-12") then
         include "leshouches_079.inc"
         iflow=nint(jamp2079(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2079(i)
         enddo
         goto 20
      elseif (str.eq."2022-2") then
         include "leshouches_080.inc"
         iflow=nint(jamp2080(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2080(i)
         enddo
         goto 20
      elseif (str.eq."2024-4") then
         include "leshouches_081.inc"
         iflow=nint(jamp2081(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2081(i)
         enddo
         goto 20
      elseif (str.eq."2023-3") then
         include "leshouches_082.inc"
         iflow=nint(jamp2082(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2082(i)
         enddo
         goto 20
      elseif (str.eq."2025-5") then
         include "leshouches_083.inc"
         iflow=nint(jamp2083(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2083(i)
         enddo
         goto 20
      elseif (str.eq."20200") then
         include "leshouches_084.inc"
         iflow=nint(jamp2084(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2084(i)
         enddo
         goto 20
      elseif (str.eq."-4-1-1-40") then
         include "leshouches_085.inc"
         iflow=nint(jamp2085(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2085(i)
         enddo
         goto 20
      elseif (str.eq."-411-40") then
         include "leshouches_086.inc"
         iflow=nint(jamp2086(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2086(i)
         enddo
         goto 20
      elseif (str.eq."-4-2-2-40") then
         include "leshouches_087.inc"
         iflow=nint(jamp2087(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2087(i)
         enddo
         goto 20
      elseif (str.eq."-422-40") then
         include "leshouches_088.inc"
         iflow=nint(jamp2088(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2088(i)
         enddo
         goto 20
      elseif (str.eq."-4-4-4-40") then
         include "leshouches_089.inc"
         iflow=nint(jamp2089(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2089(i)
         enddo
         goto 20
      elseif (str.eq."-441-10") then
         include "leshouches_090.inc"
         iflow=nint(jamp2090(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2090(i)
         enddo
         goto 20
      elseif (str.eq."-442-20") then
         include "leshouches_091.inc"
         iflow=nint(jamp2091(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2091(i)
         enddo
         goto 20
      elseif (str.eq."-444-40") then
         include "leshouches_092.inc"
         iflow=nint(jamp2092(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2092(i)
         enddo
         goto 20
      elseif (str.eq."-443-30") then
         include "leshouches_093.inc"
         iflow=nint(jamp2093(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2093(i)
         enddo
         goto 20
      elseif (str.eq."-445-50") then
         include "leshouches_094.inc"
         iflow=nint(jamp2094(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2094(i)
         enddo
         goto 20
      elseif (str.eq."-44000") then
         include "leshouches_095.inc"
         iflow=nint(jamp2095(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2095(i)
         enddo
         goto 20
      elseif (str.eq."-4-3-4-30") then
         include "leshouches_096.inc"
         iflow=nint(jamp2096(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2096(i)
         enddo
         goto 20
      elseif (str.eq."-43-430") then
         include "leshouches_097.inc"
         iflow=nint(jamp2097(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2097(i)
         enddo
         goto 20
      elseif (str.eq."-4-5-4-50") then
         include "leshouches_098.inc"
         iflow=nint(jamp2098(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2098(i)
         enddo
         goto 20
      elseif (str.eq."-45-450") then
         include "leshouches_099.inc"
         iflow=nint(jamp2099(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2099(i)
         enddo
         goto 20
      elseif (str.eq."-401-1-4") then
         include "leshouches_100.inc"
         iflow=nint(jamp2100(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2100(i)
         enddo
         goto 20
      elseif (str.eq."-402-2-4") then
         include "leshouches_101.inc"
         iflow=nint(jamp2101(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2101(i)
         enddo
         goto 20
      elseif (str.eq."-404-4-4") then
         include "leshouches_102.inc"
         iflow=nint(jamp2102(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2102(i)
         enddo
         goto 20
      elseif (str.eq."-40-43-3") then
         include "leshouches_103.inc"
         iflow=nint(jamp2103(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2103(i)
         enddo
         goto 20
      elseif (str.eq."-40-45-5") then
         include "leshouches_104.inc"
         iflow=nint(jamp2104(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2104(i)
         enddo
         goto 20
      elseif (str.eq."-40-400") then
         include "leshouches_105.inc"
         iflow=nint(jamp2105(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2105(i)
         enddo
         goto 20
      elseif (str.eq."4-1-140") then
         include "leshouches_106.inc"
         iflow=nint(jamp2106(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2106(i)
         enddo
         goto 20
      elseif (str.eq."41140") then
         include "leshouches_107.inc"
         iflow=nint(jamp2107(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2107(i)
         enddo
         goto 20
      elseif (str.eq."4-2-240") then
         include "leshouches_108.inc"
         iflow=nint(jamp2108(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2108(i)
         enddo
         goto 20
      elseif (str.eq."42240") then
         include "leshouches_109.inc"
         iflow=nint(jamp2109(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2109(i)
         enddo
         goto 20
      elseif (str.eq."4-41-10") then
         include "leshouches_110.inc"
         iflow=nint(jamp2110(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2110(i)
         enddo
         goto 20
      elseif (str.eq."4-42-20") then
         include "leshouches_111.inc"
         iflow=nint(jamp2111(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2111(i)
         enddo
         goto 20
      elseif (str.eq."4-44-40") then
         include "leshouches_112.inc"
         iflow=nint(jamp2112(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2112(i)
         enddo
         goto 20
      elseif (str.eq."4-43-30") then
         include "leshouches_113.inc"
         iflow=nint(jamp2113(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2113(i)
         enddo
         goto 20
      elseif (str.eq."4-45-50") then
         include "leshouches_114.inc"
         iflow=nint(jamp2114(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2114(i)
         enddo
         goto 20
      elseif (str.eq."4-4000") then
         include "leshouches_115.inc"
         iflow=nint(jamp2115(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2115(i)
         enddo
         goto 20
      elseif (str.eq."44440") then
         include "leshouches_116.inc"
         iflow=nint(jamp2116(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2116(i)
         enddo
         goto 20
      elseif (str.eq."4-34-30") then
         include "leshouches_117.inc"
         iflow=nint(jamp2117(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2117(i)
         enddo
         goto 20
      elseif (str.eq."43430") then
         include "leshouches_118.inc"
         iflow=nint(jamp2118(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2118(i)
         enddo
         goto 20
      elseif (str.eq."4-54-50") then
         include "leshouches_119.inc"
         iflow=nint(jamp2119(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2119(i)
         enddo
         goto 20
      elseif (str.eq."45450") then
         include "leshouches_120.inc"
         iflow=nint(jamp2120(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2120(i)
         enddo
         goto 20
      elseif (str.eq."401-14") then
         include "leshouches_121.inc"
         iflow=nint(jamp2121(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2121(i)
         enddo
         goto 20
      elseif (str.eq."402-24") then
         include "leshouches_122.inc"
         iflow=nint(jamp2122(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2122(i)
         enddo
         goto 20
      elseif (str.eq."4044-4") then
         include "leshouches_123.inc"
         iflow=nint(jamp2123(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2123(i)
         enddo
         goto 20
      elseif (str.eq."4043-3") then
         include "leshouches_124.inc"
         iflow=nint(jamp2124(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2124(i)
         enddo
         goto 20
      elseif (str.eq."4045-5") then
         include "leshouches_125.inc"
         iflow=nint(jamp2125(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2125(i)
         enddo
         goto 20
      elseif (str.eq."40400") then
         include "leshouches_126.inc"
         iflow=nint(jamp2126(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2126(i)
         enddo
         goto 20
      elseif (str.eq."-3-1-1-30") then
         include "leshouches_127.inc"
         iflow=nint(jamp2127(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2127(i)
         enddo
         goto 20
      elseif (str.eq."-311-30") then
         include "leshouches_128.inc"
         iflow=nint(jamp2128(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2128(i)
         enddo
         goto 20
      elseif (str.eq."-3-2-2-30") then
         include "leshouches_129.inc"
         iflow=nint(jamp2129(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2129(i)
         enddo
         goto 20
      elseif (str.eq."-322-30") then
         include "leshouches_130.inc"
         iflow=nint(jamp2130(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2130(i)
         enddo
         goto 20
      elseif (str.eq."-3-4-4-30") then
         include "leshouches_131.inc"
         iflow=nint(jamp2131(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2131(i)
         enddo
         goto 20
      elseif (str.eq."-344-30") then
         include "leshouches_132.inc"
         iflow=nint(jamp2132(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2132(i)
         enddo
         goto 20
      elseif (str.eq."-3-3-3-30") then
         include "leshouches_133.inc"
         iflow=nint(jamp2133(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2133(i)
         enddo
         goto 20
      elseif (str.eq."-331-10") then
         include "leshouches_134.inc"
         iflow=nint(jamp2134(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2134(i)
         enddo
         goto 20
      elseif (str.eq."-332-20") then
         include "leshouches_135.inc"
         iflow=nint(jamp2135(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2135(i)
         enddo
         goto 20
      elseif (str.eq."-334-40") then
         include "leshouches_136.inc"
         iflow=nint(jamp2136(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2136(i)
         enddo
         goto 20
      elseif (str.eq."-333-30") then
         include "leshouches_137.inc"
         iflow=nint(jamp2137(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2137(i)
         enddo
         goto 20
      elseif (str.eq."-335-50") then
         include "leshouches_138.inc"
         iflow=nint(jamp2138(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2138(i)
         enddo
         goto 20
      elseif (str.eq."-33000") then
         include "leshouches_139.inc"
         iflow=nint(jamp2139(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2139(i)
         enddo
         goto 20
      elseif (str.eq."-3-5-3-50") then
         include "leshouches_140.inc"
         iflow=nint(jamp2140(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2140(i)
         enddo
         goto 20
      elseif (str.eq."-35-350") then
         include "leshouches_141.inc"
         iflow=nint(jamp2141(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2141(i)
         enddo
         goto 20
      elseif (str.eq."-301-1-3") then
         include "leshouches_142.inc"
         iflow=nint(jamp2142(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2142(i)
         enddo
         goto 20
      elseif (str.eq."-302-2-3") then
         include "leshouches_143.inc"
         iflow=nint(jamp2143(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2143(i)
         enddo
         goto 20
      elseif (str.eq."-304-4-3") then
         include "leshouches_144.inc"
         iflow=nint(jamp2144(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2144(i)
         enddo
         goto 20
      elseif (str.eq."-303-3-3") then
         include "leshouches_145.inc"
         iflow=nint(jamp2145(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2145(i)
         enddo
         goto 20
      elseif (str.eq."-30-35-5") then
         include "leshouches_146.inc"
         iflow=nint(jamp2146(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2146(i)
         enddo
         goto 20
      elseif (str.eq."-30-300") then
         include "leshouches_147.inc"
         iflow=nint(jamp2147(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2147(i)
         enddo
         goto 20
      elseif (str.eq."3-1-130") then
         include "leshouches_148.inc"
         iflow=nint(jamp2148(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2148(i)
         enddo
         goto 20
      elseif (str.eq."31130") then
         include "leshouches_149.inc"
         iflow=nint(jamp2149(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2149(i)
         enddo
         goto 20
      elseif (str.eq."3-2-230") then
         include "leshouches_150.inc"
         iflow=nint(jamp2150(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2150(i)
         enddo
         goto 20
      elseif (str.eq."32230") then
         include "leshouches_151.inc"
         iflow=nint(jamp2151(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2151(i)
         enddo
         goto 20
      elseif (str.eq."3-4-430") then
         include "leshouches_152.inc"
         iflow=nint(jamp2152(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2152(i)
         enddo
         goto 20
      elseif (str.eq."34430") then
         include "leshouches_153.inc"
         iflow=nint(jamp2153(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2153(i)
         enddo
         goto 20
      elseif (str.eq."3-31-10") then
         include "leshouches_154.inc"
         iflow=nint(jamp2154(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2154(i)
         enddo
         goto 20
      elseif (str.eq."3-32-20") then
         include "leshouches_155.inc"
         iflow=nint(jamp2155(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2155(i)
         enddo
         goto 20
      elseif (str.eq."3-34-40") then
         include "leshouches_156.inc"
         iflow=nint(jamp2156(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2156(i)
         enddo
         goto 20
      elseif (str.eq."3-33-30") then
         include "leshouches_157.inc"
         iflow=nint(jamp2157(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2157(i)
         enddo
         goto 20
      elseif (str.eq."3-35-50") then
         include "leshouches_158.inc"
         iflow=nint(jamp2158(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2158(i)
         enddo
         goto 20
      elseif (str.eq."3-3000") then
         include "leshouches_159.inc"
         iflow=nint(jamp2159(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2159(i)
         enddo
         goto 20
      elseif (str.eq."33330") then
         include "leshouches_160.inc"
         iflow=nint(jamp2160(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2160(i)
         enddo
         goto 20
      elseif (str.eq."3-53-50") then
         include "leshouches_161.inc"
         iflow=nint(jamp2161(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2161(i)
         enddo
         goto 20
      elseif (str.eq."35350") then
         include "leshouches_162.inc"
         iflow=nint(jamp2162(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2162(i)
         enddo
         goto 20
      elseif (str.eq."301-13") then
         include "leshouches_163.inc"
         iflow=nint(jamp2163(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2163(i)
         enddo
         goto 20
      elseif (str.eq."302-23") then
         include "leshouches_164.inc"
         iflow=nint(jamp2164(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2164(i)
         enddo
         goto 20
      elseif (str.eq."304-43") then
         include "leshouches_165.inc"
         iflow=nint(jamp2165(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2165(i)
         enddo
         goto 20
      elseif (str.eq."3033-3") then
         include "leshouches_166.inc"
         iflow=nint(jamp2166(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2166(i)
         enddo
         goto 20
      elseif (str.eq."3035-5") then
         include "leshouches_167.inc"
         iflow=nint(jamp2167(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2167(i)
         enddo
         goto 20
      elseif (str.eq."30300") then
         include "leshouches_168.inc"
         iflow=nint(jamp2168(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2168(i)
         enddo
         goto 20
      elseif (str.eq."-5-1-1-50") then
         include "leshouches_169.inc"
         iflow=nint(jamp2169(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2169(i)
         enddo
         goto 20
      elseif (str.eq."-511-50") then
         include "leshouches_170.inc"
         iflow=nint(jamp2170(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2170(i)
         enddo
         goto 20
      elseif (str.eq."-5-2-2-50") then
         include "leshouches_171.inc"
         iflow=nint(jamp2171(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2171(i)
         enddo
         goto 20
      elseif (str.eq."-522-50") then
         include "leshouches_172.inc"
         iflow=nint(jamp2172(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2172(i)
         enddo
         goto 20
      elseif (str.eq."-5-4-4-50") then
         include "leshouches_173.inc"
         iflow=nint(jamp2173(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2173(i)
         enddo
         goto 20
      elseif (str.eq."-544-50") then
         include "leshouches_174.inc"
         iflow=nint(jamp2174(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2174(i)
         enddo
         goto 20
      elseif (str.eq."-5-3-3-50") then
         include "leshouches_175.inc"
         iflow=nint(jamp2175(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2175(i)
         enddo
         goto 20
      elseif (str.eq."-533-50") then
         include "leshouches_176.inc"
         iflow=nint(jamp2176(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2176(i)
         enddo
         goto 20
      elseif (str.eq."-5-5-5-50") then
         include "leshouches_177.inc"
         iflow=nint(jamp2177(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2177(i)
         enddo
         goto 20
      elseif (str.eq."-551-10") then
         include "leshouches_178.inc"
         iflow=nint(jamp2178(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2178(i)
         enddo
         goto 20
      elseif (str.eq."-552-20") then
         include "leshouches_179.inc"
         iflow=nint(jamp2179(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2179(i)
         enddo
         goto 20
      elseif (str.eq."-554-40") then
         include "leshouches_180.inc"
         iflow=nint(jamp2180(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2180(i)
         enddo
         goto 20
      elseif (str.eq."-553-30") then
         include "leshouches_181.inc"
         iflow=nint(jamp2181(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2181(i)
         enddo
         goto 20
      elseif (str.eq."-555-50") then
         include "leshouches_182.inc"
         iflow=nint(jamp2182(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2182(i)
         enddo
         goto 20
      elseif (str.eq."-55000") then
         include "leshouches_183.inc"
         iflow=nint(jamp2183(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2183(i)
         enddo
         goto 20
      elseif (str.eq."-501-1-5") then
         include "leshouches_184.inc"
         iflow=nint(jamp2184(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2184(i)
         enddo
         goto 20
      elseif (str.eq."-502-2-5") then
         include "leshouches_185.inc"
         iflow=nint(jamp2185(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2185(i)
         enddo
         goto 20
      elseif (str.eq."-504-4-5") then
         include "leshouches_186.inc"
         iflow=nint(jamp2186(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2186(i)
         enddo
         goto 20
      elseif (str.eq."-503-3-5") then
         include "leshouches_187.inc"
         iflow=nint(jamp2187(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2187(i)
         enddo
         goto 20
      elseif (str.eq."-505-5-5") then
         include "leshouches_188.inc"
         iflow=nint(jamp2188(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2188(i)
         enddo
         goto 20
      elseif (str.eq."-50-500") then
         include "leshouches_189.inc"
         iflow=nint(jamp2189(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2189(i)
         enddo
         goto 20
      elseif (str.eq."5-1-150") then
         include "leshouches_190.inc"
         iflow=nint(jamp2190(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2190(i)
         enddo
         goto 20
      elseif (str.eq."51150") then
         include "leshouches_191.inc"
         iflow=nint(jamp2191(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2191(i)
         enddo
         goto 20
      elseif (str.eq."5-2-250") then
         include "leshouches_192.inc"
         iflow=nint(jamp2192(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2192(i)
         enddo
         goto 20
      elseif (str.eq."52250") then
         include "leshouches_193.inc"
         iflow=nint(jamp2193(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2193(i)
         enddo
         goto 20
      elseif (str.eq."5-4-450") then
         include "leshouches_194.inc"
         iflow=nint(jamp2194(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2194(i)
         enddo
         goto 20
      elseif (str.eq."54450") then
         include "leshouches_195.inc"
         iflow=nint(jamp2195(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2195(i)
         enddo
         goto 20
      elseif (str.eq."5-3-350") then
         include "leshouches_196.inc"
         iflow=nint(jamp2196(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2196(i)
         enddo
         goto 20
      elseif (str.eq."53350") then
         include "leshouches_197.inc"
         iflow=nint(jamp2197(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2197(i)
         enddo
         goto 20
      elseif (str.eq."5-51-10") then
         include "leshouches_198.inc"
         iflow=nint(jamp2198(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2198(i)
         enddo
         goto 20
      elseif (str.eq."5-52-20") then
         include "leshouches_199.inc"
         iflow=nint(jamp2199(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2199(i)
         enddo
         goto 20
      elseif (str.eq."5-54-40") then
         include "leshouches_200.inc"
         iflow=nint(jamp2200(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2200(i)
         enddo
         goto 20
      elseif (str.eq."5-53-30") then
         include "leshouches_201.inc"
         iflow=nint(jamp2201(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2201(i)
         enddo
         goto 20
      elseif (str.eq."5-55-50") then
         include "leshouches_202.inc"
         iflow=nint(jamp2202(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2202(i)
         enddo
         goto 20
      elseif (str.eq."5-5000") then
         include "leshouches_203.inc"
         iflow=nint(jamp2203(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2203(i)
         enddo
         goto 20
      elseif (str.eq."55550") then
         include "leshouches_204.inc"
         iflow=nint(jamp2204(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2204(i)
         enddo
         goto 20
      elseif (str.eq."501-15") then
         include "leshouches_205.inc"
         iflow=nint(jamp2205(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2205(i)
         enddo
         goto 20
      elseif (str.eq."502-25") then
         include "leshouches_206.inc"
         iflow=nint(jamp2206(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2206(i)
         enddo
         goto 20
      elseif (str.eq."504-45") then
         include "leshouches_207.inc"
         iflow=nint(jamp2207(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2207(i)
         enddo
         goto 20
      elseif (str.eq."503-35") then
         include "leshouches_208.inc"
         iflow=nint(jamp2208(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2208(i)
         enddo
         goto 20
      elseif (str.eq."5055-5") then
         include "leshouches_209.inc"
         iflow=nint(jamp2209(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2209(i)
         enddo
         goto 20
      elseif (str.eq."50500") then
         include "leshouches_210.inc"
         iflow=nint(jamp2210(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2210(i)
         enddo
         goto 20
      elseif (str.eq."0-11-1-1") then
         include "leshouches_211.inc"
         iflow=nint(jamp2211(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2211(i)
         enddo
         goto 20
      elseif (str.eq."0-1-12-2") then
         include "leshouches_212.inc"
         iflow=nint(jamp2212(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2212(i)
         enddo
         goto 20
      elseif (str.eq."0-1-14-4") then
         include "leshouches_213.inc"
         iflow=nint(jamp2213(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2213(i)
         enddo
         goto 20
      elseif (str.eq."0-1-13-3") then
         include "leshouches_214.inc"
         iflow=nint(jamp2214(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2214(i)
         enddo
         goto 20
      elseif (str.eq."0-1-15-5") then
         include "leshouches_215.inc"
         iflow=nint(jamp2215(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2215(i)
         enddo
         goto 20
      elseif (str.eq."0-1-100") then
         include "leshouches_216.inc"
         iflow=nint(jamp2216(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2216(i)
         enddo
         goto 20
      elseif (str.eq."0111-1") then
         include "leshouches_217.inc"
         iflow=nint(jamp2217(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2217(i)
         enddo
         goto 20
      elseif (str.eq."0112-2") then
         include "leshouches_218.inc"
         iflow=nint(jamp2218(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2218(i)
         enddo
         goto 20
      elseif (str.eq."0114-4") then
         include "leshouches_219.inc"
         iflow=nint(jamp2219(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2219(i)
         enddo
         goto 20
      elseif (str.eq."0113-3") then
         include "leshouches_220.inc"
         iflow=nint(jamp2220(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2220(i)
         enddo
         goto 20
      elseif (str.eq."0115-5") then
         include "leshouches_221.inc"
         iflow=nint(jamp2221(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2221(i)
         enddo
         goto 20
      elseif (str.eq."01100") then
         include "leshouches_222.inc"
         iflow=nint(jamp2222(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2222(i)
         enddo
         goto 20
      elseif (str.eq."0-21-1-2") then
         include "leshouches_223.inc"
         iflow=nint(jamp2223(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2223(i)
         enddo
         goto 20
      elseif (str.eq."0-22-2-2") then
         include "leshouches_224.inc"
         iflow=nint(jamp2224(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2224(i)
         enddo
         goto 20
      elseif (str.eq."0-2-24-4") then
         include "leshouches_225.inc"
         iflow=nint(jamp2225(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2225(i)
         enddo
         goto 20
      elseif (str.eq."0-2-23-3") then
         include "leshouches_226.inc"
         iflow=nint(jamp2226(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2226(i)
         enddo
         goto 20
      elseif (str.eq."0-2-25-5") then
         include "leshouches_227.inc"
         iflow=nint(jamp2227(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2227(i)
         enddo
         goto 20
      elseif (str.eq."0-2-200") then
         include "leshouches_228.inc"
         iflow=nint(jamp2228(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2228(i)
         enddo
         goto 20
      elseif (str.eq."021-12") then
         include "leshouches_229.inc"
         iflow=nint(jamp2229(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2229(i)
         enddo
         goto 20
      elseif (str.eq."0222-2") then
         include "leshouches_230.inc"
         iflow=nint(jamp2230(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2230(i)
         enddo
         goto 20
      elseif (str.eq."0224-4") then
         include "leshouches_231.inc"
         iflow=nint(jamp2231(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2231(i)
         enddo
         goto 20
      elseif (str.eq."0223-3") then
         include "leshouches_232.inc"
         iflow=nint(jamp2232(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2232(i)
         enddo
         goto 20
      elseif (str.eq."0225-5") then
         include "leshouches_233.inc"
         iflow=nint(jamp2233(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2233(i)
         enddo
         goto 20
      elseif (str.eq."02200") then
         include "leshouches_234.inc"
         iflow=nint(jamp2234(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2234(i)
         enddo
         goto 20
      elseif (str.eq."0-41-1-4") then
         include "leshouches_235.inc"
         iflow=nint(jamp2235(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2235(i)
         enddo
         goto 20
      elseif (str.eq."0-42-2-4") then
         include "leshouches_236.inc"
         iflow=nint(jamp2236(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2236(i)
         enddo
         goto 20
      elseif (str.eq."0-44-4-4") then
         include "leshouches_237.inc"
         iflow=nint(jamp2237(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2237(i)
         enddo
         goto 20
      elseif (str.eq."0-4-43-3") then
         include "leshouches_238.inc"
         iflow=nint(jamp2238(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2238(i)
         enddo
         goto 20
      elseif (str.eq."0-4-45-5") then
         include "leshouches_239.inc"
         iflow=nint(jamp2239(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2239(i)
         enddo
         goto 20
      elseif (str.eq."0-4-400") then
         include "leshouches_240.inc"
         iflow=nint(jamp2240(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2240(i)
         enddo
         goto 20
      elseif (str.eq."041-14") then
         include "leshouches_241.inc"
         iflow=nint(jamp2241(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2241(i)
         enddo
         goto 20
      elseif (str.eq."042-24") then
         include "leshouches_242.inc"
         iflow=nint(jamp2242(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2242(i)
         enddo
         goto 20
      elseif (str.eq."0444-4") then
         include "leshouches_243.inc"
         iflow=nint(jamp2243(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2243(i)
         enddo
         goto 20
      elseif (str.eq."0443-3") then
         include "leshouches_244.inc"
         iflow=nint(jamp2244(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2244(i)
         enddo
         goto 20
      elseif (str.eq."0445-5") then
         include "leshouches_245.inc"
         iflow=nint(jamp2245(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2245(i)
         enddo
         goto 20
      elseif (str.eq."04400") then
         include "leshouches_246.inc"
         iflow=nint(jamp2246(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2246(i)
         enddo
         goto 20
      elseif (str.eq."0-31-1-3") then
         include "leshouches_247.inc"
         iflow=nint(jamp2247(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2247(i)
         enddo
         goto 20
      elseif (str.eq."0-32-2-3") then
         include "leshouches_248.inc"
         iflow=nint(jamp2248(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2248(i)
         enddo
         goto 20
      elseif (str.eq."0-34-4-3") then
         include "leshouches_249.inc"
         iflow=nint(jamp2249(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2249(i)
         enddo
         goto 20
      elseif (str.eq."0-33-3-3") then
         include "leshouches_250.inc"
         iflow=nint(jamp2250(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2250(i)
         enddo
         goto 20
      elseif (str.eq."0-3-35-5") then
         include "leshouches_251.inc"
         iflow=nint(jamp2251(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2251(i)
         enddo
         goto 20
      elseif (str.eq."0-3-300") then
         include "leshouches_252.inc"
         iflow=nint(jamp2252(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2252(i)
         enddo
         goto 20
      elseif (str.eq."031-13") then
         include "leshouches_253.inc"
         iflow=nint(jamp2253(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2253(i)
         enddo
         goto 20
      elseif (str.eq."032-23") then
         include "leshouches_254.inc"
         iflow=nint(jamp2254(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2254(i)
         enddo
         goto 20
      elseif (str.eq."034-43") then
         include "leshouches_255.inc"
         iflow=nint(jamp2255(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2255(i)
         enddo
         goto 20
      elseif (str.eq."0333-3") then
         include "leshouches_256.inc"
         iflow=nint(jamp2256(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2256(i)
         enddo
         goto 20
      elseif (str.eq."0335-5") then
         include "leshouches_257.inc"
         iflow=nint(jamp2257(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2257(i)
         enddo
         goto 20
      elseif (str.eq."03300") then
         include "leshouches_258.inc"
         iflow=nint(jamp2258(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2258(i)
         enddo
         goto 20
      elseif (str.eq."0-51-1-5") then
         include "leshouches_259.inc"
         iflow=nint(jamp2259(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2259(i)
         enddo
         goto 20
      elseif (str.eq."0-52-2-5") then
         include "leshouches_260.inc"
         iflow=nint(jamp2260(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2260(i)
         enddo
         goto 20
      elseif (str.eq."0-54-4-5") then
         include "leshouches_261.inc"
         iflow=nint(jamp2261(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2261(i)
         enddo
         goto 20
      elseif (str.eq."0-53-3-5") then
         include "leshouches_262.inc"
         iflow=nint(jamp2262(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2262(i)
         enddo
         goto 20
      elseif (str.eq."0-55-5-5") then
         include "leshouches_263.inc"
         iflow=nint(jamp2263(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2263(i)
         enddo
         goto 20
      elseif (str.eq."0-5-500") then
         include "leshouches_264.inc"
         iflow=nint(jamp2264(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2264(i)
         enddo
         goto 20
      elseif (str.eq."051-15") then
         include "leshouches_265.inc"
         iflow=nint(jamp2265(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2265(i)
         enddo
         goto 20
      elseif (str.eq."052-25") then
         include "leshouches_266.inc"
         iflow=nint(jamp2266(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2266(i)
         enddo
         goto 20
      elseif (str.eq."054-45") then
         include "leshouches_267.inc"
         iflow=nint(jamp2267(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2267(i)
         enddo
         goto 20
      elseif (str.eq."053-35") then
         include "leshouches_268.inc"
         iflow=nint(jamp2268(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2268(i)
         enddo
         goto 20
      elseif (str.eq."0555-5") then
         include "leshouches_269.inc"
         iflow=nint(jamp2269(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2269(i)
         enddo
         goto 20
      elseif (str.eq."05500") then
         include "leshouches_270.inc"
         iflow=nint(jamp2270(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2270(i)
         enddo
         goto 20
      elseif (str.eq."001-10") then
         include "leshouches_271.inc"
         iflow=nint(jamp2271(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2271(i)
         enddo
         goto 20
      elseif (str.eq."002-20") then
         include "leshouches_272.inc"
         iflow=nint(jamp2272(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2272(i)
         enddo
         goto 20
      elseif (str.eq."004-40") then
         include "leshouches_273.inc"
         iflow=nint(jamp2273(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2273(i)
         enddo
         goto 20
      elseif (str.eq."003-30") then
         include "leshouches_274.inc"
         iflow=nint(jamp2274(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2274(i)
         enddo
         goto 20
      elseif (str.eq."005-50") then
         include "leshouches_275.inc"
         iflow=nint(jamp2275(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2275(i)
         enddo
         goto 20
      elseif (str.eq."00000") then
         include "leshouches_276.inc"
         iflow=nint(jamp2276(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2276(i)
         enddo
         goto 20
      endif
      
      do while(mtc)
         do i= 2+1,nexternal-1
            ic(i)=ic(i)- 2
         enddo
         goto 10
      enddo
      if (.not.mtc) then
         write (*,*) "Error #1, in sborn_proc.f"
         stop
      endif
      
 20   continue
      xtarget=jamp2cum(iflow)*random()
      ifl=1
      do while (jamp2cum(ifl).lt.xtarget)
         ifl=ifl+1
      enddo
      do i=1,2
         do j=1,nexternal-1
            color1(i,j)=ICOLUP(i,j,ifl)
         enddo
      enddo
      call switchcolor(color1,color,
     &     ic,nexternal-1)
      
      end
      
      
      
      
      subroutine convert_to_string(npart,id,string,lstring)
      implicit none
      integer npart,lstring,i
      integer id(npart)
      character*140 string
      character*3 s
      
      do i=1,140
         string(i:i)=' '
      enddo
      lstring=0
      do i=1,npart
         if (id(i).eq.21) id(i)=0
         if (abs(id(i)).le.9) then
            s=char(abs(id(i))+48)
         elseif(abs(id(i)).le.99)then
            s=char(abs(id(i))/10+48)
     &           //char(mod(abs(id(i)),10)+48)
               elseif(abs(id(i)).le.999) then
                  s=char(abs(id(i))/100+48)
     &           //char((abs(id(i))-(abs(id(i))/100)*100)/10+48)
     &           //char(mod(abs(id(i))-(abs(id(i))/100)*100,10)+48)
         else
            write (*,*) 'error, particle ID is too large',abs(id(i))
         endif
         if (id(i).ge.0) then
            if (id(i).le.9) then
               string(lstring+1:lstring+1)=s
               lstring=lstring+1
            elseif (id(i).le.99) then
               string(lstring+1:lstring+2)=s
               lstring=lstring+2
            elseif (id(i).le.999) then
               string(lstring+1:lstring+3)=s
               lstring=lstring+3
            endif
         else
            if (abs(id(i)).le.9) then
               string(lstring+1:lstring+2)='-'//s
               lstring=lstring+2
            elseif (abs(id(i)).le.99) then
               string(lstring+1:lstring+3)='-'//s
               lstring=lstring+3
            elseif (abs(id(i)).le.999) then
               string(lstring+1:lstring+4)='-'//s
               lstring=lstring+4
            endif
         endif
      enddo
      end
